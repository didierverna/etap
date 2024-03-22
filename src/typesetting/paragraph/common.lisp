(in-package :etap)

;; For GUI popups.
(defgeneric properties (object)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))




;; ==========================================================================
;; Dispositions
;; ==========================================================================

(defparameter *dispositions*
  '(:flush-left :centered :flush-right :justified))

(defparameter *disposition-options* '((:overstretch t) (:overshrink t)))

(defparameter *disposition-options-help-keys*
  '(:disposition-option-overstretch :disposition-option-overshrink))

(defparameter *disposition-options-tooltips*
  '(:disposition-option-overstretch
    "In Justified disposition, stretch as needed,
ignoring the algorithm's decision."
    :disposition-option-overshrink
    "In Justified disposition, shrink as needed,
ignoring the algorithm's decision."))

(defun disposition-type (disposition)
  "Return DISPOSITION type."
  (car-or-symbol disposition))

(defun disposition-options (disposition)
  "Return DISPOSITION options."
  (cdr-or-nil disposition))




;; ==========================================================================
;; Pinned Items
;; ==========================================================================

(defclass pinned-character (pinned)
  ((character-metrics :documentation "The pinned character."
		      :initarg :character-metrics
		      :reader character-metrics))
  (:documentation "The PINNED-CHARACTER class."))

(defun pinned-character-p (object)
  "Return T if OBJECT is a pinned character."
  (typep object 'pinned-character))

(defmethod width ((character pinned-character))
  "Return pinned CHARACTER's width."
  (width (character-metrics character)))

(defmethod height ((character pinned-character))
  "Return pinned CHARACTER's height."
  (height (character-metrics character)))

(defmethod depth ((character pinned-character))
  "Return pinned CHARACTER's depth."
  (depth (character-metrics character)))

(defun pin-character (character board x &optional (y 0))
  "Pin CHARACTER on BOARD at position (X, Y)."
  (make-instance 'pinned-character
    :character-metrics character :board board :x x :y y))


;; Always pinned, so no "pinned" prefix.
(defclass hyphenation-clue (pinned)
  ((explicitp :documentation
	      "Whether this hyphenation clue comes from an explicit hyphen."
	      :initform t :initarg
	      :explicit :reader explicitp))
  (:documentation "The HYPHENATION-CLUE class.
Hyphenation clues are positioned at Y = 0."))

(defun hyphenation-clue-p (object)
  "Return T if OBJECT is a hyphenation clue."
  (typep object 'hyphenation-clue))

(defun make-hyphenation-clue (board x &optional (explicit t))
  "Pin possibly EXPLICIT hyphenation clue at (X, 0)."
  (make-instance 'hyphenation-clue :board board :x x :explicit explicit))


;; Always pinned, so no "pinned" prefix.
(defclass bed (pinned)
  ((width :documentation "The river bed's width"
	  :initarg :width :reader width))
  (:documentation "The river BED class.
River beds stand in the middle of glue space and are positioned at Y = 0."))

(defun bedp (object)
  "Return T if OBJECT is a river bed."
  (typep object 'bed))

(defun make-bed (board x width)
  "Make a river bed of WIDTH centered at X."
  (make-instance 'bed :board board :x x :width width))




;; ==========================================================================
;; Lines
;; ==========================================================================

(defclass line ()
  ((harray :documentation "The corresponding harray."
	   :initarg :harray
	   :reader harray)
   (start-idx :documentation "This line's start index in HARRAY."
	      :initarg :start-idx
	      :reader start-idx)
   (stop-idx :documentation "This line's stop index in HARRAY."
	     :initarg :stop-idx
	     :reader stop-idx)
   (scale :documentation "The line'scale, as computed by the algorithm.
It may be different from the effective scale used to pin the objects,
depending on the algorithm itself, and on the Overstretch and Overshrink
disposition options)."
	  :initform 0
	  :initarg :scale
	  :reader scale)
   (effective-scale
    :documentation "The line's effective scale, used for pinning the objects.
It may be different from the scale computed by the algorithm in use, depending
on the algorithm itself, and on the Overstretch and Overshrink disposition
options)."
    :initarg :effective-scale
    :reader effective-scale)
   (pinned-objects :documentation "The list of pinned objects."
		   :reader pinned-objects))
  (:documentation "The LINE class.
A line contains a list of pinned objects (currently, characters and
hyphenation clues). The objects are positioned relatively to the line's
origin. A line also remembers its scale factor."))

(defmethod hyphenated ((line line))
  "Return LINE's hyphenation status."
  (hyphenated (aref (harray line) (1- (stop-idx line)))))

(defmethod penalty
    ((line line) &aux (element (aref (harray line) (1- (stop-idx line)))))
  "Return LINE's penalty."
  (if (break-point-p element) (penalty element) 0))

(defmethod width ((line line) &aux (object (car (last (pinned-objects line)))))
  "Return LINE's width."
  (+ (x object) (width object)))

(defmethod height ((line line))
  "Return LINE's height."
  (loop :for object :in (pinned-objects line) :maximize (height object)))

(defmethod depth ((line line))
  "Return LINE's depth."
  (loop :for object :in (pinned-objects line) :maximize (depth object)))

(defun flatten-harray (harray start stop)
  "Return a flattened list of HARRAY elements between START and STOP."
  (loop :for i :from start :upto (1- stop)
	:for elt := (haref harray i start stop)
	:if (consp elt) :append elt :else :collect elt))

(defmethod initialize-instance :after ((line line) &key beds &aux scale)
  "Possibly initialize the LINE's effective scale, and pin its objects.
Maybe also include river BEDS."
  ;; #### NOTE: infinite scaling means that we do not have any elasticity.
  ;; Leaving things as they are, we would end up doing (* +/-∞ 0) below, which
  ;; is not good. However, the intended value of (* +/-∞ 0) is 0 here (again,
  ;; no elasticity) so we can get the same behavior by resetting SCALE to 0.
  (unless (slot-boundp line 'effective-scale)
    (setf (slot-value line 'effective-scale) (scale line)))
  (setq scale (if (numberp (effective-scale line)) (effective-scale line) 0))
  (setf (slot-value line 'pinned-objects)
	(loop :with x := 0 :with w
	      :with harray := (harray line)
	      :with last-elt := (aref harray (1- (length harray)))
	      :for elt
		:in (flatten-harray harray (start-idx line) (stop-idx line))
	      :if (eq elt :explicit-hyphenation-clue)
		:collect (make-hyphenation-clue line x)
	      :else :if (eq elt :hyphenation-clue)
		      :collect (make-hyphenation-clue line x nil)
	      :else :if (typep elt 'tfm:character-metrics)
		      :collect (pin-character elt line x)
		      :and :do (incf x (width elt))
	      :else :if (kernp elt)
		      :do (incf x (width elt))
	      :else :if (gluep elt)
		:do (setq w (width elt))
		:and :unless (zerop scale)
		       :do (incf w (if (> scale 0)
				     (* scale (stretch elt))
				     (* scale (shrink elt))))
		     :end
		:and :when (and beds (not (eq elt last-elt)))
		       ;; do not count a final glue as a river bed.
		       :collect (make-bed line (+ x (/ w 2)) w) :end
		:and :do (incf x w))))

(defmethod properties strnlcat ((line line))
  "Advertise LINE's width. This is the default method."
  (format nil "Width: ~Apt.~%Scale: ~A~:[~; (effective: ~A)~]"
    (float (width line))
    ($float (scale line))
    ($/= (scale line) (effective-scale line))
    ($float (effective-scale line))))




;; ==========================================================================
;; Pinned lines
;; ==========================================================================

(defclass pinned-line (pinned)
  ((line :documentation "The corresponding line."
	 :initarg :line
	 :reader line))
  (:documentation "The PINNED-LINE class."))

(defmethod width ((line pinned-line))
  "Return pinned LINE's width."
  (width (line line)))

(defmethod height ((line pinned-line))
  "Return pinned LINE's height."
  (height (line line)))

(defmethod depth ((line pinned-line))
  "Return pinned LINE's depth."
  (depth (line line)))

(defmethod scale ((line pinned-line))
  "Return pinned LINE's scale."
  (scale (line line)))

(defmethod effective-scale ((line pinned-line))
  "Return pinned LINE's effective scale factor."
  (effective-scale (line line)))

(defmethod hyphenated ((line pinned-line))
  "Return pinned LINE's hyphenation status."
  (hyphenated (line line)))

(defmethod penalty ((line pinned-line))
  "Return pinned LINE's penalty."
  (penalty (line line)))

;; #### NOTE: we don't have having nesting feature right now, so no board for
;; pinned lines (toplevel objects).
(defun pin-line (line x y)
  "Pin LINE at position (X, Y)."
  (let ((pinned-line (make-instance 'pinned-line :line line :x x :y y)))
    ;; #### FIXME: gross hack alert. Pinned objects have their line as the
    ;; board. But a line is not a pinned object, so it has no 2D coordinates,
    ;; and there is no back pointer from a line to a pinned line. For rivers
    ;; detection, I'm thus changing the beds boards to their pinned line for
    ;; now. Of course, this is completely broken.
    (mapc (lambda (object)
	    (when (bedp object)
	      (setf (slot-value object 'board) pinned-line)))
      (pinned-objects line))
    pinned-line))

;; #### TODO: this is gross but it works for now (we use a single font). 1.2
;; (expressed in ratio to avoid going all floats) is what TeX uses with the
;; Computer Modern fonts. But we should get the appropriate value somewhere
;; (it's up to the font designers, but it's not in the TFM format for
;; example).
(defun baseline-skip (harray)
  "Return HARRAY's baseline skip."
  (* 12/10
     (tfm:design-size (tfm:font (find 'tfm:character-metrics harray
				  :key #'type-of)))))

(defun pin-lines (lines disposition width)
  "Pin LINES in DISPOSITION for a paragraph of WIDTH."
  (let ((baseline-skip (if lines (baseline-skip (harray (first lines))) 0)))
    (loop :for line :in lines
	  :for x := (case disposition
		      ((:flush-left :justified) 0)
		      (:centered (/ (- width (width line)) 2))
		      (:flush-right (- width (width line))))
	  ;; #### TODO: nothing fancy about interline spacing yet.
	  :for y := 0 :then (+ y baseline-skip)
	  :collect (pin-line line x y))))
