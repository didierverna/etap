(in-package :etap)


;; For the interface.

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



;; ==============
;; Pinned Objects
;; ==============

(defclass pinned-character (pinned)
  ((character-metrics :documentation "The pinned character."
		      :initarg :character-metrics
		      :reader character-metrics))
  (:documentation "The PINNED-CHARACTER class.
The character's 2D position is relative to the line it belongs to."))

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

(defun pin-character (character &rest initargs &key x y)
  "Pin CHARACTER at position (X, Y)."
  (declare (ignore x y))
  (apply #'make-instance 'pinned-character
    :character-metrics character initargs))


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

(defmethod width ((clue hyphenation-clue))
  "Return hyphenation clue's width (0)."
  0)

(defmethod height ((clue hyphenation-clue))
  "Return hyphenation clue's height (0)."
  0)

(defmethod depth ((clue hyphenation-clue))
  "Return hyphenation clue's depth (0)."
  0)

(defun make-hyphenation-clue (x &optional (explicit t))
  "Pin possibly EXPLICIT hyphenation clue at (X, 0)."
  (make-instance 'hyphenation-clue :x x :explicit explicit))



;; =====
;; Lines
;; =====

(defclass line ()
  ((lineup :documentation "The corresponding lineup."
	   :initarg :lineup
	   :reader lineup)
   (start-idx :documentation "This line's start index in LINEUP."
	      :initarg :start-idx
	      :reader start-idx)
   (stop-idx :documentation "This line's stop index in LINEUP."
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

(defgeneric hyphenated (object)
  (:documentation "Whether OBJECT is hyphenated.
Possible values are nil, :explicit, or :implicit.")
  (:method
      ((line line) &aux (element (aref (lineup line) (1- (stop-idx line)))))
    "Whether LINE is hyphenated.
Possible values are nil, :explicit, or :implicit."
    (when (hyphenation-point-p element)
      (if (explicitp element) :explicit :implicit))))

(defmethod penalty
    ((line line) &aux (element (aref (lineup line) (1- (stop-idx line)))))
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

(defun flatten-lineup (lineup start stop)
  "Return a flattened list of LINEUP elements between START and STOP."
  (loop :for i :from start :upto (1- stop)
	:for elt := (lineup-aref lineup i start stop)
	:if (consp elt) :append elt :else :collect elt))

(defmethod initialize-instance :after ((line line) &key &aux scale)
  "Possibly initialize the LINE's effective scale, and pin its objects."
  ;; #### NOTE: infinite scaling means that we do not have any elasticity.
  ;; Leaving things as they are, we would end up doing (* +/-∞ 0) below, which
  ;; is not good. However, the intended value of (* +/-∞ 0) is 0 here (again,
  ;; no elasticity) so we can get the same behavior by resetting SCALE to 0.
  (unless (slot-boundp line 'effective-scale)
    (setf (slot-value line 'effective-scale) (scale line)))
  (setq scale (if (numberp (effective-scale line)) (effective-scale line) 0))
  (setf (slot-value line 'pinned-objects)
	(loop :with x := 0
	      :for elt :in (flatten-lineup
			    (lineup line) (start-idx line) (stop-idx line))
	      :if (eq elt :explicit-hyphenation-clue)
		:collect (make-hyphenation-clue x)
	      :else :if (eq elt :hyphenation-clue)
		      :collect (make-hyphenation-clue x nil)
	      :else :if (typep elt 'tfm:character-metrics)
		      :collect (pin-character elt :x x)
		      :and :do (incf x (width elt))
	      :else :if (kernp elt)
		      :do (incf x (width elt))
	      :else :if (gluep elt)
		      :do (incf x (width elt))
		      :and :unless (zerop scale)
			     :do (incf x (if (> scale 0)
					   (* scale (stretch elt))
					   (* scale (shrink elt)))))))

(defgeneric line-properties (line)
  (:documentation "Return a string describing LINE's properties.")
  (:method-combination strnlcat :most-specific-last)
  (:method strnlcat ((line line))
    "Advertise LINE's width. This is the default method."
    (format nil "Width: ~Apt.~%Scale: ~A~:[~; (effective: ~A)~]"
      (float (width line))
      ($float (scale line))
      ($/= (scale line) (effective-scale line))
      ($float (effective-scale line)))))



;; ==========
;; Paragraphs
;; ==========

(defclass paragraph ()
  ((width :documentation "The paragraph's width."
	  :initarg :width
	  :reader width)
   (disposition :documentation "The paragraph's disposition."
		:initarg :disposition
		:reader disposition)
   (pinned-lines :documentation "The paragraph's pinned lines."
		 :initform nil
		 :initarg :pinned-lines
		 :reader pinned-lines))
  (:documentation "The PARAGRAPH class."))

(defgeneric paragraph-properties (paragraph)
  (:documentation "Return a string describing PARAGRAPH's properties.")
  (:method-combination strnlcat :most-specific-last)
  (:method strnlcat ((paragraph paragraph))
    "Advertise PARAGRAPH's vertical dimensions and line number.
This is the default method."
    (format nil "Vertical size: ~Apt (height: ~Apt, depth: ~Apt).~%~A line~:P."
      (float (+ (height paragraph) (depth paragraph)))
      (float (height paragraph))
      (float (depth paragraph))
      (length (pinned-lines paragraph)))))
