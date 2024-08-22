(in-package :etap)

;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun scaling (width target stretch shrink)
  "Return the amount of scaling required to reach TARGET from WIDTH.
The amount in question is 0 if WIDTH is equal to TARGET.
Otherwise, it's a possibly infinite stretching (positive) or shrinking
(negative) ratio relative to the elasticity provided by STRETCH and SHRINK."
  (cond ((= width target) 0)
	((< width target) ($/ (- target width) stretch))
	((< target width) ($/ (- target width) shrink))))

(defun actual-scales (scale &key (shrink-tolerance -1) (stretch-tolerance 1)
				 (overshrink nil) (overstretch nil))
  "Compute the actual scales for a line, based on required SCALE.
This function returns two values.
- The theoretical scale computed by the algorithm in use. This value depends
  on the algorithm s SHRINK / STRETCH-TOLERANCE (-1 / 1 by default).
- The effective scale, used to pin the line's objects. This value further
  depends on the OVERSHRINK / OVERSTRETCH options (nil by default)."
  (let ((theoretical-scale scale) (effective-scale scale))
    (cond (($< scale 0)
	   (setq theoretical-scale ($max theoretical-scale shrink-tolerance))
	   (unless overshrink (setq effective-scale theoretical-scale)))
	  (($> scale 0)
	   (setq theoretical-scale ($min theoretical-scale stretch-tolerance))
	   (unless overstretch (setq effective-scale theoretical-scale))))
    (values theoretical-scale effective-scale)))


;; --------------------
;; Quality measurements
;; --------------------

;; #### NOTE: the two functions below normally belong to the Knuth-Plass
;; algorithm. They're here because the Best-Fit / Justified one uses them as
;; well.

;; #### NOTE: according to #108, TeX clamps badness values to 10000 which is
;; an approximation of 2^13, and called "infinitely bad", but there's in fact
;; more to it than that when the badness function is used in the paragraph
;; breaking algorithm.

;; According to #853 (and this is explained in #851), an "infinitely bad" + 1
;; value is returned for lines which can't shrink enough, knowing that it is
;; strictly prohibited to shrink more than what's available; that is, for a
;; scaling < -1. Note that this includes lines which have no shrinkability at
;; all.

;; According to #852, an "infinitely bad" value is returned for the whole
;; bunch of lines which would require too much stretching (because of the
;; clamping), but this also includes lines which have no stretchability at
;; all.

;; In other words, for reasonable lines, the badness doesn't make a
;; distinction between shrinking and stretching. However, there is a
;; distinction for unreasonable lines. First of all, the definition of
;; "unreasonable" is different for stretching and shrinking (see the 2
;; paragraphs above). And then, the badness is different for unreasonably
;; shrunk and unreasonably stretched lines.

;; In fact, it seems that TeX uses this distinction only to decide whether or
;; not to deactivate a node (again, as explained in #851). On the other hand,
;; in our implementation, we look at the scaling instead of the badness to
;; make that decision. So it turns out that we can get rid of the clamping
;; altogether. Note also that the tolerance calibration in our implementation
;; turns 10000 into +∞, so we 'll indeed get the same effect as in TeX (cf.
;; #828), that is, to accept arbitrarily bad lines.

;; Consequently, our version of badness below returns +∞ for strictly
;; prohibited scaling (i.e. no scaling available, or negative below -1) and a
;; numerical positive value otherwise.

;; #### TODO: we could generalize the notion of badness to allow /some/
;; overshrinking, possibly with an exponential cost (at least it should be
;; much more important than for stretching). Maybe one difficulty would be
;; that if we want to switch to +∞ when there is no more space between words,
;; the badness computation would then depend on the glue's natural width.

;; #### TODO: we could sign the badness (like the scaling) in order to keep
;; track of whether we're stretching or shrinking.

(defun scale-badness (scale)
  (if (or ($< scale -1) ($= scale +∞))
    +∞
    (* 100 (expt (abs scale) 3))))

(defun local-demerits (badness penalty line-penalty)
  "Return a line's local demerits.
Local demerits do not account for contextual information such as hyphens
adjacency or fitness class difference (that's what they are called \"local\").
They are computed from the line scale's BADNESS, a possible PENALTY where the
line ends, and also include the LINE-PENALTY parameter."
  (cond ((and (numberp penalty) (<= 0 penalty))
	 ($+ ($^ ($+ line-penalty badness) 2) (expt penalty 2)))
	((and (numberp penalty) (< penalty 0))
	 ($+ ($^ ($+ line-penalty badness) 2) (- (expt penalty 2))))
	(t ;; -∞
	 ($^ ($+ line-penalty badness) 2))))




;; ==========================================================================
;; HArrays
;; ==========================================================================

;; An harray can be as simple as an array version of the original hlist, but
;; contrary to the hlist, it depends on the final paragraph disposition and
;; typesetting algorithm in use. Most algorithms post-process the hlist (for
;; example by adjusting penalties or adding glues).

;; ------
;; Access
;; ------

(defun haref (harray i start stop &aux (element (aref harray i)))
  "Return HARRAY element at position I, between START and STOP boundaries.
If element is a discretionary, return the appropriate pre/no/post break part."
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the hlist, including
    ;; ligatures / kerning management in the presence of hyphenation points,
    ;; we may end up with harrays beginning or ending with discretionaries (or
    ;; even consecutive discretionaries for that matter). When discretionaries
    ;; begin or end the harray, we must not consider them as post- or
    ;; pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- stop)) (not (= stop (length harray))))
	   (pre-break element))
	  (t (no-break element)))
    element))

(defun flatten-harray (harray start stop)
  "Return a flattened list of HARRAY elements between START and STOP."
  (loop :for i :from start :upto (1- stop)
	:for elt := (haref harray i start stop)
	:if (consp elt) :append elt :else :collect elt))


;; --------
;; Geometry
;; --------

(defun harray-width (harray start stop)
  "Compute HARRAY's width between START and STOP.
Return five values: the natural, maximum, and minimum width, followed by the
stretch and shrink amounts."
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- stop)
	;; #### FIXME: this works for now, but it is not quite right in the
	;; general case. When ELEMENT is a list (typically the contents of a
	;; discretionary, there could be anything inside, including, e.g.,
	;; glues. See also the long comment above the KERNING function.
	:for element := (haref harray i start stop)
	:do (incf width (width element))
	:when (gluep element)
	  :do (setq stretch ($+ stretch (stretch element))
		    shrink (+ shrink (shrink element)))
	:finally (return (values width ($+ width stretch) (- width shrink)
				 stretch shrink))))

(defun harray-max-width (harray start stop)
  "Return HARRAY's width between START and STOP, with maximal stretching."
  (multiple-value-bind (natural max) (harray-width harray start stop)
    (declare (ignore natural))
    max))

(defun harray-min-width (harray start stop)
  "Return HARRAY's width between START and STOP, with maximal shrinking."
  (multiple-value-bind (natural max min) (harray-width harray start stop)
    (declare (ignore natural max))
    min))

(defun harray-scale (harray start stop target &optional extra)
  "Return the amount of scaling required for HARRAY chunk between START and
STOP to reach TARGET width, possibly with EXTRA stretch.
See `scaling' for more information."
  (multiple-value-bind (width max min stretch shrink)
      (harray-width harray start stop)
    (declare (ignore max min))
    (when extra (setq stretch ($+ stretch extra)))
    (scaling width target stretch shrink)))

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


;; ----------
;; Boundaries
;; ----------

(defclass boundary ()
  ((item
    :documentation "The harray item at that boundary."
    :initarg :item :reader item)
   (stop-idx
    :documentation "The harray index for an end of line at that boundary."
    :initarg :stop-idx :reader stop-idx)
   (start-idx
    :documentation "The harray index for a beginning of line at that boundary."
    :initarg :start-idx :reader start-idx))
  (:default-initargs :allow-other-keys t) ;; allow :harray
  (:documentation "Base class for boundaries.
A boundary represents a possible break point in an harray.
The end of the harray is represented by a special boundary with a null item
and start index (the stop index being the harray's length).

Greedy algorithms may extend this class in order to memoize various aspects of
line computation (see `next-boundary'), essentially because all boundaries
subject to comparison at one point in time relate to the same beginning of
line. Non-greedy algorithms should not."))

(defmethod hyphenated ((boundary boundary))
  "Return BOUNDARY's hyphenation status."
  (hyphenated (item boundary)))

(defun last-boundary-p (boundary)
  "Return T if BOUNDARY is the last one."
  (null (item boundary)))

(defun next-boundary (harray from &optional (boundary-class 'boundary)
				  &rest keys &key &allow-other-keys
				  &aux (length (length harray)))
  "Return the next boundary in HARRAY FROM position, or NIL.
The returned object is an instance of BOUNDARY-CLASS (BOUNDARY by default).
This function understands the terminal case where FROM = HARRAY's length
(possibly coming from the end of harray special boundary), in which case it
signals that there is no more boundary to find by returning NIL."
  (unless (= from length)
    (let* ((idx (position-if #'break-point-p harray :start (1+ from)))
	   (item (when idx (aref harray idx)))
	   stop-idx start-idx)
      (etypecase item
	(glue (setq stop-idx idx start-idx (1+ idx)))
	(discretionary (setq stop-idx (1+ idx) start-idx idx))
	(null (setq stop-idx length)))
      (apply #'make-instance boundary-class
	     :item item :stop-idx stop-idx :start-idx start-idx :harray harray
	     keys))))




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




;; ==========================================================================
;; Algorithm Specification
;; ==========================================================================

(defun algorithm-type (algorithm)
  "Return ALGORITHM type.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (car-or-symbol algorithm))

(defun algorithm-options (algorithm)
  "Return ALGORITHM options.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (cdr-or-nil algorithm))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "*~A*" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If *NAME* is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))


(defmacro define-global-variables (&rest names)
  "Define global variables for all NAMES. Earmuffs are added to all NAMES."
  `(progn
     ,@(mapcar (lambda (name) (list 'defvar (intern (format nil "*~A*" name))))
	 names)))
