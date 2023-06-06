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
  ((character-metrics :initarg :character-metrics :accessor character-metrics
		      :documentation "The pinned character."))
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


(defclass pinned-hyphenation-clue (pinned)
  ((explicitp
    :initform t :initarg :explicit :reader explicitp
    :documentation
    "Whether this hyphenation clue comes from an explicit hyphen."))
  (:documentation "The PINNED-HYPHENATION-CLUE class.
The hyphenation clue's 2D position is relative to the line it belongs to."))

(defun pinned-hyphenation-clue-p (object)
  "Return T if OBJECT is a pinned hyphenation clue."
  (typep object 'pinned-hyphenation-clue))

(defmethod width ((clue pinned-hyphenation-clue))
  "Return pinned hyphenation clue's width (0)."
  0)

(defmethod height ((clue pinned-hyphenation-clue))
  "Return pinned hyphenation clue's height (0)."
  0)

(defmethod depth ((clue pinned-hyphenation-clue))
  "Return pinned hyphenation clue's depth (0)."
  0)

(defun pin-hyphenation-clue (x &optional (explicit t))
  "Pin possibly EXPLICIT hyphenation clue at (X, 0)."
  (make-instance 'pinned-hyphenation-clue :x x :explicit explicit))



;; =====
;; Lines
;; =====

(defclass line ()
  ((lineup :initarg :lineup :reader lineup
	   :documentation "The corresponding lineup.")
   (start-idx :initarg :start-idx :reader start-idx
	      :documentation "This line's start index in LINEUP.")
   (stop-idx :initarg :stop-idx :reader stop-idx
	     :documentation "This line's stop index in LINEUP.")
   (scale :initform 0 :initarg :scale :reader scale
	  :documentation "The line'scale, as computed by the algorithm.
It may be different from the effective scale used to pin the objects,
depending on the algorithm itself, and on the Overstretch and Overshrink
disposition options).")
   (effective-scale
    :initarg :effective-scale :reader effective-scale
    :documentation "The line's effective scale, used for pinning the objects.
It may be different from the scale computed by the algorithm in use, depending
on the algorithm itself, and on the Overstretch and Overshrink disposition
options).")
   (pinned-objects :reader pinned-objects
		   :documentation "The list of pinned objects.")
   (hyphenated :initarg :hyphenated :reader hyphenated
	       :documentation "Whether the line is hyphenated.
Possible values are nil, :explicit, or :implicit."))
  (:documentation "The LINE class.
A line contains a list of pinned objects (currently, characters and
hyphenation clues). The objects are positioned relatively to the line's
origin. A line also remembers its scale factor."))


;; #### FIXME: probably rename this to EFFECTIVE-WIDTH.
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
		:collect (pin-hyphenation-clue x)
	      :else :if (eq elt :hyphenation-clue)
		      :collect (pin-hyphenation-clue x nil)
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
					   (* scale (shrink elt))))))
  (setf (slot-value line 'hyphenated)
	(when (hyphenation-point-p (aref (lineup line) (1- (stop-idx line))))
	  (if (explicitp (aref (lineup line) (1- (stop-idx line))))
	    :explicit
	    :implicit))))

(defgeneric line-properties (line)
  (:documentation "Return a string describing LINE's properties.")
  (:method (line)
    "Advertise LINE's width. This is the default method."
    (format nil "Line width: ~Spt.~%Line scale: ~S~:[~;(effective: ~S)~]"
      (coerce (width line) 'float)
      (coerce (scale line) 'float)
      (/== (scale line) (effective-scale line))
      (effective-scale line))))

;; #### FIXME: this interface is broken because of the hardwired limits on
;; scaling. They're ok for many algorithms, but TeX has its own tolerance
;; which can accept some overstretch, and so those lines shouldn't be
;; considered as overstretched. In fact, we need to keep track of each
;; algorithm's decision, and the over* options should override that. Also,
;; we'd need two different sets of visual indicators: indicators for when
;; we're over the font's recommendations, and indicators for when we override
;; the algorithm's decision.

#+()(defun effective-scale (scale overshrink overstretch)
  "Return effective SCALE, normally limited to [-1,+1].
Those limitations may be ignored if OVERSHRINK or OVERSTRETCH."
  (cond ((<< scale 0) (if overshrink scale (mmaaxx scale -1)))
	((== scale 0) 0)
	((>> scale 0) (if overstretch scale (mmiinn scale 1)))))

#+()(defun make-scaled-line (lineup start stop scale overshrink overstretch)
  "Make a SCALEd line from LINEUP chunk between START and STOP.
See `effective-scale' for more information on how SCALE is handled."
  (make-line lineup start stop (effective-scale scale overshrink overstretch)))
