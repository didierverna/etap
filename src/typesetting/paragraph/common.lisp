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
ignoring the font's inter-word spacing recommendation."
    :disposition-option-overshrink
    "In Justified disposition, shrink as needed,
ignoring the font's inter-word spacing recommendation."))

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
  ((pinned-objects :initarg :pinned-objects :accessor pinned-objects
		   :documentation "The list of pinned objects.")
   (scale :initarg :scale :reader scale
	  :documentation "The line's scale factor."))
  (:documentation "The LINE class.
A line contains a list of pinned objects (currently, characters and
hyphenation clues). The objects are positioned relatively to the line's
origin. A line also remembers its scale factor."))


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

;; #### FIXME: we normally support infinite elasticity throughout the code
;; (leading to scalings of 0). However, there's currently only one situation
;; in which this occurs: the infinitely stretchable glue that the KP algorithm
;; appends at the end of the lineup. The function below works correctly in
;; that case because indeed, a final line would return to its normal spacing
;; (scaling = 0). On the other hand, if / when we start to support multiple
;; occurrences of infinite elasticity within the same line, we will need to
;; write things differently, because the actual shrinking / stretching needs
;; to be spread equally over all infinitely elastic glues (hence, we still
;; need to keep the target width around).
(defun make-line (lineup start stop &optional (scale 0))
  "Make a possibly SCALEd line from LINEUP chunk between START and STOP."
  ;; #### FIXME: do we still need this?
  (unless stop (setq stop (length lineup)))
  ;; #### NOTE: infinite scaling means that we do not have any elasticity.
  ;; Leaving things as they are, we would end up doing (* +/-∞ 0) below, which
  ;; is not good. However, the intended value of (* +/-∞ 0) is 0 here (again,
  ;; no elasticity) so we can get the same behavior by resetting SCALE to 0.
  (unless (numberp scale) (setq scale 0))
  (make-instance 'line
    :pinned-objects
    (loop :with x := 0
	  :for elt :in (flatten-lineup lineup start stop)
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
				 (* scale (shrink elt)))))
    :scale scale))

(defun effective-scale (scale overshrink overstretch)
  "Return effective SCALE, normally limited to [-1,+1].
Those limitations may be ignored if OVERSHRINK or OVERSTRETCH."
  (cond ((<< scale 0) (if overshrink scale (mmaaxx scale -1)))
	((== scale 0) 0)
	((>> scale 0) (if overstretch scale (mmiinn scale 1)))))

(defun make-scaled-line (lineup start stop scale overshrink overstretch)
  "Make a SCALEd line from LINEUP chunk between START and STOP.
See `effective-scale' for more information on how SCALE is handled."
  (make-line lineup start stop (effective-scale scale overshrink overstretch)))
