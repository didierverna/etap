(in-package :etap)


;; For the interface.

(defparameter *dispositions*
  '(:flush-left :centered :flush-right :justified))

(defparameter *disposition-options* '((:sloppy t)))

(defparameter *disposition-options-help-keys*
  '(:disposition-option-sloppy))

(defparameter *disposition-options-tooltips*
  '(:disposition-option-sloppy
    "In Justified disposition, stretch or shrink as needed,
ignoring the font's inter-word spacing boundaries."))



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

(defun make-pinned-character (character &rest initargs &key x y)
  "Make a new pinned CHARACTER at position (X, Y)."
  (declare (ignore x y))
  (apply #'make-instance 'pinned-character
    :character-metrics character initargs))


(defclass pinned-hyphenation-clue (pinned)
  ()
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

(defun make-pinned-hyphenation-clue (&rest initargs &key x y)
  "Make a new pinned hyphenation clue at (X, Y)."
  (declare (ignore x y))
  (apply #'make-instance 'pinned-hyphenation-clue initargs))



;; =====
;; Lines
;; =====

(defclass line ()
  ((pinned-objects :initarg :pinned-objects :accessor pinned-objects
		   :documentation "The list of pinned objects."))
  (:documentation "The LINE class.
A line contains a list of pinned objects (currently, characters and
hyphenation clues). The objects are positioned relatively to the line's
origin."))


(defmethod width ((line line) &aux (object (car (last (pinned-objects line)))))
  "Return LINE's width."
  (+ (x object) (width object)))

(defmethod height ((line line))
  "Return LINE's height."
  (loop :for object :in (pinned-objects line) :maximize (height object)))

(defmethod depth ((line line))
  "Return LINE's depth."
  (loop :for object :in (pinned-objects line) :maximize (depth object)))

(defun make-line (objects)
  "Make a new line of pinned OBJECTS"
  (make-instance 'line :pinned-objects objects))


(defun flatten-lineup (lineup start stop)
  "Return a flattened list of LINEUP elements between START and STOP."
  (loop :for i :from start :upto (1- stop)
	:for elt := (lineup-aref lineup i start stop)
	:if (consp elt) :append elt :else :collect elt))

(defun create-line (lineup start end &optional (scale 0))
  "Create a possibly SCALEd line from LINEUP chunk between START and STOP."
  (unless end (setq end (length lineup)))
  (make-line (loop :with x := 0
		   :for elt :in (flatten-lineup lineup start end)
		   :if (eq elt :hyphenation-clue)
		     :collect (make-pinned-hyphenation-clue :x x)
		   :else :if (typep elt 'tfm:character-metrics)
		     :collect (make-pinned-character elt :x x)
		     :and :do (incf x (width elt))
		   :else :if (kernp elt)
		     :do (incf x (width elt))
		   :else :if (gluep elt)
		     :do (incf x (width elt))
		     :and :unless (zerop scale)
			    :do (incf x (if (> scale 0)
					  (* scale (stretch elt))
					  (* scale (shrink elt)))))))

(defun create-justified-line
    (lineup start stop width sloppy
     &aux (scale (lineup-scale lineup start stop width)))
  "Create a line from LINEUP chunk between START and STOP, justified to WIDTH.
If no elasticity is available, the created line will not be justified.
If elasticity is available, get as close as possible to WIDTH within the
limits of the available elasticity, unless SLOPPY, in which case disregard
those limits."
  (if scale
    (create-line lineup start stop
		 (cond (sloppy (max scale -1))
		       ((zerop scale) 0)
		       ((< scale 0) (max scale -1))
		       ((> scale 0) (min scale 1))))
    (create-line lineup start stop)))