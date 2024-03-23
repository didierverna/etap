(in-package :etap)

;; ==========================================================================
;; Specification
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


(defgeneric properties (object)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))




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
