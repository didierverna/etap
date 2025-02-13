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


(defgeneric properties (object &key &allow-other-keys)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))




;; ==========================================================================
;; Pinned Items
;; ==========================================================================

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
