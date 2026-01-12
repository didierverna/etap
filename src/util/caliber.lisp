(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Calibers
;; ==========================================================================

;; #### TODO: a lot of the complication below would go away with custom
;; widgets looking like rotary knobs.

(defstruct (caliber (:constructor make-caliber
			(property min default max &key infinity bounded)))
  "The CALIBER structure.
Calibers establish a correspondence between scalar values and their GUI
representation through sliders.

A caliber defines MIN, DEFAULT, and MAX values for PROPERTY, all numerical.
By default, calibrated values will be clamped within these bounds. This
behavior can be modified as follows.
- Values below MIN or above MAX (inclusive) will be converted to -∞ (resp. +∞)
  depending on INFINITY (:MIN, :MAX, or T meaning both)
- Otherwise, the behavior is further controlled by BOUNDED (NIL, :MIN, MAX, or
  T meaning both). When BOUNDED, the numerical bounds are a hard limit.
  Otherwise, all values are acceptable. In such a case, the numerical bounds
  are only used to limit the GUI views on values."
  property min default max infinity bounded)

(defmacro define-caliber
    (prefix property min default max &rest keys &key infinity bounded)
  "Define a *PREFIX-PROPERTY* caliber with MIN, DEFAULT, and MAX values.
The corresponding PROPERTY is automatically interned in the keyword package.
If supplied, INFINITY may be :MIN, :MAX, or T meaning both.
See the `caliber' structure for information on INFINITY and BOUNDED."
  (declare (ignore infinity bounded))
  `(defparameter ,(intern (format nil "*~A-~A*" prefix property))
     (make-caliber ,(intern (symbol-name property) :keyword)
		   ,min ,default, max ,@keys)))

(defmacro calibrate
    (prefix name
     &key (earmuffs t)
     &aux (earmuff (if earmuffs "*" ""))
	  (variable (intern (format nil "~A~A~A" earmuff name earmuff)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate variable according to the *PREFIX-NAME* caliber.
The variable's name is NAME or *NAME* depending on EARMUFFS (T by default).
- If variable is null, set it to the caliber's default.
- If variable is already properly calibrated, leave it be.
- If variable is out of bounds (large inequality), clamp it or set it to an
  infinity value (depending on the caliber's behavior)."
  `(setq ,variable (calibrated-value ,variable ,caliber)))


(defun calibrated-value (value caliber)
  "Return CALIBERated VALUE."
  (cond ((null value)
	 (caliber-default caliber))
	(($<= value (caliber-min caliber))
	 (cond ((member (caliber-infinity caliber) '(t :min))
		-∞)
	       ((member (caliber-bounded caliber) '(t :min))
		(caliber-min caliber))
	       (t value)))
	(($>= value (caliber-max caliber))
	 (cond ((member (caliber-infinity caliber) '(t :max))
		+∞)
	       ((member (caliber-bounded caliber) '(t :max))
		(caliber-max caliber))
	       (t value)))
	(t value)))

(defun decalibrated-value (value caliber)
  "Return deCALIBERated VALUE."
  (cond ((null value)
	 (caliber-default caliber))
	((eq value +∞) (caliber-max caliber))
	((eq value -∞) (caliber-min caliber))
	(t value)))
