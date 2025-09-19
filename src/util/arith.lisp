(in-package :etap)

;; Arithmetics with a notion of infinity.


;; ==========================================================================
;; Extended Arithmetics
;; ==========================================================================

;; #### NOTE: we may use EQL to compare numbers below, which is fine because
;; we're only supposed to work on ratios (all numbers of the same type).

(defconstant +∞ '+∞ "The + infinity value.")
(defconstant -∞ '-∞ "The - infinity value.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is to prevent the OR macro expansion in $<= to trigger an undefined
  ;; function warning.
  (setf (symbol-function '$=) #'eql))

(defun $< (x y)
  "Infinity handling <."
  (cond ((eql x y) nil)
	((or (eq x -∞) (eq y +∞)) t)
	((and (numberp x) (numberp y)) (< x y))
	(t nil)))

(defun $<= (x y)
  "Infinity handling <=."
  (or ($= x y) ($< x y)))

(defun $> (x y)
  "Infinity handling >."
  (cond ((eql x y) nil)
	((or (eq x +∞) (eq y -∞)) t)
	((and (numberp x) (numberp y)) (> x y))
	(t nil)))

(defun $>= (x y)
  "Infinity handling >=."
  (or ($= x y) ($> x y)))

(defun $/= (x y)
  "Infinity handling /=."
  (not ($= x y)))

(defun $+ (x y)
  "Infinity handling +."
  (cond ((and (numberp x) (numberp y)) (+ x y))
	((numberp x) y)
	((numberp y) x)
	((eq x y) x)
	(t (error "Can't compute -∞ + +∞."))))

(defun $- (x y)
  "Infinity handling -."
  (cond ((and (numberp x) (numberp y)) (- x y))
	((numberp x) (if (eq y +∞) -∞ +∞))
	((numberp y) x)
	((and (eq x +∞) (eq y -∞)) +∞)
	((and (eq x -∞) (eq y +∞)) -∞)
	(t (error "Can't compute ∞ - ∞."))))

(defun $* (x y)
  "Infinity handling *."
  (cond ((or (and (eq x +∞) (eq y +∞)) (and (eq x -∞) (eq y -∞)))
	 +∞)
	((or (and (eq x +∞) (eq y -∞)) (and (eq x -∞) (eq y +∞)))
	 -∞)
	((or (and (not (numberp x)) (eql y 0))
	     (and (eql x 0) (not (numberp y))))
	 (error "Can't compute 0 * ∞."))
	((and (eq x +∞) (numberp y)) (if (>= y 0) +∞ -∞))
	((and (eq x -∞) (numberp y)) (if (>= y 0) -∞ +∞))
	((and (numberp x) (eq y +∞)) (if (>= x 0) +∞ -∞))
	((and (numberp x) (eq y -∞)) (if (>= x 0) -∞ +∞))
	(t (* x y))))

(defun $/ (x y)
  "Infinity handling /."
  (cond ((and (not (numberp x)) (not (numberp y)))
	 (error "Don't know how to divide ∞ by ∞."))
	((and (numberp x) (not (numberp y))) 0) ; note that (eql -0 0)
	((and (eq x +∞) (numberp y)) (if (>= y 0) +∞ -∞))
	((and (eq x -∞) (numberp y)) (if (>= y 0) -∞ +∞))
	((zerop y) (if (>= x 0) +∞ -∞))
	(t (/ x y))))

(defun $^ (base power)
  "Infinity handling (BASE only) expt."
  (cond ((eq base +∞) (if (zerop power) 1 +∞))
	((numberp base) (expt base power))
	((eq base -∞)
	 (cond ((zerop power) 1)
	       ((evenp power) +∞)
	       (t -∞)))))

(defun $max (x y)
  "Infinity handling MAX."
  (if ($>= x y) x y))

(defun $min (x y)
  "Infinity handling MIN."
  (if ($<= x y) x y))

(defun $abs (x)
  "Infinity handling ABS."
  (if (numberp x) (abs x) +∞))

(defun $float (inumber &optional prototype)
  "Infinity handling FLOAT."
  (if (numberp inumber)
    (if prototype (float inumber prototype) (float inumber))
    inumber))




;; ==========================================================================
;; Calibers
;; ==========================================================================

(defstruct
    (caliber
     (:constructor make-caliber (property min default max &key infinity)))
  "The CALIBER structure.
A caliber defines MIN, DEFAULT, and MAX values for PROPERTY.
Additionally, extreme values will be converted to -∞ (resp. +∞) depending on
INFINITY (:MIN, :MAX, or T meaning both)."
  property min default max infinity)

(defmacro define-caliber
    (prefix property min default max &rest keys &key infinity)
  "Define a *PREFIX-PROPERTY* caliber with MIN, DEFAULT, and MAX values.
The corresponding PROPERTY is automatically interned in the keyword package.
If supplied, INFINITY may be :MIN, :MAX, or T meaning both.
In such a case, a calibrated value equal to MIN (resp. MAX) will be converted
to -∞ (resp. +∞)."
  (declare (ignore infinity))
  `(defparameter ,(intern (format nil "*~A-~A*" prefix property))
     (make-caliber ,(intern (symbol-name property) :keyword)
		   ,min ,default, max ,@keys)))

(defun calibrated-value (value caliber)
  "Return CALIBERated VALUE."
  (cond ((null value)
	 (caliber-default caliber))
	((<= value (caliber-min caliber))
	 (if (member (caliber-infinity caliber) '(t :min))
	   -∞
	   (caliber-min caliber)))
	((>= value (caliber-max caliber))
	 (if (member (caliber-infinity caliber) '(t :max))
	   +∞
	   (caliber-max caliber)))
	(t value)))

(defun decalibrated-value (value caliber)
  "Return deCALIBERated VALUE (VALUE is supposed to be CALIBERated)."
  ;; The checks are simpler here since we're supposed to be working on a
  ;; previously calibrated value, so not NULL, and if not +/-∞, within the
  ;; caliber's min and max bounds.
  (cond ((eq value +∞) (caliber-max caliber))
	((eq value -∞) (caliber-min caliber))
	(t value)))

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
