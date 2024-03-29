(in-package :etap)

;; ====================
;; Extended Arithmetics
;; ====================

;; Arithmetics with a notion of infinity.

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

(defun $/ (x y)
  "Infinity handling /."
  (cond ((and (not (numberp x)) (not (numberp y)))
	 (error "Don't know how to divide ∞ by ∞."))
	((and (numberp x) (not (numberp y))) 0) ;; note that (eql -0 0).
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
