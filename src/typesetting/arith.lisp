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
  ;; This is to prevent the OR macro expansion in <== to trigger an undefined
  ;; function warning.
  (setf (symbol-function '==) #'eql))

(defun << (x y)
  "Infinity handling <."
  (cond ((eql x y) nil)
	((or (eq x -∞) (eq y +∞)) t)
	((and (numberp x) (numberp y)) (< x y))
	(t nil)))

(defun <== (x y)
  "Infinity handling <=."
  (or (== x y) (<< x y)))

(defun /== (x y)
  "Infinity handling /=."
  (not (== x y)))

;; I know what you're gonna say...
(defun ++ (x y)
  "Infinity handling +."
  (cond ((and (numberp x) (numberp y)) (+ x y))
	((numberp x) y)
	((numberp y) x)
	((eq x y) x)
	;; #### FIXME: I think it's wrong to do it like that, and here. This
	;; rationale is specific to the KP algorithm, so we should perform the
	;; check in there rather than in a raw arithmetic function.
	;; #### WARNING: we consider that -∞ + +∞ = +∞. The rationale is that
	;; this situation would occur when calculating badness +
	;; hyphen-penalty on an hyphen-overfull, with hyphen penalties of -∞.
	;; But we always consider overfulls as infinitely bad. Note that this
	;; means that mandatory but not overfull hyphen breaks need to be
	;; handled first.
	(t +∞)))

(defun // (x y)
  "Infinity handling /."
  (cond ((and (not (numberp x)) (not (numberp y)))
	 (error "Don't know how to divide ∞ by ∞."))
	((and (numberp x) (not (numberp y))) 0) ;; note that (eql -0 0).
	((and (eq x +∞) (numberp y)) (if (>= y 0) +∞ -∞))
	((and (eq x -∞) (numberp y)) (if (>= y 0) -∞ +∞))
	((zerop y) (if (>= x 0) +∞ -∞))
	(t (/ x y))))

(defun ^^ (base power)
  "Infinity handling (BASE only) expt."
  (cond ((eq base +∞) (if (zerop power) 1 +∞))
	((numberp base) (expt base power))
	((eq base -∞)
	 (cond ((zerop power) 1)
	       ((evenp power) +∞)
	       (t -∞)))))
