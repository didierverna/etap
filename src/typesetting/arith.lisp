(in-package :etap)

;; ====================
;; Extended Arithmetics
;; ====================

;; Arithmetics with a notion of infinity.

(defconstant +∞ '+∞ "The + infinity value.")
(defconstant -∞ '-∞ "The - infinity value.")

(defun << (x y)
  "Infinity handling <."
  (cond ((eql x y) nil)
	((and (numberp x) (numberp y)) (< x y))
	((or (eq x -∞) (eq y +∞)) t)
	(t nil)))

(defun <<= (x y)
  "Infinity handling <=."
  (or (eql x y) (<< x y)))

;; I know what you're gonna say...
(defun ++ (x y)
  "Infinity handling +."
  (cond ((and (numberp x) (numberp y)) (+ x y))
	((numberp x) y)
	((numberp y) x)
	((eq x y) x)
	;; #### WARNING: we consider that -∞ + +∞ = +∞. The rationale is that
	;; this situation would occur when calculating badness +
	;; hyphen-penalty on an hyphen-overfull, with hyphen penalties of -∞.
	;; But we always consider overfulls as infinitely bad. Note that this
	;; means that mandatory but not overfull hyphen breaks need to be
	;; handled first.
	(t +∞)))

(defun ^ (base power)
  "Infinity handling (BASE only) expt."
  (cond ((eq base +∞) +∞)
	((numberp base) (expt base power))
	((eq base -∞)
	 (cond ((zerop power) 1)
	       ((evenp power) +∞)
	       (t -∞)))))



