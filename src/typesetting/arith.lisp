(in-package :etap)

;; i=i=i=i=i=i=i=i=i=i=
;; Extended Arithmetics
;; i=i=i=i=i=i=i=i=i=i=

;; Arithmetics with a notion of infinity.

;; #### NOTE: we may use EQL to compare numbers below, which is fine because
;; we're only supposed to work on ratios (all numbers of the same type).

(defconstant +∞ '+∞ "The + infinity value.")
(defconstant -∞ '-∞ "The - infinity value.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is to prevent the OR macro expansion in i<= to trigger an undefined
  ;; function warning.
  (setf (symbol-function 'i=) #'eql))

(defun i< (x y)
  "Infinity handling <."
  (cond ((eql x y) nil)
	((or (eq x -∞) (eq y +∞)) t)
	((and (numberp x) (numberp y)) (< x y))
	(t nil)))

(defun i<= (x y)
  "Infinity handling <=."
  (or (i= x y) (i< x y)))

(defun i> (x y)
  "Infinity handling >."
  (cond ((eql x y) nil)
	((or (eq x +∞) (eq y -∞)) t)
	((and (numberp x) (numberp y)) (> x y))
	(t nil)))

(defun i>= (x y)
  "Infinity handling >=."
  (or (i= x y) (i> x y)))

(defun i/= (x y)
  "Infinity handling /=."
  (not (i= x y)))

(defun i+ (x y)
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

(defun i- (x y)
  "Infinity handling -."
  (cond ((and (numberp x) (numberp y)) (- x y))
	((numberp x) (if (eq y +∞) -∞ +∞))
	((numberp y) x)
	((and (eq x +∞) (eq y -∞)) +∞)
	((and (eq x -∞) (eq y +∞)) -∞)
	;; #### FIXME: same design decision as above, but bad.
	(t +∞)))

(defun i/ (x y) ;; I still know what you're gonna say...
  "Infinity handling /."
  (cond ((and (not (numberp x)) (not (numberp y)))
	 (error "Don't know how to divide ∞ by ∞."))
	((and (numberp x) (not (numberp y))) 0) ;; note that (eql -0 0).
	((and (eq x +∞) (numberp y)) (if (>= y 0) +∞ -∞))
	((and (eq x -∞) (numberp y)) (if (>= y 0) -∞ +∞))
	((zerop y) (if (>= x 0) +∞ -∞))
	(t (/ x y))))

(defun i^ (base power)
  "Infinity handling (BASE only) expt."
  (cond ((eq base +∞) (if (zerop power) 1 +∞))
	((numberp base) (expt base power))
	((eq base -∞)
	 (cond ((zerop power) 1)
	       ((evenp power) +∞)
	       (t -∞)))))

(defun imax (x y)
  "Infinity handling MAX."
  (if (i>= x y) x y))

(defun imin (x y)
  "Infinity handling MIN."
  (if (i<= x y) x y))

(defun iabs (x)
  "Infinity handling ABS."
  (if (numberp x) (abs x) +∞))

(defun ifloat (inumber &optional prototype)
  "Infinity handling FLOAT."
  (if (numberp inumber)
    (if prototype (float inumber prototype) (float inumber))
    inumber))
