(in-package :etap)


;; =========================
;; Parametrization Utilities
;; =========================

;; For easy exchange with the interface and manipulation as function keyword
;; parameters.

(defun algorithm-type (algorithm)
  "Return ALGORITHM type.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (car-or-symbol algorithm))

(defun algorithm-options (algorithm)
  "Return ALGORITHM options.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (cdr-or-nil algorithm))


(defstruct (caliber (:constructor make-caliber (min default max)))
  "The CALIBER structure.
A caliber represents values that have a mininum, a maximum, and a default."
  min default max)

(defmacro define-caliber (prefix name min default max)
  "Define a *PREFIX-NAME* caliber with MIN, DEFAULT, and MAX values."
  `(defparameter ,(intern (format nil "*~A-~A*" prefix name))
     (make-caliber ,min ,default, max)))

(defmacro calibrate
    (prefix name
     &optional infinity
     &aux (variable (intern (format nil "~A" name)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate NAMEd variable according to the *PREFIX-NAME* caliber.
If the variable's value is out of bounds, either clamp it (the default),
or use INFINITY values."
  `(cond ((null ,variable)
	  (setq ,variable (caliber-default ,caliber)))
	 ((<= ,variable (caliber-min ,caliber))
	  (setq ,variable ,(if infinity :-infinity `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if infinity :+infinity `(caliber-max ,caliber))))))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "~A" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If NAMEd variable is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))



;; ====================
;; Extended Arithmetics
;; ====================

;; Arithmetics with a notion of infinity.

(defun !< (x y)
  "Infinity handling <."
  (cond ((eql x y) nil)
	((and (numberp x) (numberp y)) (< x y))
	((or (eq x :-infinity) (eq y :+infinity)) t)
	(t nil)))

(defun !<= (x y)
  "Infinity handling <=."
  (or (eql x y) (!< x y)))

(defun !+ (x y)
  "Infinity handling +."
  (cond ((and (numberp x) (numberp y)) (+ x y))
	((numberp x) y)
	((numberp y) x)
	((eq x y) x)
	;; #### WARNING: we consider that -infinity + +infinity gives
	;; +infinity. The rationale is that this situation would occur when
	;; calculating badness + hyphen-penalty on an hyphen-overfull, with
	;; hyphen penalties of -infinity. But we always consider overfulls as
	;; infinitely bad. Note that this means that mandatory but not
	;; overfull hyphen breaks need to be handled first.
	(t :+infinity)))

(defun !expt (base power)
  "Infinity handling (BASE only) expt."
  (cond ((eq base :+infinity) :+infinity)
	((numberp base) (expt base power))
	((eq base :-infinity)
	 (cond ((zerop power) 1)
	       ((evenp power) :+infinity)
	       (t :-infinity)))))



;; ====================
;; Quality measurements
;; ====================

(defparameter *maximum-badness* 10000)

(defun scale-badness (scale)
  (if (or (null scale) (< scale -1))
    :+infinity
    (min (* 100 (expt (abs scale) 3)) *maximum-badness*)))

(defun badness (lineup start stop width &optional emergency-stretch)
  (scale-badness (lineup-scale lineup start stop width emergency-stretch)))



;; ===========
;; Entry Point
;; ===========

(defgeneric create-lines
    (lineup width disposition algorithm &key &allow-other-keys)
  (:documentation
   "Typeset LINEUP as a DISPOSITION paragraph of WIDTH with ALGORITHM."))
