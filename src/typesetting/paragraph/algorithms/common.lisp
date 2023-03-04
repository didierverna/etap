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
	  (setq ,variable ,(if infinity -∞ `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if infinity +∞ `(caliber-max ,caliber))))))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "~A" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If NAMEd variable is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))



;; ====================
;; Quality measurements
;; ====================

(defparameter *maximum-badness* 10000)

(defun scale-badness (scale)
  (if (or (null scale) (< scale -1))
    +∞
    (min (* 100 (expt (abs scale) 3)) *maximum-badness*)))

(defun badness (lineup start stop width &optional emergency-stretch)
  (scale-badness (lineup-scale lineup start stop width emergency-stretch)))



;; ===========
;; Entry Point
;; ===========

(defgeneric make-lines
    (lineup disposition width algorithm &key &allow-other-keys)
  (:documentation
   "Typeset LINEUP as a DISPOSITION paragraph of WIDTH with ALGORITHM.")
  (:method :around (lineup disposition width algorithm &rest args)
    "Proceed only if LINEUP is not null, and transform it into an array."
    (when lineup
      (setq lineup (make-array (length lineup) :initial-contents lineup))
      (apply #'call-next-method lineup disposition width algorithm args))))
