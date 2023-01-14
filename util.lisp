(in-package :etap)

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

(defmacro endpush (object place)
  "Push OBJECT at the end of PLACE."
  `(setf ,place (nconc ,place (list ,object))))


(defmacro define-calibration (calibration min default max)
  `(define-constant ,calibration '(,min ,default ,max)))

(defmacro calibrate (variable calibration &optional infinity)
  `(cond ((null ,variable)
	  (setq ,variable (cadr ,calibration)))
	 ((<= ,variable (car ,calibration))
	  (setq ,variable ,(if infinity :-infinity `(car ,calibration))))
	 ((>= ,variable (caddr ,calibration))
	  (setq ,variable ,(if infinity :+infinity `(caddr ,calibration))))))

(defmacro calibrate-variable
    (variable prefix
     &optional infinity
     &aux (calibration (intern (format nil "+~A-~A+" prefix variable))))
  `(calibrate ,variable ,calibration ,infinity))


(defmacro default (variable choices)
  `(when (null ,variable) (setq ,variable (car ,choices))))

(defmacro default-variable
    (variable prefix
     &aux (choices (intern (format nil "+~A-~AS+" prefix variable))))
  `(default ,variable ,choices))
