(in-package :etap)

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

(defmacro endpush (object place)
  `(setf ,place (nconc ,place (list ,object))))
