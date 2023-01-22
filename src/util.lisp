(in-package :etap)

(defmacro endpush (object place)
  "Push OBJECT at the end of PLACE."
  `(setf ,place (nconc ,place (list ,object))))


;; These two make it more readable to handle interface and keyword arguments
;; which can be of the form STUFF, or (STUFF OPTIONS...).

(defun car-or-symbol (object)
  "Return OBJECT, if a symbol, or its CAR if a cons. Error otherwise."
  (etypecase object
    (cons (car object))
    (symbol object)))

(defun cdr-or-nil (object)
  "Return NIL if OBJECT is a symbol, or its CDR if a cons. Error otherwise."
  (etypecase object
    (cons (cdr object))
    (symbol nil)))

