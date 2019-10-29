(in-package :etap)

(defmacro endpush (object place)
  `(setf ,place (nconc ,place (list ,object))))
