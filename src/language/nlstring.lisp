(in-package :etap)

(defclass nlstring ()
  ((text
    :documentation "The nlstring's text (a string or NIL; *TEXT* by default)."
    :initform *text* :initarg :text :reader text)
   (language
    :documentation "The nlstring's language (*LANGUAGE* by default)."
    :initform *language* :initarg :language :reader language))
  (:documentation "The NLString (Natural Language String) class."))

(defun make-nlstring (&rest initargs &key text language)
  "Make a new NLString for TEXT in LANGUAGE.
TEXT and LANGUAGE default to *TEXT* and *LANGUAGE* respectively."
  (declare (ignore text language))
  (apply #'make-instance 'nlstring initargs))
