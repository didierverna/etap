(in-package :etap)

(defclass nlstring ()
  ((text :documentation "The nlstring's text (a string, or NIL; the default)."
	 :initform nil :initarg :text :accessor text)
   (language :documentation "The nlstring's language, *LANGUAGE* by default."
	     :initform *language* :initarg :language :accessor language))
  (:documentation "The NLString (Natural Language String) class."))

(defun make-nlstring (&rest initargs &key text language)
  "Make a new NLString with TEXT in LANGUAGE."
  (declare (ignore text language))
  (apply #'make-instance 'nlstring initargs))

(defvar *text*
  "In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful; and the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
Close by the king's castle lay a great dark forest, and under an old lime-tree
in the forest was a well, and when the day was very warm, the king's child
went out into the forest and sat down by the side of the cool fountain; and
when she was bored she took a golden ball, and threw it up on high and caught
it; and this ball was her favorite plaything."
  "The default text.")
