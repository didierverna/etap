(in-package :etap)

(defparameter *text*
  "In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful; and the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
Close by the king's castle lay a great dark forest, and under an old lime-tree
in the forest was a well, and when the day was very warm, the king's child
went out into the forest and sat down by the side of the cool fountain; and
when she was bored she took a golden ball, and threw it up on high and caught
it; and this ball was her favorite plaything."
  "The text.")


(defun retain (object list &key (test #'eq) key pre-test)
  "Return a copy of LIST from which only OBJECT is retained.
Each item in LIST is TESTed with EQ by default. TEST is performed on the item
itself by default, or on the result of applying KEY to it. Optionally, only
items satisfying PRE-TEST are considered."
  (loop :for element :in list
	:when (and (or (not pre-test) (funcall pre-test element))
		   (funcall test
		     (if key (funcall key element) element)
		     object))
	  :collect element))


(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	  :nconc (list key val)))

(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	  :nconc (list key val)))


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
