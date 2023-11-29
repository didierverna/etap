(in-package :etap)

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

(defun strnlcat (&rest strings)
  "Concatenate STRINGS, inserting newlines in between."
  (with-output-to-string (stream nil :element-type 'character)
    (loop :for remainder :on strings
	  :do (princ (car remainder) stream)
	  :when (cdr remainder) :do (terpri stream))))

(define-method-combination strnlcat
  :documentation "The STRNLCAT method combination."
  :operator strnlcat :identity-with-one-argument t)
