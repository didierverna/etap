(in-package :etap)

(defun hash-table-counts (hash)
  "Count the numbers of non-null (resp. null) entries in HASH table.
Return those as two values."
  (loop :with non-null := 0 :with null := 0
	:for value :being :the :hash-values :in hash
	:if value :do (incf non-null) :else :do (incf null)
	:finally (return (values non-null null))))

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

(defun compare (seq1 seq2)
  "Compare SEQ1 and SEQ2. Return the number of consecutive identical elements."
  (loop :with i := 0
	:for elt1 :in seq1 :for elt2 :in seq2
	:when (eq elt1 elt2) :do (incf i)
	:finally (return i)))

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
  "Concatenate STRINGS, inserting newlines in between.
Before concatenation, STRINGS is rid of null elements or empty strings."
  ;; This works both on NIL or "".
  (setq strings (remove-if (lambda (str) (zerop (length str))) strings))
  (with-output-to-string (stream nil :element-type 'character)
    (loop :for remainder :on strings
	  :do (princ (car remainder) stream)
	  :when (cdr remainder) :do (terpri stream))))

(define-method-combination strnlcat
  :documentation "The STRNLCAT method combination."
  :operator strnlcat :identity-with-one-argument t)


;; Calibers

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
     &key infinity (earmuffs t)
     &aux (earmuff (if earmuffs "*" ""))
	  (variable (intern (format nil "~A~A~A" earmuff name earmuff)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate variable according to the *PREFIX-NAME* caliber.
The variable's name is NAME or *NAME* depending on EARMUFFS (T by default).
- If variable is null, set it to the caliber's default.
- If variable is already properly calibrated, leave it be.
- If variable is out of bounds (large inequality), clamp it or set it to an
  infinity value of the same sign, according to INFINITY. INFINITY may be NIL
  (the default), T, :positive, or :negative."
  `(cond ((null ,variable)
	  (setq ,variable (caliber-default ,caliber)))
	 ((<= ,variable (caliber-min ,caliber))
	  (setq ,variable ,(if (member infinity '(t :negative))
			     -∞
			     `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if (member infinity '(t :positive))
			     +∞
			     `(caliber-max ,caliber))))))


