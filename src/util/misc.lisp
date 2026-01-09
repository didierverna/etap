(in-package :etap)
(in-readtable :etap)

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

(defun compare (list1 list2)
  "Return the number of consecutive identical elements in LIST1 and LIST2."
  (loop :for elt1 :in list1 :for elt2 :in list2
	:while (eq elt1 elt2)
	:count t))

(Defun select-keys (keys &rest selected)
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


;; Based on public domain Alexandria / Quickutil version
(defmacro when-let (bindings &body body)
  "Execute BODY only when all BINDINGS are non-nil.
BINDINGS must be either a single binding of the form (VARIABLE VALUE),
or a list of such. VALUEs are computed sequentially in the specified order,
and then VARIABLEs are bound to the corresponding VALUEs. If all VALUEs are
non-nil, BODY is executed."
  (when (and (consp bindings) (symbolp (car bindings)))
    (setq bindings (list bindings)))
  (let ((variables (mapcar #'car bindings)))
    `(let ,bindings
       (when (and ,@variables)
	 ,@body))))


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




;; ==========================================================================
;; CLOS Utilities
;; ==========================================================================

;; --------------------
;; Portability wrappers
;; --------------------

(defmacro declare-valid-superclass (class superclass)
  "Validate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  `(defmethod validate-superclass ((class ,class) (superclass ,superclass))
     #+ecl (declare (ignore class superclass))
     t))

(defmacro declare-invalid-superclass (class superclass)
  "Invalidate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  `(defmethod validate-superclass ((class ,class) (superclass ,superclass))
     #+ecl (declare (ignore class superclass))
     nil))



;; -----------------
;; Singleton classes
;; -----------------

(defclass singleton-class (standard-class)
  ((instance :initform nil))
  (:documentation "The Singleton Class meta-class."))

(defmacro defsingleton (class super-classes slots &rest options)
  "Like DEFCLASS, but define a singleton class."
  (when (assoc :metaclass options)
    (error "Defining singleton class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
     (:metaclass singleton-class)))

(defmethod make-instance ((class singleton-class) &key)
  (or (slot-value class 'instance)
      (setf (slot-value class 'instance) (call-next-method))))

(declare-valid-superclass   singleton-class standard-class)
(declare-invalid-superclass standard-class singleton-class)



;; ----------------
;; Abstract classes
;; ----------------

(defclass abstract-class (standard-class)
  ()
  (:documentation "The Abstract Class meta-class."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
     (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

(declare-valid-superclass abstract-class standard-class)
(declare-valid-superclass standard-class abstract-class)
