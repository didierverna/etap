(in-package :etap)

(defclass hyphenation-rules ()
  ((patterns
    :initform (make-hash-table :test #'equal) :accessor patterns
    :documentation "The hyphenation patterns.
This is a hash table mapping radicals (strings) with a list of elements of the
form (INDEX . WEIGTH).")
   (exceptions
    :initform (make-hash-table :test #'equal) :accessor exceptions
    :documentation "The hyphenation exceptions.
This is a hash table mapping words (strings) with a list of hyphenation point
indexes."))
  (:documentation "The HYPHENATION-RULES class."))

(defun hyphenation-rules-p (object)
  "Return T if OBJECT is an HYPHENTATION-RULES."
  (typep object 'hyphenation-rules))

(defun make-hyphenation-rules ()
  "Create an empty instance of HYPHENATION-RULES."
  (make-instance 'hyphenation-rules))


(defun (setf hyphenation-pattern) (value string hyphenation-rules)
  "Set HYPHENATION-RULES'pattern for STRING to VALUE."
  (setf (gethash string (patterns hyphenation-rules)) value))

(defun (setf hyphenation-exception) (value string hyphenation-rules)
  "Set HYPHENATION-RULES'exception for STRING to VALUE."
  (setf (gethash string (exceptions hyphenation-rules)) value))


(defun hyphenation-pattern (string hyphenation-rules)
  "Return HYPHENATION-RULES'pattern for STRING.
Also return a second value indicating whether a pattern was found."
  (gethash string (patterns hyphenation-rules)))

(defun hyphenation-exception (string hyphenation-rules)
  "Return HYPHENATION-RULES'exception for STRING.
Also return a second value indicating whether an exception was found."
  (gethash string (exceptions hyphenation-rules)))

