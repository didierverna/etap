(in-package :etap)

(defparameter *lefthyphenmin* 2
  "The minimum number of characters before a potential hyphenation point.")

(defparameter *righthyphenmin* 3
  "The minimum number of characters after a potential hyphenation point.")

(defparameter *hyphenation-patterns-file*
  (asdf:system-relative-pathname :etap #p"share/hyph-en-us.pat.txt")
  "The US English hyphenation patterns file.")

(defparameter *hyphenation-exceptions-file*
  (asdf:system-relative-pathname :etap #p"share/hyph-en-us.hyp.txt")
  "The US English hyphenation exceptions file.")

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

(defun hyphenation-pattern (string hyphenation-rules)
  "Return HYPHENATION-RULES'pattern for STRING.
Also return a second value indicating whether a pattern was found."
  (gethash string (patterns hyphenation-rules)))

(defun (setf hyphenation-pattern) (value string hyphenation-rules)
  "Set HYPHENATION-RULES'pattern for STRING to VALUE."
  (setf (gethash string (patterns hyphenation-rules)) value))

(defun hyphenation-exception (string hyphenation-rules)
  "Return HYPHENATION-RULES'exception for STRING.
Also return a second value indicating whether an exception was found."
  (gethash string (exceptions hyphenation-rules)))

(defun (setf hyphenation-exception) (value string hyphenation-rules)
  "Set HYPHENATION-RULES'exception for STRING to VALUE."
  (setf (gethash string (exceptions hyphenation-rules)) value))

(defun parse-hyphenation-pattern (string)
  "Parse STRING as a hyphenation pattern.
Return two values: the radical and the list of weighted hyphenation points."
  (loop :with word := (make-string (- (length string)
				      (count-if #'digit-char-p string)))
	:with digit-is-char := nil
	:with hyphenation-points := (list)
	:with i := 0
	:for char :across string
	:if (digit-char-p char)
	  :if digit-is-char
	    :do (setf (aref word i) char)
	    :and :do (incf i)
	    :and :do (setq digit-is-char nil)
	  :else
	    :do (endpush (cons i (- (char-code char) 48)) hyphenation-points)
	    :and :do (setq digit-is-char t)
	  :end
	:else
	  :do (setf (aref word i) char)
	  :and :do (incf i)
	  :and :do (setq digit-is-char nil)
	:finally (return (values word hyphenation-points))))

(defun parse-hyphenation-exception (string)
  "Parse STRING as a hyphenation exception.
Return two values: the radical and the list of hyphenation points."
  (loop :with word := (make-string (- (length string) (count #\- string)))
	:with hyphenation-points := (list)
	:with i := 0
	:for char :across string
	:if (char= char #\-) :do (endpush i hyphenation-points)
	  :else :do (setf (aref word i) char) :and :do (incf i)
	:finally (return (values word hyphenation-points))))

(defun create-hyphenation-rules (&aux (rules (make-hyphenation-rules)))
  "Create a HYPHENATION-RULES instance.
Fill it with patterns and exceptions from the files specified by
*HYPHENATION-PATTERNS-FILE* and *HYPHENATION-EXCEPTIONS-FILE*."
  (with-open-file (stream *hyphenation-patterns-file*)
    (loop :for line := (read-line stream nil)
	  :while line
	  :do (multiple-value-bind (word pattern)
		  (parse-hyphenation-pattern line)
		(setf (hyphenation-pattern word rules) pattern))))
  (with-open-file (stream *hyphenation-exceptions-file*)
    (loop :for line := (read-line stream nil)
	  :while line
	  :do (multiple-value-bind (word pattern)
		  (parse-hyphenation-exception line)
		(setf (hyphenation-exception word rules) pattern))))
  rules)

(defun hyphenation-points (word rules)
  "Return a list of hyphenation points for WORD (a string) based on RULES."
  (multiple-value-bind (points found) (hyphenation-exception word rules)
    (if found
      points
      (loop :with word := (concatenate 'string '(#\.) word '(#\.))
	    :with length := (length word)
	    :with points := (list)
	    :for i :from 0 :upto (1- length)
	    :do (loop :for j :from (1+ i) :upto length
		      :for pattern
			:= (hyphenation-pattern (subseq word i j) rules)
		      :when pattern
			:do (mapc
				(lambda (elt)
				  (let* ((position (+ i (car elt) -1))
					 (value (cdr elt))
					 (point (member position points
							:key #'car)))
				    (if point
				      (when (> value (cdar point))
					(setf (cdar point) value))
				      (push (cons position value) points))))
			      pattern))
	    :finally (return
		       (sort
			   (remove-if (lambda (position)
				     (or (< position *lefthyphenmin*)
					 (> position
					    ;; #### WARNING: LENGTH is too
					    ;; large by 2 because it counts
					    ;; the two additional dots!
					    (- length *righthyphenmin* 2))))
				   (mapcar #'car
				     (remove-if-not #'oddp points :key #'cdr)))
			#'<))))))
