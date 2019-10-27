(in-package :etap)

(defconstant +lefthyphenmin+ 2)
(defconstant +righthyphenmin+ 3)

(defconstant +hyphenation-patterns-file+
  (asdf:system-relative-pathname :etap #p"share/hyph-en-us.pat.txt"))

(defconstant +hyphenation-exceptions-file+
  (asdf:system-relative-pathname :etap #p"share/hyph-en-us.hyp.txt"))

(defclass hyphenation-rules ()
  ((patterns
    :initform (make-hash-table :test #'equal) :accessor patterns)
   (exceptions
    :initform (make-hash-table :test #'equal) :accessor exceptions)))

(defun hyphenation-rules-p (object)
  (typep object 'hyphenation-rules))

(defun make-hyphenation-rules ()
  (make-instance 'hyphenation-rules))

(defun hyphenation-pattern (string hyphenation-rules)
  (gethash string (patterns hyphenation-rules)))

(defun (setf hyphenation-pattern) (value string hyphenation-rules)
  (setf (gethash string (patterns hyphenation-rules)) value))

(defun hyphenation-exception (string hyphenation-rules)
  (gethash string (exceptions hyphenation-rules)))

(defun (setf hyphenation-exception) (value string hyphenation-rules)
  (setf (gethash string (exceptions hyphenation-rules)) value))

(defun parse-hyphenation-pattern (string)
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
	    :do (push (cons i (- (char-code char) 48)) hyphenation-points)
	    :and :do (setq digit-is-char t)
	  :end
	:else
	  :do (setf (aref word i) char)
	  :and :do (incf i)
	  :and :do (setq digit-is-char nil)
	:finally (return (values word hyphenation-points))))

(defun parse-hyphenation-exception (string)
  (loop :with word := (make-string (- (length string) (count #\- string)))
	:with hyphenation-points := (list)
	:with i := 0
	:for char :across string
	:if (char= char #\-) :do (push i hyphenation-points)
	  :else :do (setf (aref word i) char) :and :do (incf i)
	:finally (return (values word hyphenation-points))))

(defun create-hyphenation-rules (&aux (rules (make-hyphenation-rules)))
  (with-open-file (stream +hyphenation-patterns-file+)
    (loop :for line := (read-line stream nil)
	  :while line
	  :do (multiple-value-bind (word pattern)
		  (parse-hyphenation-pattern line)
		(setf (hyphenation-pattern word rules) pattern))))
  (with-open-file (stream +hyphenation-exceptions-file+)
    (loop :for line := (read-line stream nil)
	  :while line
	  :do (multiple-value-bind (word pattern)
		  (parse-hyphenation-exception line)
		(setf (hyphenation-exception word rules) pattern))))
  rules)

(defun hyphenation-points (word rules)
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
				     (or (< position +lefthyphenmin+)
					 (> position
					    (- length +righthyphenmin+ 1))))
				   (mapcar #'car
				     (remove-if-not #'oddp points :key #'cdr)))
			#'<))))))
