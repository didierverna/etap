(in-package :etap)


;; =================
;; Hyphenation Files
;; =================

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

(defun load-hyphenation-rules
    (language
     &aux (radical (or (cdr (assoc language *languages*))
		       (error "No hyphenation files for ~A." language)))
	  (rules (make-hyphenation-rules)))
  "Load LANGUAGE's hyphenation rules. Return an HYPHENATION-RULES instance.
LANGUAGE must exist in `*LANGUAGES*', which see."
  (with-open-file
      (stream (asdf:system-relative-pathname :etap
		  (concatenate 'string "share/hyphenation/" radical ".pat")
		:type "txt"))
    (loop :for line := (read-line stream nil)
	  :while line
	  :do (multiple-value-bind (word pattern)
		  (parse-hyphenation-pattern line)
		(setf (hyphenation-pattern word rules) pattern))))
  (with-open-file
      (stream (asdf:system-relative-pathname :etap
		  (concatenate 'string "share/hyphenation/" radical ".hyp")
		:type "txt")
       :if-does-not-exist nil)
    (when (streamp stream)
      (loop :for line := (read-line stream nil)
	    :while line
	    :do (multiple-value-bind (word pattern)
		    (parse-hyphenation-exception line)
		  (setf (hyphenation-exception word rules) pattern)))))
  rules)



;; =================
;; Hyphenation Rules
;; =================

(defvar *hyphenation-rules* (make-hash-table)
  "A hash table mapping languages (keywords) to loaded hyphenation rules.")

(defun get-hyphenation-rules (language)
  "Get LANGUAGE's hyphenation rules, possibly loading them first.
Return an HYPHENATION-RULES instance."
  (or (gethash language *hyphenation-rules*)
      (setf (gethash language *hyphenation-rules*)
	    (load-hyphenation-rules language))))
