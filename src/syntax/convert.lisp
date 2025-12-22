(in-package :etap)

(defun %convert (instr)
  (with-input-from-string (stream instr)
    (with-output-to-string (outstr)
      (let ((*package* (find-package :etap-user))
	    (*print-readably* t)
	    (*print-pretty* nil))
	(loop :with in-string := nil
	      :for char := (read-char stream nil stream)
	      :until (eq char stream)
	      :if (eq char #\\)
		:do (progn (cond (in-string
				  (write-char #\" outstr)
				  (setq in-string nil))
				 ;; Be sure to separate Lisp expressions by at
				 ;; least a space, in case it is needed to
				 ;; syntactically separate them (e.g. a
				 ;; sequence of symbols).
				 (t (write-char #\Space outstr)))
			   (prin1 (read-preserving-whitespace stream) outstr))
	      :else
		:do (progn (unless in-string
			     (write-char #\" outstr)
			     (setq in-string t))
			   (write-char char outstr))
	      :finally (when in-string (write-char #\" outstr)))
	outstr))))

(defun convert (string)
  (with-input-from-string (s1 "(list ")
    (with-input-from-string (s2 (%convert string))
      (with-input-from-string (s3 ")")
	(let ((s (make-concatenated-stream s1 s2 s3)))
	  (prog1 (eval (read s)) (close s)))))))
