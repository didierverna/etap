(in-package :etap)

(defun %convert (buffer)
  "Convert BUFFER string to a regular Lisp code string, and return it.
BUFFER is in \"string mode\". Characters are accumulated in strings, until
a backslash is encountered in which case a lisp expression is read and
printed, and string mode is set again."
  (with-input-from-string (stream buffer)
    (with-output-to-string (outstr)
      (loop :with in-string := nil
	    :for char := (read-char stream nil stream)
	    :until (eq char stream)
	    :if (eq char #\\)
	      :do (cond (in-string
			 (write-char #\" outstr)
			 (setq in-string nil))
			;; Be sure to separate Lisp expressions by at least a
			;; space, in case it is needed to syntactically
			;; separate them (e.g. a sequence of symbols).
			(t (write-char #\Space outstr)))
	      :and :do (prin1 (read-preserving-whitespace stream) outstr)
	    :else
	      :do (unless in-string
		    (write-char #\" outstr)
		    (setq in-string t))
	      :and :if (char= char #\") :do (write-char #\\ outstr) :end
	      :and :do (write-char char outstr)
	    :finally (when in-string (write-char #\" outstr)))
      outstr)))

(defvar *hlist*)

(defun load-buffer (buffer)
  (ignore-errors
   (with-input-from-string (s1 "(setq etap::*hlist* (etap::assemble-hlist ")
     (with-input-from-string (s2 (%convert buffer))
       (with-input-from-string (s3 "))")
	 (let ((s (make-concatenated-stream s1 s2 s3))
	       (*package* (find-package :etap-user))
	       (*print-readably* t)
	       (*print-pretty* nil))
	   (unwind-protect (load s :verbose nil :print nil)
	     (close s))))))))
