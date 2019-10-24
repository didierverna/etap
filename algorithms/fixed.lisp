(in-package :etap)

(defun fixed-line-end (start lineup width disposition prefer-overfull-lines)
  (loop :with underfull-i
	:with underfull-w
	:with fit-i
	:with overfull-i
	:with overfull-w
	:for i := (next-glue-position lineup start) :then ii
	:for ii := (when i (next-glue-position lineup (1+ i))) ; discard glue
	:for w := (lineup-width lineup start i) :then (when ww (+ w ww))
	:for ww := (when i (lineup-width lineup i ii))
	:while (and w (not overfull-i))
	:if (< w width)
	  :do (setq underfull-i i underfull-w w)
	:else :if (= w width)
		:do (setq fit-i i)
	:else :do (setq overfull-i i overfull-w w)
	:finally
	   (return
	     (cond (fit-i fit-i)
		   ((and underfull-w (not overfull-w)) underfull-i)
		   ((and overfull-w (not underfull-w)) overfull-i)
		   (t
		    (case disposition
		      ((:flush-left :centered :flush-right) underfull-i)
		      (:justified
		       (cond ((< (- width underfull-w) (- overfull-w width))
			      underfull-i)
			     ((< (- overfull-w width) (- width underfull-w))
			      overfull-i)
			     (prefer-overfull-lines overfull-i)
			     (t underfull-i)))))))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :fixed))
     &key prefer-overfull-lines)
  (loop :for start := 0 :then (when end (1+ end)) ; discard glue
	:while start
	:for end := (fixed-line-end start lineup width disposition
				    prefer-overfull-lines)
	:collect (create-line lineup start end)))
