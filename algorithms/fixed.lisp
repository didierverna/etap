(in-package :etap)

(defun natural-line-end (start lineup width disposition)
  (loop :with underfull-end
	:with underfull-w
	:with fit-end
	:with overfull-end
	:with overfull-w
	:for i := (next-glue-position lineup start) :then ii
	:for ii := (when i (next-glue-position lineup (1+ i))) ; discard glue
	:for w := (lineup-width lineup start i) :then (when ww (+ w ww))
	:for ww := (when i (lineup-width lineup i ii))
	:while (and w (not overfull-end))
	:if (< w width)
	  :do (setq underfull-end i underfull-w w)
	:else :if (= w width)
		:do (setq fit-end i)
	:else :do (setq overfull-end i overfull-w w)
	:finally (return
		   (cond (fit-end
			  fit-end)
			 ((and underfull-w (not overfull-w))
			  underfull-end)
			 ((and overfull-w (not underfull-w))
			  overfull-end)
			 (t
			  (case disposition
			    ((:flush-left :centered :flush-right)
			     underfull-end)
			    (:justified
			     ;; <= means favor underfull lines over overfull
			     ;; ones in case if delta equality.
			     (if (<= (- width underfull-w)
				     (- overfull-w width))
			       underfull-end
			       overfull-end))))))))


(defmethod create-lines
    (lineup width disposition (algorithm (eql :fixed)) &key)
  (loop :for start := 0 :then (when end (1+ end)) ; discard glue
	:while start
	:for end := (natural-line-end start lineup width disposition)
	:collect (%create-line lineup start end 0)))
