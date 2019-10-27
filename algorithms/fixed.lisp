(in-package :etap)

(defun fixed-line-boundaries
    (start lineup width disposition prefer-overfull-lines)
  (loop :with underfull-boundaries
	:with underfull-w
	:with fit-boundaries
	:with overfull-boundaries
	:with overfull-w
	;; #### NOTE: this works even the first time because at worst,
	;; NEXT-SEARCH is gonna be (length lineup) first, and NIL only
	;; afterwards.
	:for (end next-start next-search) := (next-break-position lineup start)
	  :then (next-break-position lineup next-search)
	:for w := (lineup-width lineup start end)
	:while (and next-search (not overfull-boundaries))
	:if (< w width)
	  :do (setq underfull-boundaries (list end next-start) underfull-w w)
	:else :if (= w width)
	  :do (setq fit-boundaries (list end next-start))
	:else
	  :do (setq overfull-boundaries (list end next-start) overfull-w w)
	:finally
	   (return
	     (cond (fit-boundaries fit-boundaries)
		   ((and underfull-w (not overfull-w)) underfull-boundaries)
		   ((and overfull-w (not underfull-w)) overfull-boundaries)
		   (t
		    (case disposition
		      ((:flush-left :centered :flush-right)
		       underfull-boundaries)
		      (:justified
		       (cond ((< (- width underfull-w) (- overfull-w width))
			      underfull-boundaries)
			     ((< (- overfull-w width) (- width underfull-w))
			      overfull-boundaries)
			     (prefer-overfull-lines
			      overfull-boundaries)
			     (t underfull-boundaries)))))))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :fixed))
     &key prefer-overfull-lines)
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start)
	  := (fixed-line-boundaries start lineup width disposition
				    prefer-overfull-lines)
	:collect (create-line lineup start end)))
