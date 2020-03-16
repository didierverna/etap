(in-package :etap)

(defun fixed-line-boundary
    (start lineup width variant prefer-overfull-lines)
  (loop :with underfull-boundary
	:with underfull-w
	:with fit-boundary
	:with overfull-boundary
	:with overfull-w
	;; #### NOTE: this works even the first time because at worst,
	;; NEXT-SEARCH is gonna be (length lineup) first, and NIL only
	;; afterwards.
	:for (end next-start next-search) := (next-break-position lineup start)
	  :then (next-break-position lineup next-search)
	:for w := (lineup-width lineup start end)
	:while (and next-search (not overfull-boundary))
	:if (< w width)
	  :do (setq underfull-boundary (list end next-start) underfull-w w)
	:else :if (= w width)
	  :do (setq fit-boundary (list end next-start))
	:else
	  :do (setq overfull-boundary (list end next-start) overfull-w w)
	:finally
	   (return
	     (cond (fit-boundary fit-boundary)
		   ((and underfull-w (not overfull-w)) underfull-boundary)
		   ((and overfull-w (not underfull-w)) overfull-boundary)
		   (t
		    (case variant
		      (:underfull
		       underfull-boundary)
		      (:overfull
		       overfull-boundary)
		      (:best
		       (cond ((< (- width underfull-w) (- overfull-w width))
			      underfull-boundary)
			     ((< (- overfull-w width) (- width underfull-w))
			      overfull-boundary)
			     (prefer-overfull-lines
			      overfull-boundary)
			     (t underfull-boundary)))))))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :fixed))
     &key (variant :underfull) prefer-overfull-lines)
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start)
	  := (fixed-line-boundary
	      start lineup width variant prefer-overfull-lines)
	:collect (create-line lineup start end)))
