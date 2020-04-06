(in-package :etap)


(defclass edge () ((node :initarg :node :reader node)))

(defstruct (node (:constructor make-node (boundary edges)))
  boundary edges)

(defun create-node (lineup width algorithm edge-type boundary hash)
  (or (gethash (stop boundary) hash)
      (setf (gethash (stop boundary) hash)
	    (if (= (stop boundary) (length lineup))
	      (make-node boundary nil)
	      (let ((next-boundaries (apply #'next-boundaries
				       lineup (next-start boundary) width
				       (algorithm-type algorithm)
				       (algorithm-options algorithm))))
		(make-node
		 boundary
		 (mapcar
		     (lambda (next-boundary)
		       (make-instance edge-type
			 :lineup lineup :width width
			 :start (next-start boundary)
			 :node (create-node lineup width algorithm edge-type
					    next-boundary hash)))
		   next-boundaries)))))))

(defun layouts-graph
    (lineup width algorithm
     &aux (edge-type
	   (intern (format nil "~A-EDGE" (algorithm-type algorithm)) :etap))
	  (next-boundaries (apply #'next-boundaries lineup 0 width
				  (algorithm-type algorithm)
				  (algorithm-options algorithm)))
	  (hash (make-hash-table)))
  (make-node
   nil
   (mapcar
       (lambda (next-boundary)
	 (make-instance edge-type
	   :lineup lineup :width width :start 0
	   :node (create-node lineup width algorithm edge-type
			      next-boundary hash)))
     next-boundaries)))


#+()(defstruct (solution
	    (:constructor make-solution (lines hyphens underfulls overfulls)))
  lines hyphens underfulls overfulls)

#+()(defun create-solution (lineup width lines)
  (loop :with hyphens := 0
	:with underfulls := 0
	:with overfulls := 0
	:for line :in lines
	:unless (word-stop-p lineup (cdr line))
	  :do (incf hyphens)
	;; #### WARNING: dirty trick to not count the last line as underfull!
	:when (and (< (cdr line) (length lineup))
		   (< (lineup-max-width lineup (car line) (cdr line)) width))
	  :do (incf underfulls)
	:when (> (lineup-min-width lineup (car line) (cdr line)) width)
	  :do (incf overfulls)
	:finally
	   (return
	     (make-solution lines hyphens underfulls overfulls))))

;; #### NOTE: with the defaults (default text, 284pt, all features), there are
;; #### 66576 paragraph solutions including going through under and overfull
;; #### lines (21096 without hyphenation). The raw tree of all such solutions
;; #### has 150860 nodes (48338 without hyphenation). However, once a line
;; #### stop has been decided, all possible solutions for the next lines
;; #### remain the same, however we reached that possible stop. This means
;; #### that there is a lot of room for re-using branches. And indeed, when
;; #### sharing nodes, we fall from 150860 to 98 (from 48338 to 83 without
;; #### hyphenation).

;; #### If we avoid preventive fulls, that is, if we include only under- and
;; #### overfull solutions when there is not fit, the number of paragraph
;; #### solutions falls to 37 (it actually raises up to 61 without
;; #### hyphenation, all mistfits). The raw tree of all such solutions has
;; #### only 109 nodes (192 without hyphenations). Once shared, the actual
;; #### number of nodes falls down to 30 (33 without hyphenation).
#+()(defun report-solutions
    (context
     &key (width (paragraph-width context)) preventive-fulls)
  (let* ((lineup (create-lineup context))
	 (solutions
	   (mapcar (lambda (lines) (create-solution lineup width lines))
	     (root-node-lines (root-node lineup width preventive-fulls))))
	 (length 0)
	 (fits 0)
	 (fits-hyphened (make-hash-table))
	 (misfits 0))
    (loop :for solution :in solutions
	  :do (incf length)
	  :if (and (zerop (solution-hyphens solution))
		   (zerop (solution-underfulls solution))
		   (zerop (solution-overfulls solution)))
	    :do (incf fits)
	  :else :if (and (zerop (solution-underfulls solution))
			 (zerop (solution-overfulls solution)))
	    :do (if (gethash (solution-hyphens solution) fits-hyphened)
		  (setf (gethash (solution-hyphens solution) fits-hyphened)
			(1+ (gethash (solution-hyphens solution)
				     fits-hyphened)))
		  (setf (gethash (solution-hyphens solution) fits-hyphened) 1))
	  :else
	    :do (incf misfits))
    (format t "~A solutions in total.~%
~A fit solutions without hyphens.~%"
      length fits)
    (maphash (lambda (key value)
	       (format t "~A fit solutions with ~A hyphen~:P.~%"
		 value key))
	     fits-hyphened)
    (format t "~A mistfit solutions.~%" misfits)))
