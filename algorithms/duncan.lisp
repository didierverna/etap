;; This is the Duncan algorithm from: C.J. Duncan, J. Eve, L. Molyneux, E.S.
;; Page, and Margaret G. Robson, Printing Technology 7, 133-151 (1963).

;; I don't have the article, but the Knuth-Plass paper gives a description of
;; it. It searches for an acceptable breaking solution (that is, with
;; adjustment ratios, that I call lineup-scales <= 1 in abs), while minimizing
;; hyphenation. What I don't really know however is how it chooses the final
;; solution when there is several possibilities.

;; #### FIXME: I don't know if Duncan is restricted to the Justified
;; #### disposition, or if it does something for the ragged ones.


(in-package :etap)


(defclass duncan-child (child) ())

(defmethod next-boundaries (lineup start width (algorithm (eql :duncan)) &key)
  (loop :with underfull
	:with fits := (list)
	:with overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-search boundary))
	:while (and boundary (not overfull))
	:for span := (lineup-span lineup start (stop boundary))
	:if (< (max-width span) width)
	  :do (setq underfull boundary)
	:else :if (and (<= (min-width span) width)
		       (>= (max-width span) width))
	  :do (push boundary fits)
	:else
	  :do (setq overfull boundary)
	:finally
	   ;; #### WARNING: here we avoid preventive fulls, that is, we don't
	   ;; return *full boundaries if there is at least one fit boundary.
	   ;; Experience shows that including preventive fulls leads to
	   ;; an explosion of the graph size. On the other hand, maybe it is
	   ;; possible that we miss better solutions like this. For example,
	   ;; it could be possible that by making a line arbitrarily underfull
	   ;; instead of fit, we reduce the number of subsequent *fulls. I
	   ;; hope that if it's possible, it would only affect very rare
	   ;; cases.
	   (return (cond (fits fits)
			 ((and underfull overfull) (list overfull underfull))
			 (overfull (list overfull))
			 (underfull (list underfull))))))


(defstruct
    (duncan-solution
     (:conc-name duncan-)
     (:constructor make-duncan-solution (lines hyphens underfulls overfulls)))
  lines hyphens underfulls overfulls)

(defun duncan-make-solution (lineup width lines)
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
	     (make-duncan-solution lines hyphens underfulls overfulls))))

(defun duncan-create-lines (lineup solution width sloppy)
  (mapcar (lambda (line)
	    (create-justified-line lineup (car line) (cdr line) width sloppy))
    (duncan-lines solution)))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :duncan))
     &key
     &aux (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  (let* ((root-node (create-root-node lineup width :duncan))
	 (root-lines (root-node-lines root-node))
	 (solutions (mapcar (lambda (lines)
			      (duncan-make-solution lineup width lines))
		      root-lines))
	 (perfects
	   (remove-if-not (lambda (solution)
			    (and (zerop (duncan-hyphens solution))
				 (zerop (duncan-underfulls solution))
				 (zerop (duncan-overfulls solution))))
			  solutions))
	 (hyphened
	   (remove-if-not (lambda (solution)
			    (and (not (zerop (duncan-hyphens solution)))
				 (zerop (duncan-underfulls solution))
				 (zerop (duncan-overfulls solution))))
			  solutions))
	 (misfits
	   (remove-if (lambda (solution)
			(and (zerop (duncan-underfulls solution))
			     (zerop (duncan-overfulls solution))))
		      solutions)))
    ;; #### FIXME: options to do better than just returning the first ones.
    (cond (perfects
	   (duncan-create-lines lineup (car perfects) width sloppy))
	  (hyphened
	   (let ((minimum-hyphens (loop :for solution :in hyphened
					:minimize (duncan-hyphens solution))))
	     (duncan-create-lines lineup (find minimum-hyphens hyphened
					       :key #'duncan-hyphens)
				  width sloppy)))
	  (t
	   (let* ((minimum-fulls
		    (loop :for misfit :in misfits
			  :minimize (+ (duncan-underfulls misfit)
				       (duncan-overfulls misfit))))
		  (best-misfits
		    (remove-if-not (lambda (misfit)
				     (= (+ (duncan-underfulls misfit)
					   (duncan-overfulls misfit))
					minimum-fulls))
				   misfits))
		  (minimum-hyphens (loop :for misfit :in best-misfits
					 :minimize (duncan-hyphens misfit))))
	     (duncan-create-lines lineup (find minimum-hyphens best-misfits
					       :key #'duncan-hyphens)
				  width sloppy))))))
