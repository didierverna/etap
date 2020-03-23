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

(defstruct (duncan-node (:constructor make-duncan-node (boundary children)))
  boundary children)

(defun duncan-make-node (lineup boundary width)
  (when boundary
    (if (= (stop boundary) (length lineup))
      (make-duncan-node boundary nil)
      (multiple-value-bind (underfull-boundary fit-boundaries overfull-boundary)
	  (next-boundaries lineup (next-start boundary) width)
	(when underfull-boundary (push underfull-boundary fit-boundaries))
	(when overfull-boundary (push overfull-boundary fit-boundaries))
	(make-duncan-node
	 boundary
	 (mapcar (lambda (boundary) (duncan-make-node lineup boundary width))
	   fit-boundaries))))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :duncan)) &key sloppy)
  (let ((root-node
	  (multiple-value-bind
		(underfull-boundary fit-boundaries overfull-boundary)
	      (next-boundaries lineup 0 width)
	    (when underfull-boundary (push underfull-boundary fit-boundaries))
	    (when overfull-boundary (push overfull-boundary fit-boundaries))
	    (make-duncan-node
	     nil
	     (mapcar
		 (lambda (boundary) (duncan-make-node lineup boundary width))
	       fit-boundaries)))))
    root-node))
