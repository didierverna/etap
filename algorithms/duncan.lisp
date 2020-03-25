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

;; #### WARNING: this is currently a duplicate of the reporting code in
;; node.lisp.
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

;; #### FIXME: same as Barnett, and Fit Justified.
(defun duncan-create-line (lineup start stop width sloppy)
  (let ((scale (lineup-scale lineup start stop width)))
    (if scale
      (create-line lineup start stop
		   (cond (sloppy scale)
			 ((zerop scale) 0)
			 ((< scale 0) (max scale -1))
			 ((> scale 0) (min scale 1))))
      (create-line lineup start stop))))

(defun duncan-create-lines (lineup solution width sloppy)
  (mapcar (lambda (line)
	    (duncan-create-line lineup (car line) (cdr line) width sloppy))
    (duncan-lines solution)))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :duncan)) &key sloppy)
  (let* ((solutions
	   (mapcar (lambda (lines) (duncan-make-solution lineup width lines))
	     (root-node-lines (root-node lineup width))))
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
