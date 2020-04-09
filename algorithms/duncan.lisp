;; This is the Duncan algorithm from: C.J. Duncan, J. Eve, L. Molyneux, E.S.
;; Page, and Margaret G. Robson, Printing Technology 7, 133-151 (1963).

;; I don't have the article, but the Knuth-Plass paper gives a description of
;; it. It searches for an acceptable breaking solution (that is, with
;; adjustment ratios, that I call lineup-scales <= 1 in abs), while minimizing
;; hyphenation. What I don't really know however is how it chooses the final
;; solution when there is several possibilities.

;; #### FIXME: I don't know if Duncan is restricted to the Justified
;; #### disposition, or if it does something for the ragged ones. Currently,
;; #### I'm just creating lines intended for justification, and putting them
;; #### back to normal spacing otherwise. Given what this algorithm does, it
;; #### results in many overfulls.


(in-package :etap)


(defclass duncan-edge (paragraph-edge)
  ((hyphen :initform 0 :accessor hyphen)
   (overfull :initform 0 :accessor overfull)
   (underfull :initform 0 :accessor underfull)))

(defmethod initialize-instance :after
    ((edge duncan-edge)
     &key lineup width start
     &aux (stop (stop (boundary (node edge)))))
  (unless (word-stop-p lineup stop)
    (setf (hyphen edge) 1))
  (cond ((< (lineup-max-width lineup start stop) width)
	 (setf (underfull edge) 1))
	((> (lineup-min-width lineup start stop) width)
	 (setf (overfull edge) 1))))

(defmethod next-boundaries (lineup start width (algorithm (eql :duncan)) &key)
  (loop :with underfull
	:with fits := (list)
	:with overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-start boundary))
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


(defclass duncan-layout (paragraph-layout)
  ((hyphens :initform 0 :accessor hyphens)
   (underfulls :initform 0 :accessor underfulls)
   (overfulls :initform 0 :accessor overfulls)))

(defmethod update-paragraph-layout ((layout duncan-layout) (edge duncan-edge))
  (incf (hyphens layout) (hyphen edge))
  (incf (underfulls layout) (underfull edge))
  (incf (overfulls layout) (overfull edge)))


(defun duncan-create-lines
    (lineup width disposition layout
     &aux (justified (eq (disposition-type disposition) :justified))
	  (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  (loop :for node :in (cdr (nodes layout))
	:and start := 0 :then (next-start (boundary node))
	:for stop := (stop (boundary node))
	:if justified
	  :collect (create-justified-line lineup start stop width sloppy)
	:else
	  :collect (create-line lineup start stop)))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :duncan)) &key)
  (let* ((graph (paragraph-graph lineup width :duncan))
	 (layouts (paragraph-layouts graph :duncan))
	 (perfects
	   (remove-if-not (lambda (layout)
			    (and (zerop (hyphens layout))
				 (zerop (underfulls layout))
				 (zerop (overfulls layout))))
			  layouts))
	 (hyphened
	   (remove-if-not (lambda (layout)
			    (and (not (zerop (hyphens layout)))
				 (zerop (underfulls layout))
				 (zerop (overfulls layout))))
			  layouts))
	 (misfits
	   (remove-if (lambda (layout)
			(and (zerop (underfulls layout))
			     (zerop (overfulls layout))))
		      layouts)))
    ;; #### FIXME: options to do better than just returning the first ones.
    (cond (perfects
	   (duncan-create-lines lineup width disposition (car perfects)))
	  (hyphened
	   (let ((minimum-hyphens (loop :for layout :in hyphened
					:minimize (hyphens layout))))
	     (duncan-create-lines
	      lineup width disposition
	      (find minimum-hyphens hyphened :key #'hyphens))))
	  (t
	   (let* ((minimum-fulls
		    (loop :for misfit :in misfits
			  :minimize (+ (underfulls misfit)
				       (overfulls misfit))))
		  (best-misfits
		    (remove-if-not (lambda (misfit)
				     (= (+ (underfulls misfit)
					   (overfulls misfit))
					minimum-fulls))
				   misfits))
		  (minimum-hyphens (loop :for misfit :in best-misfits
					 :minimize (hyphens misfit))))
	     (duncan-create-lines
	      lineup width disposition
	      (find minimum-hyphens best-misfits :key #'hyphens)))))))
