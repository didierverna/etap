;; This is the Duncan algorithm from: C.J. Duncan, J. Eve, L. Molyneux, E.S.
;; Page, and Margaret G. Robson, Printing Technology 7, 133-151 (1963).

;; I don't have the article, but the Knuth-Plass paper gives a description of
;; it. It searches for an acceptable breaking solution (that is, with
;; adjustment ratios, that I call lineup-scales <= 1 in abs), while minimizing
;; hyphenation. What I don't really know however is how it chooses the final
;; solution when there is several possibilities (hence a Fit-like
;; discriminating function to make a choice).

;; #### FIXME: I don't know if Duncan is restricted to the Justified
;; disposition, or if it does something for the ragged ones. Currently, I'm
;; just creating lines intended for justification, and putting them back to
;; normal spacing otherwise. Given what this algorithm does, it results in
;; many overfulls.


(in-package :etap)


(defparameter *duncan-discriminating-functions*
  '(:minimize-distance :minimize-scaling))


(defclass duncan-edge (edge)
  ((hyphenp :documentation "Whether this edge is hyphenated."
	    :reader hyphenp)
   (fitness :documentation "This edge's fitness status.
Possible values are :underfull, :fit, and :overfull."
	    :reader fitness)
   (weight :documentation "This edge's weight.
The weight is computed according to the discriminating function."
	   :reader weight))
  (:documentation "The DUNCAN-EDGE class."))

(defmethod initialize-instance :after
    ((edge duncan-edge)
     &key lineup start width
	  (discriminating-function (car *duncan-discriminating-functions*))
     &allow-other-keys
     &aux (stop (stop-idx (boundary (destination edge)))))
  "Initialize Duncan EDGE's properties."
  (setf (slot-value edge 'hyphenp)
	(hyphenation-point-p (item (boundary (destination edge)))))
  (multiple-value-bind (natural max min stretch shrink)
      (lineup-width lineup start stop)
    (setf (slot-value edge 'fitness)
	  (cond ((< max width) :underfull)
		((> min width) :overfull)
		(t :fit)))
    (setf (slot-value edge 'weight)
	  (ecase discriminating-function
	    (:minimize-distance (abs (- width natural)))
	    (:minimize-scaling
	     (when (eq (fitness edge) :fit)
	       (abs (scaling natural width stretch shrink))))))))


(defclass duncan-layout (paragraph-layout)
  ((hyphens :documentation "This layout's number of hyphenated lines."
	    :accessor hyphens)
   (underfulls :documentation "This layout's number of underfull lines."
	       :accessor underfulls)
   (overfulls :documentation "This layout's number of overfull lines."
	      :accessor overfulls)
   (weight :documentation "This layout's weight."
	   :accessor weight))
  (:documentation "The DUNCAN-LAYOUT class."))

(defmethod initialize-instance :after ((layout duncan-layout) &key edge)
  "Initialize Duncan LAYOUT's properties."
  (setf (hyphens layout) (if (hyphenp edge) 1 0)
	(underfulls layout) (if (eq (fitness edge) :underfull) 1 0)
	(overfulls layout) (if (eq (fitness edge) :overfull) 1 0)
	(weight layout) (weight edge)))

(defmethod update-paragraph-layout
    ((layout duncan-layout) &aux (edge (first (edges layout))))
  (when (hyphenp edge) (incf (hyphens layout)))
  (case (fitness edge)
    (:underfull (incf (underfulls layout)))
    (:overfull (incf (overfulls layout))))
  (setf (weight layout)
	(when (and (weight layout) (weight edge))
	  (+ (weight layout) (weight edge)))))


(defun duncan-make-lines
    (lineup disposition width layout
     &aux (justified (eq (disposition-type disposition) :justified))
	  (overstretch
	   (cadr (member :overstretch (disposition-options disposition))))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  (loop :for edge :in (edges layout)
	:and start := 0 :then (start-idx (boundary (destination edge)))
	:for stop := (stop-idx (boundary (destination edge)))
	:if (and justified (last-boundary-p (boundary (destination edge))))
	  ;; Justified last line: maybe shrink it but don't stretch it.
	  :collect (let ((scale (lineup-scale lineup start stop width)))
		     (if (and scale (< scale 0))
		       (make-wide-line lineup start stop width nil overshrink)
		       (make-line lineup start stop)))
	:else :if justified
	  ;; Justified regular line: make it fit.
	  :collect (make-wide-line lineup start stop width
				   overstretch overshrink)
	:else
	  ;; Other dispositions: just switch back to normal spacing.
	  :collect (make-line lineup start stop)))

;; #### TODO: this is in fact not specific to Duncan but... here we avoid
;; preventive fulls, that is, we don't return *full boundaries if there is at
;; least one fit boundary. Experience shows that including preventive fulls
;; leads to an explosion of the graph size. On the other hand, maybe it is
;; possible that we miss better solutions like this. For example, it could be
;; possible that by making a line arbitrarily underfull instead of fit, we
;; reduce the number of subsequent *fulls. I hope that if it's possible, it
;; would only affect very rare cases. But this should be experimented.
(defmethod make-lines
    (lineup disposition width (algorithm (eql :duncan))
     &rest options &key discriminating-function)
  (declare (ignore discriminating-function))
  (let* ((graph (apply #'make-graph lineup width
		       :edge-type 'duncan-edge :fulls t
		       options))
	 (layouts (paragraph-layouts graph :duncan))
	 (perfects
	   (sort (remove-if-not (lambda (layout)
				  (and (zerop (hyphens layout))
				       (zerop (underfulls layout))
				       (zerop (overfulls layout))))
				layouts)
		 #'< :key #'weight))
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
    (cond (perfects
	   (duncan-make-lines lineup disposition width (car perfects)))
	  (hyphened
	   (let ((minimum-hyphens (loop :for layout :in hyphened
					:minimize (hyphens layout))))
	     (duncan-make-lines
	      lineup disposition width
	      (car (sort (remove-if-not
			  (lambda (hyphens) (= hyphens minimum-hyphens))
			  hyphened :key #'hyphens)
			 #'< :key #'weight)))))
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
	     (duncan-make-lines
	      lineup disposition width
	      (find minimum-hyphens best-misfits :key #'hyphens)))))))
