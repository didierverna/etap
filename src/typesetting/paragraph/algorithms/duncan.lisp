;; This is the Duncan algorithm from: C.J. Duncan, J. Eve, L. Molyneux, E.S.
;; Page, and Margaret G. Robson, Printing Technology 7, 133-151 (1963).

;; I don't have the article, but the Knuth-Plass paper gives a description of
;; it. It searches for an acceptable breaking solution (that is, with
;; adjustment ratios (what I call scales) <= 1 in abs), while minimizing
;; hyphenation. What I don't really know however is how it chooses the final
;; solution when there is several possibilities (hence a Fit-like
;; discriminating function to make a choice).

;; #### FIXME: I don't know if Duncan is restricted to the Justified
;; disposition, or if it does something for the ragged ones. Currently, I'm
;; just creating lines intended for justification, and putting them back to
;; normal spacing otherwise. Given what this algorithm does, it results in
;; many overfulls.


(in-package :etap)


;; =============
;; Specification
;; =============

(defparameter *duncan-discriminating-functions*
  '(:minimize-distance :minimize-scaling))



;; ====================
;; Graph Specialization
;; ====================

;; -----
;; Edges
;; -----

;; #### NOTE: we handle the weights below with extended arithmetic, but this
;; is in fact not necessary because they're only used with actual solutions
;; (hence, for which the scaling would be numerical).

(defclass duncan-edge (edge)
  ((fitness :documentation "This edge's fitness status.
Possible values are :underfull, :fit, and :overfull."
	    :reader fitness)
   (scale :documentation "This edge's scale."
	  :reader scale)
   (weight :documentation "This edge's weight.
The weight is computed according to the discriminating function."
	   :reader weight))
  (:documentation "The DUNCAN-EDGE class."))

(defmethod initialize-instance :after
    ((edge duncan-edge)
     &key lineup start width
	  (discriminating-function (car *duncan-discriminating-functions*)))
  "Initialize Duncan EDGE's properties."
  (multiple-value-bind (natural max min stretch shrink)
      (lineup-width lineup start (stop-idx (boundary (destination edge))))
    (setf (slot-value edge 'fitness)
	  (cond ((i< max width) :underfull)
		((> min width) :overfull)
		(t :fit)))
    (setf (slot-value edge 'scale) (scaling natural width stretch shrink))
    (setf (slot-value edge 'weight)
	  (ecase discriminating-function
	    (:minimize-distance (abs (- width natural)))
	    (:minimize-scaling (iabs (scale edge)))))))


;; -------
;; Layouts
;; -------

(defclass duncan-layout (layout)
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

(defmethod push-edge :after (edge (layout duncan-layout))
  "Update Duncan LAYOUT's properties after pushing EDGE to it."
  (when (hyphenp edge) (incf (hyphens layout)))
  (case (fitness edge)
    (:underfull (incf (underfulls layout)))
    (:overfull (incf (overfulls layout))))
  (setf (weight layout) (i+ (weight layout) (weight edge))))



;; =========
;; Algorithm
;; =========

(defclass duncan-line (line)
  ((weight :initarg :weight :reader weight
	   :documentation "This line's weight."))
  (:documentation "The Duncan line class.
This class keeps track of the line's weight."))

(defmethod line-properties strnlcat ((line duncan-line))
  "Return a string advertising LINE's weight."
  (format nil "Weight: ~A." (ifloat (weight line))))

(defun duncan-make-lines
    (lineup disposition layout
     &aux (overstretch
	   (cadr (member :overstretch (disposition-options disposition))))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  "Typeset LINEUP as a DISPOSITION paragraph with Duncan LAYOUT."
  (loop :for edge :in (edges layout)
	:and start := 0 :then (start-idx (boundary (destination edge)))
	:for stop := (stop-idx (boundary (destination edge)))
	:for scale = (scale edge)
	:if (eq (disposition-type disposition) :justified)
	  :collect (multiple-value-bind (theoretical effective)
		       (if (last-boundary-p (boundary (destination edge)))
			 ;; Justified last line: maybe shrink it but don't
			 ;; stretch it.
			 (actual-scales scale
			   :overshrink overshrink :stretch-tolerance 0)
			 ;; Justified regular line: make it fit.
			 (actual-scales scale
			   :overshrink overshrink :overstretch overstretch))
		     (make-instance 'duncan-line
		       :lineup lineup :start-idx start :stop-idx stop
		       :scale theoretical
		       :effective-scale effective
		       :weight (weight edge)))
	:else
	  ;; Other dispositions: just switch back to normal spacing.
	  :collect (make-instance 'line
		     :lineup lineup :start-idx start :stop-idx stop)))

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
  "Typeset LINEUP with the Duncan algorithm."
  (declare (ignore discriminating-function))
  (let* ((graph (make-graph lineup width
		  :edge-type `(duncan-edge ,@options) :fulls t))
	 (layouts (layouts graph 'duncan-layout)))
    (labels ((perfect (layout)
	       (and (zerop (hyphens layout))
		    (zerop (underfulls layout))
		    (zerop (overfulls layout))))
	     (hyphenated (layout)
	       (and (not (zerop (hyphens layout)))
		    (zerop (underfulls layout))
		    (zerop (overfulls layout))))
	     (misfit (layout)
	       (or (not (zerop (underfulls layout)))
		   (not (zerop (overfulls layout)))))
	     (better (l1 l2) ;; The Almighty Duncan Sorting Function!
	       ;; #### NOTE: no need for extended arithmetic when comparing
	       ;; weights below.
	       (or (and (perfect l1) (perfect l2) (< (weight l1) (weight l2)))
		   (and (perfect l1) (not (perfect l2)))
		   (and (hyphenated l1) (hyphenated l2)
			(= (hyphens l1) (hyphens l2))
			(< (weight l1) (weight l2)))
		   (and (hyphenated l1) (hyphenated l2)
			(< (hyphens l1) (hyphens l2)))
		   (and (hyphenated l1) (misfit l2))
		   (and (misfit l1) (misfit l2)
			(= (+ (underfulls l1) (overfulls l1))
			   (+ (underfulls l2) (overfulls l2)))
			(< (hyphens l1) (hyphens l2)))
		   (and (misfit l1) (misfit l2)
			(< (+ (underfulls l1) (overfulls l1))
			   (+ (underfulls l2) (overfulls l2)))))))
      (duncan-make-lines lineup disposition (car (sort layouts #'better))))))
