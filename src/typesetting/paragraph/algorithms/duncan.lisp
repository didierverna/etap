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

;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *duncan-discriminating-functions*
  '(:minimize-distance :minimize-scaling))

(defvar *discriminating-function*)

(defmacro default-duncan (name)
  "Default Duncan NAMEd variable."
  `(default duncan ,name))




;; ==========================================================================
;; Graph Specialization
;; ==========================================================================

;; -----
;; Edges
;; -----

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
     &key harray start width
     &aux (last-line-p (last-boundary-p (boundary (destination edge)))))
  "Initialize Duncan EDGE's properties."
  (multiple-value-bind (natural max min stretch shrink)
      (harray-width harray start (stop-idx (boundary (destination edge))))
    (setf (slot-value edge 'fitness)
	  ;; #### NOTE: an underfull last line is actually a fit.
	  (cond (($< max width) (if last-line-p :fit :underfull))
		((> min width) :overfull)
		(t :fit)))
    (setf (slot-value edge 'scale) (scaling natural width stretch shrink))
    ;; #### NOTE: a fit last line (which, in fact, can be underfull as
    ;; mentioned above) needs a special treatment here. We won't consider its
    ;; weight at all because it's not justified. On the other hand, if the
    ;; last line is overfull, then it's bad and we need to take its weight
    ;; into account.
    (setf (slot-value edge 'weight)
	  (if (and last-line-p (eq (fitness edge) :fit))
	    0
	    (ecase *discriminating-function*
	      (:minimize-distance (abs (- width natural)))
	      (:minimize-scaling ($abs (scale edge))))))))


;; ------
;; Ledges
;; ------

(defclass duncan-ledge (ledge)
  ((weight :documentation "The total weight so far in the layout."
	   :reader weight))
  (:documentation "The Duncan Ledge class."))

(defmethod properties strnlcat ((ledge duncan-ledge))
  "Advertise Duncan LEDGE properties."
  (format nil "Weights: ~A (line), ~A (cumulative)."
    ($float (weight (edge ledge)))
    ($float (weight ledge))))


;; -------
;; Layouts
;; -------


;; #### TODO: I'm keeping this around because those are properties we don't
;; want to advertise, contrary to cumulative weights: the layout's total
;; weight is in fact the weight of the last ledge. But I haven't given this a
;; lot of thought yet.
(defclass duncan-layout (layout)
  ((hyphens :documentation "This layout's number of hyphenated lines."
	    :initform 0 :reader hyphens)
   (underfulls :documentation "This layout's number of underfull lines."
	       :initform 0 :reader underfulls)
   (overfulls :documentation "This layout's number of overfull lines."
	      :initform 0 :reader overfulls))
  (:documentation "The DUNCAN-LAYOUT class."))

(defmethod weight ((layout duncan-layout))
  "Return Duncan LAYOUT's weight."
  (weight (car (last (ledges layout)))))

(defmethod properties strnlcat ((layout duncan-layout))
  "Advertise Duncan LAYOUT properties."
  (format nil "Weight: ~A." ($float (weight layout))))

(defun duncan-postprocess-layout (layout)
  "Compute LAYOUT's properties."
  (loop :with total-weight := 0
	:for ledge :in (ledges layout)
	:do (setq total-weight ($+ total-weight (weight (edge ledge))))
	:do (setf (slot-value ledge 'weight) total-weight)
	:when (hyphenated ledge)
	  :do (incf (slot-value layout 'hyphens))
	:do (case (fitness (edge ledge))
	      (:underfull (incf (slot-value layout 'underfulls)))
	      (:overfull  (incf (slot-value layout 'overfulls))))))




;; ==========================================================================
;; Lines
;; ==========================================================================

(defun duncan-pin-layout (harray disposition width beds layout)
  "Pin Duncan LAYOUT from HARRAY for a DISPOSITION paragraph."
  (when layout
    (loop :with disposition-options := (disposition-options disposition)
	  :with overstretch := (getf disposition-options :overstretch)
	  :with overshrink := (getf disposition-options :overshrink)
	  :with disposition := (disposition-type disposition)
	  :with baseline-skip := (baseline-skip harray)
	  :for y := 0 :then (+ y baseline-skip)
	  :for ledge :in (ledges layout)
	  :for edge := (edge ledge)
	  :and start := 0 :then (start-idx (boundary (destination edge)))
	  :for stop := (stop-idx (boundary (destination edge)))
	  :for scale = (scale edge)
	  :for line := (case disposition
			 (:justified
			  (multiple-value-bind (theoretical effective)
			      (if (last-boundary-p
				   (boundary (destination edge)))
				;; Justified last line: maybe shrink it but
				;; don't stretch it.
				(actual-scales scale
				  :overshrink overshrink :stretch-tolerance 0)
				;; Justified regular line: make it fit.
				(actual-scales scale
				  :overshrink overshrink
				  :overstretch overstretch))
			    (make-instance 'graph-line
			      :harray harray :start-idx start :stop-idx stop
			      :beds beds
			      :scale theoretical
			      :effective-scale effective
			      :ledge ledge)))
			 (t ;; just switch back to normal spacing.
			  (make-instance 'line
			    :harray harray :start-idx start :stop-idx stop
			    :beds beds)))
	  :for x := (case disposition
		      ((:flush-left :justified) 0)
		      (:centered (/ (- width (width line)) 2))
		      (:flush-right (- width (width line))))
	  :collect (pin-line line x y))))




;; ==========================================================================
;; Breakup
;; ==========================================================================

(defmethod break-harray
    (harray disposition width beds (algorithm (eql :duncan))
     &key ((:discriminating-function *discriminating-function*)))
  "Break HARRAY with the Duncan algorithm."
  (default-duncan discriminating-function)
  (if (zerop (length harray))
    (make-instance 'graph-breakup :width width)
    ;; #### TODO: this is in fact not specific to Duncan but... here we avoid
    ;; preventive fulls, that is, we don't return *full boundaries if there is
    ;; at least one fit boundary. Experience shows that including preventive
    ;; fulls leads to an explosion of the graph size. On the other hand, maybe
    ;; it is possible that we miss better solutions like this. For example, it
    ;; could be possible that by making a line arbitrarily underfull instead
    ;; of fit, we reduce the number of subsequent *fulls. I hope that if it's
    ;; possible, it would only affect very rare cases. But this should be
    ;; experimented.
    (multiple-value-bind (root nodes)
	(make-graph harray width
	  :edge-type 'duncan-edge :next-boundaries '(next-boundaries :fulls t))
      (let ((layouts (make-layouts root
		       :layout-type 'duncan-layout :ledge-type 'duncan-ledge))
	    breakup)
	(mapc #'duncan-postprocess-layout layouts)
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
		   ;; #### NOTE: no need for extended arithmetic when
		   ;; comparing weights below.
		   (or (and (perfect l1)
			    (perfect l2)
			    (< (weight l1) (weight l2)))
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
	  (setq layouts (sort layouts #'better)))
	(setq breakup (make-instance 'graph-breakup
			:width width :root root :nodes nodes :layouts layouts))
	(unless (zerop (length layouts))
	  (setf (aref (renditions breakup) 0)
		(duncan-pin-layout harray disposition width beds
				   (first layouts))))
	breakup))))
