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
;; Boundaries
;; ==========================================================================

(defclass duncan-boundary (fit-boundary)
  ((fitness
    :documentation "This boundary's fitness status.
Possible values are :underfull, :fit, and :overfull."
    :reader fitness)
   (weight
    :documentation "This boundary's weight.
The weight is computed according to the discriminating function."
    :reader weight))
  (:documentation "The DUNCAN-BOUNDARY class."))

(defmethod initialize-instance :after
    ((boundary duncan-boundary)
     ;; #### NOTE: the slots from the superclasses are already initialized by
     ;; now, but we're still saving some reader calls by using the propagated
     ;; keyword arguments.
     &key natural-width min-width max-width width
     &aux (eopp (eopp (break-point boundary)))
	  (fitness ;; #### NOTE: an underfull last line is actually a fit.
	   (cond (($< max-width width) (if eopp :fit :underfull))
		 ((>  min-width width) :overfull)
		 (t :fit))))
  "Initialize BOUNDARY's fitness and weight."
  (setf (slot-value boundary 'fitness) fitness)
  ;; #### NOTE: a fit last line (which, in fact, can be underfull as mentioned
  ;; above) needs a special treatment here. We won't consider its weight at
  ;; all because it's not justified. On the other hand, if the last line is
  ;; overfull, then it's bad and we need to take its weight into account.
  (setf (slot-value boundary 'weight)
	(if (and eopp (eq fitness :fit))
	  0
	  (ecase *discriminating-function*
	    (:minimize-distance (abs (- width natural-width)))
	    (:minimize-scaling ($abs (scale boundary)))))))


;; -----------------
;; Boundaries lookup
;; -----------------

;; #### TODO: for experimentation, we could make PREVENTIVE-*FULLS a number
;; instead of just a Boolean, for keeping more than 1 *full. On the other
;; hand, this function (which was originally a general utility) does already
;; too much as Duncan only uses the default keyword values.

(defun duncan-get-boundaries (harray bol width &key (fulls t) strict)
  "Get boundaries for an HARRAY line of WIDTH beginning at BOL.
This is the Duncan algorithm version.

If no boundary is found, return NIL, unless FULLS (the default), in which case
return the last underfull and the first overfull (if any) as a fallback
solution. If FULLS is :PREVENTIVE, also return these fulls even if acceptable
boundaries are found.

If STRICT, consider that even the last line must fit exactly. Otherwise
(the default), consider a final underfull as a fit.

The possible endings are listed in reverse order (from last to first)."
  (loop :with underfull :with fits := (list) :with overfull
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol (not overfull))
	:for boundary := (make-instance 'duncan-boundary
			   :harray harray :bol bol :break-point eol
			   :width width)
	:do (cond (($< (max-width boundary) width)
		   (if (and (eopp eol) (not strict))
		     (push boundary fits)
		     (setq underfull boundary)))
		  ((> (min-width boundary) width)
		   (setq overfull boundary))
		  (t ;; note the reverse order
		   (push boundary fits)))
	:finally
	   (return (cond ((eq fulls :preventive)
			  (append (when overfull (list overfull))
				  fits
				  (when underfull (list underfull))))
			 (fulls
			  (or fits
			      (append (when overfull (list overfull))
				      (when underfull (list underfull)))))
			 (t fits)))))




;; ==========================================================================
;; Layouts
;; ==========================================================================

;; ------
;; Ledges
;; ------

(defclass duncan-ledge (ledge)
  ((weight :documentation "The cumulative weight so far in the layout."
	   :initarg :weight :reader weight))
  (:documentation "The Duncan Ledge class."))

(defmethod properties strnlcat ((ledge duncan-ledge))
  "Advertise Duncan LEDGE properties."
  (format nil "Weights: ~A (line), ~A (cumulative)."
    ($float (weight (boundary ledge)))
    ($float (weight ledge))))


;; -------
;; Layouts
;; -------

;; #### NOTE: the layout's weight is in fact the weight of the last ledge.
(defclass duncan-layout (layout)
  ((weight
    :documentation "This layout's total weight."
    :initform 0 :reader weight)
   (hyphens
    :documentation "This layout's number of hyphenated lines."
    :initform 0 :reader hyphens)
   (underfulls
    :documentation "This layout's number of underfull lines."
    :initform 0 :reader underfulls)
   (overfulls
    :documentation "This layout's number of overfull lines."
    :initform 0 :reader overfulls))
  (:documentation "The DUNCAN-LAYOUT class."))

;; #### NOTE: we only advertise the layout's weight for now. The other
;; properties are here for sorting the layouts from best to worse.
(defmethod properties strnlcat ((layout duncan-layout))
  "Advertise Duncan LAYOUT properties."
  (format nil "Weight: ~A." ($float (weight layout))))

(defun duncan-postprocess-layout (layout)
  "Compute LAYOUT's properties."
  (change-class layout 'duncan-layout)
  (with-slots (weight hyphens underfulls overfulls) layout
    (loop :for ledge :in (ledges layout)
	  :do (setf weight ($+ weight (weight (boundary ledge))))
	  :do (change-class ledge 'duncan-ledge :weight weight)
	  :when (hyphenated ledge) :do (incf hyphens)
	    :do (case (fitness (boundary ledge))
		  (:underfull (incf underfulls))
		  (:overfull  (incf overfulls))))))




;; ==========================================================================
;; Lines
;; ==========================================================================

(defun duncan-make-justified-line
    (harray bol ledge beds overstretch overshrink
     &aux (boundary (boundary ledge))
	  (scale (scale boundary)))
  "Duncan version of `make-ledge-line' for justified lines."
  (multiple-value-bind (theoretical effective)
      (if (eopp (break-point boundary))
	;; Justified last line: maybe shrink it but don't stretch it.
	(actual-scales scale :overshrink overshrink :stretch-tolerance 0)
	;; Justified regular line: make it fit.
	(actual-scales scale :overshrink overshrink :overstretch overstretch))
    (make-ledge-line harray bol ledge beds
      :scale theoretical
      :effective-scale effective)))




;; ==========================================================================
;; Breakup
;; ==========================================================================

(defmethod break-harray
    (harray disposition width beds (algorithm (eql :duncan))
     &key ((:discriminating-function *discriminating-function*)))
  "Break HARRAY with the Duncan algorithm."
  (default-duncan discriminating-function)
  (if (zerop (length harray))
    (make-instance 'graph-breakup :disposition disposition :width width)
    ;; #### TODO: this is in fact not specific to Duncan but... here we avoid
    ;; preventive fulls, that is, we don't return *full boundaries if there is
    ;; at least one fit boundary. Experience shows that including preventive
    ;; fulls leads to an explosion of the graph size. On the other hand, maybe
    ;; it is possible that we miss better solutions like this. For example, it
    ;; could be possible that by making a line arbitrarily underfull instead
    ;; of fit, we reduce the number of subsequent *fulls. Perhaps this should
    ;; be experimented. On the other hand, if we get even just one full, the
    ;; solution is unacceptable in theory so it's probably not worth it.
    (let* ((graph (make-graph harray width #'duncan-get-boundaries))
	   (layouts (mapc #'duncan-postprocess-layout (make-layouts graph)))
	   breakup)
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
		      :disposition disposition :width width
		      :graph graph :layouts layouts))
      (unless (zerop (length (layouts breakup))) ; not happening in Duncan
	(let ((disposition-type (disposition-type disposition))
	      (overstretch (getf (disposition-options disposition)
				 :overstretch))
	      (overshrink (getf (disposition-options disposition)
				:overshrink)))
	  (setf (aref (renditions breakup) 0)
		(pin-lines
		 (make-layout-lines
		  harray beds (aref (layouts breakup) 0)
		  (case disposition-type
		    (:justified
		     (lambda (harray bol ledge beds)
		       (duncan-make-justified-line harray bol ledge beds
						   overstretch overshrink)))
		    (t  ;; just switch back to normal spacing.
		     (lambda (harray bol ledge beds)
		       (make-line harray bol (boundary ledge) beds)))))
		 disposition-type
		 width))))
      breakup)))
