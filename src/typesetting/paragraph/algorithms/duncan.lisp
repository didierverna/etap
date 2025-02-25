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
     &key width min-width max-width target
     &aux (eopp (eopp boundary))
	  (fitness ;; #### NOTE: an underfull last line is actually a fit.
	   (cond (($< max-width target) (if eopp :fit :underfull))
		 ((>  min-width target) :overfull)
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
	    (:minimize-distance (abs (- target width)))
	    (:minimize-scaling ($abs (scale boundary)))))))

;; It's not worth it to advertise the boundary's fitness.
(defmethod properties strnlcat ((boundary duncan-boundary) &key)
  "Return a string advertising Duncan BOUNDARY's weight."
  (format nil "Weigth: ~A."
    ($float (weight boundary))))


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
			   :target width)
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
;; Lines
;; ==========================================================================

;; #### TODO: it's probably a bad idea to call the cumulative weight just
;; "weight".
(defclass duncan-line (line)
  ((weight
    :documentation "The cumulative weight so far in the layout."
    :initarg :weight :reader weight))
  (:documentation "The Duncan Line class."))

(defmethod properties strnlcat ((line duncan-line) &key)
  "Return a string advertising Duncan LINE's cumulative weight."
  (format nil "Cumulative weight: ~A." ($float (weight line))))

(defun duncan-make-justified-line
    (harray bol boundary weight overstretch overshrink
     &aux (scale (scale boundary)))
  "Duncan version of `make-line' for justified lines."
  (multiple-value-bind (theoretical effective)
      (if (eopp boundary)
	;; Justified last line: maybe shrink it but don't stretch it.
	(actual-scales scale :overshrink overshrink :stretch-tolerance 0)
	;; Justified regular line: make it fit.
	(actual-scales scale :overshrink overshrink :overstretch overstretch))
    (make-instance 'duncan-line
      :harray harray :bol bol :boundary boundary
      :scale theoretical
      :effective-scale effective
      :weight weight)))


(defclass duncan-pinned-line (duncan-line pin)
  ()
  (:documentation "The Duncan Pinned Line class."))




;; ==========================================================================
;; Layouts
;; ==========================================================================

;; #### NOTE: the layout's weight is in fact the weight of the last line.
(defclass duncan-layout (layout)
  ((pinned-line-class :initform 'duncan-pinned-line) ; slot override
   (weight
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
(defmethod properties strnlcat ((layout duncan-layout) &key)
  "Return a string advertising Duncan LAYOUT's weight."
  (format nil "Weight: ~A." ($float (weight layout))))

(defun duncan-make-layout
    (breakup path
     &aux (harray (harray breakup))
	  (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (disposition-options (disposition-options disposition))
	  (overstretch (getf disposition-options :overstretch))
	  (overshrink (getf disposition-options :overshrink))
	  (layout (make-instance 'duncan-layout :breakup breakup)))
  "Create a Duncan layout for BREAKUP from PATH."
  (with-slots (weight hyphens underfulls overfulls) layout
    (loop :with make-line := (case disposition-type
			       (:justified
				(lambda (harray bol boundary weight)
				  (duncan-make-justified-line
				   harray bol boundary weight
				   overstretch overshrink)))
			       (t  ;; just switch back to normal spacing.
				(lambda (harray bol boundary weight)
				  (make-instance 'duncan-line
				    :harray harray
				    :bol bol
				    :boundary boundary
				    :weight weight))))
	  :for bol := *bop* :then (break-point boundary)
	  :for boundary :in path
	  :do (setf weight ($+ weight (weight boundary)))
	  :when (hyphenated boundary) :do (incf hyphens)
	  :do (case (fitness boundary)
		(:underfull (incf underfulls))
		(:overfull  (incf overfulls)))
	  :collect (funcall make-line harray bol boundary weight) :into lines
	  :finally (setf (slot-value layout 'lines) lines)))
  layout)




;; ==========================================================================
;; Breakup
;; ==========================================================================

(defmethod break-harray
    (harray disposition width (algorithm (eql :duncan))
     &key ((:discriminating-function *discriminating-function*))
     &aux (breakup (make-instance 'graph-breakup
		     :harray harray :disposition disposition :width width)))
  "Break HARRAY with the Duncan algorithm."
  (default-duncan discriminating-function)
  (unless (zerop (length harray))
    ;; #### TODO: this is in fact not specific to Duncan but... here we avoid
    ;; preventive fulls, that is, we don't return *full boundaries if there is
    ;; at least one fit boundary. Experience shows that including preventive
    ;; fulls leads to an explosion of the graph size. On the other hand, maybe
    ;; it is possible that we miss better solutions like this. For example, it
    ;; could be possible that by making a line arbitrarily underfull instead
    ;; of fit, we reduce the number of subsequent *fulls. Perhaps this should
    ;; be experimented. On the other hand, if we get even just one full, the
    ;; solution is unacceptable in theory so it's probably not worth it.
    (setf (slot-value breakup 'graph)
	  (make-graph harray width #'duncan-get-boundaries))
    (let ((layouts (mapcar (lambda (path) (duncan-make-layout breakup path))
		     (make-graph-paths (graph breakup)))))
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
      (setf (slot-value breakup 'layouts)
	    (make-array (length layouts) :initial-contents layouts))))
  breakup)
