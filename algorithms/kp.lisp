;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

(define-constant +kp-variants+
    '(:graph :dynamic))

(define-constant +kp-variants-help-keys+
    '(:kp-variant-graph :kp-variant-dynamic))


(define-calibration +kp-line-penalty+ 0 10 100)
(define-calibration +kp-hyphen-penalty+ -10000 50 10000)
(define-calibration +kp-explicit-hyphen-penalty+ -10000 50 10000)
(define-calibration +kp-adjacent-demerits+ 0 10000 10000)
(define-calibration +kp-double-hyphen-demerits+ 0 10000 10000)
(define-calibration +kp-final-hyphen-demerits+ 0 5000 10000)
(define-calibration +kp-pre-tolerance+ -1 100 10000)
(define-calibration +kp-tolerance+ 0 200 10000)
(define-calibration +kp-emergency-stretch+ 0 0 20)
(define-calibration +kp-looseness+ -10 0 10)

(define-constant +kp-tooltips+
    '(:kp-variant-graph "Graph-based implementation."
      :kp-variant-dynamic "Dynamic programming implementation."))

(defmacro kp-calibrate (variable &optional infinity)
  `(calibrate-variable ,variable kp ,infinity))

(defclass kp-edge (paragraph-edge)
  ((hyphenp :accessor hyphenp)
   (fitness-class :accessor fitness-class)
   ;; #### NOTE: these are only line-local demerits.
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start line-penalty hyphen-penalty explicit-hyphen-penalty
     &allow-other-keys
     &aux (stop (stop (boundary (node edge))))
	  (hyphenp (not (word-stop-p lineup stop)))
	  (scale (lineup-scale lineup start stop width))
	  (badness (badness lineup start stop width))
	  (penalty (if hyphenp
		     (if (pre-break (aref lineup (1- stop)))
		       hyphen-penalty
		       explicit-hyphen-penalty)
		       0)))
  (assert (not (eq penalty :+infinity)))
  ;; #### WARNING: it is possible to get a rigid line here (scale = NIL), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf (hyphenp edge) hyphenp)
  (setf (fitness-class edge)
	(cond ((or (null scale) (< scale -1/2)) 3)
	      ((<= -1/2 scale 1/2) 2)
	      ((<= 1/2 scale 1) 1)
	      ((< 1 scale) 0)))
  (setf (demerits edge)
	(cond ((and (numberp penalty) (<= 0 penalty))
	       (!+ (!expt (!+ line-penalty badness) 2) (expt penalty 2)))
	      ((and (numberp penalty) (< penalty 0))
	       (!+ (!expt (!+ line-penalty badness) 2) (- (expt penalty 2))))
	      (t ;; :-infinity
	       (!expt (!+ line-penalty badness) 2)))))

(defmethod next-boundaries
    (lineup start width (algorithm (eql :kp))
     &key pass threshold
	  hyphen-penalty explicit-hyphen-penalty
	  emergency-stretch)
  (loop :with boundaries :with overfull :with emergency-boundary
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop boundary))
	:while (and boundary (not overfull))
	:for boundary-type
	  := (cond ((word-boundary-p lineup boundary)
		    :word)
		   ((pre-break (aref lineup (1- (stop boundary))))
		    :hyphen)
		   (t
		    :explicit-hyphen))
	:for min-width := (lineup-min-width lineup start (stop boundary))
	:when (or (eq boundary-type :word)
		  (and (> pass 1)
		       (or (and (eq boundary-type :hyphen)
				(!< hyphen-penalty :+infinity))
			   (and (eq boundary-type :explicit-hyphen)
				(!< explicit-hyphen-penalty :+infinity)))))
	  :if (> min-width width)
	    :do (setq overfull boundary)
	  :else :if (or (and (eq boundary-type :hyphen)
			     (eq hyphen-penalty :-infinity))
			(and (eq boundary-type :explicit-hyphen)
			     (eq explicit-hyphen-penalty :-infinity)))
	    :do (return (list boundary))
	  :else :if (!<= (badness lineup start (stop boundary) width
				  emergency-stretch)
			 threshold)
	    :do (push boundary boundaries)
	  :else
	    :do (setq emergency-boundary boundary)
	:finally (return (if boundaries
			   boundaries
			   ;; #### NOTE: we absolutely need to return
			   ;; something here even on pass 2, because the
			   ;; emergency stretch could be 0.
			   (when (> pass 1)
			     (if overfull (list overfull)
				 (list emergency-boundary)))))))


(defclass kp-layout (paragraph-layout)
  ((size :accessor size)
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((layout kp-layout)  &key &aux (edge (car (edges layout))))
  (setf (size layout) 1
	(demerits layout) (demerits edge)))

(defmethod update-paragraph-layout ((layout kp-layout) (edge kp-edge))
  (incf (size layout))
  (setf (demerits layout) (!+ (demerits layout) (demerits edge))))

(defun kp-postprocess-layout
    (layout adjacent-demerits double-hyphen-demerits final-hyphen-demerits)
  (when (> (length (edges layout)) 1)
    (loop :for edge1 :in (edges layout)
	  :for edge2 :in (cdr (edges layout))
	  :when (and (hyphenp edge1) (hyphenp edge2))
	    :do (setf (demerits layout)
		      (!+ (demerits layout) double-hyphen-demerits))
	  :when (> (abs (- (fitness-class edge1) (fitness-class edge2))) 1)
	    :do (setf (demerits layout)
		      (!+ (demerits layout) adjacent-demerits))))
  (when (hyphenp (nth (- (size layout) 2) (edges layout)))
    (setf (demerits layout) (!+ final-hyphen-demerits (demerits layout)))))

(defun kp-create-layout-lines
    (lineup width disposition layout
     &aux (justified (eq (disposition-type disposition) :justified))
	  (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  (loop :for edge :in (edges layout)
	:and start := 0 :then (next-start (boundary (node edge)))
	:for stop := (stop (boundary (node edge)))
	:if justified
	  :collect (create-justified-line lineup start stop width sloppy)
	:else
	  :collect (create-line lineup start stop)))

(defun kp-graph-create-lines
    (lineup width disposition
     line-penalty hyphen-penalty explicit-hyphen-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness)
  (let* ((graph (or (when (!<= 0 pre-tolerance)
		      (paragraph-graph lineup width :kp
			:pass 1 :threshold pre-tolerance
			:line-penalty line-penalty))
		    (paragraph-graph lineup width :kp
		      :pass 2 :threshold tolerance
		      :line-penalty line-penalty
		      :hyphen-penalty hyphen-penalty
		      :explicit-hyphen-penalty explicit-hyphen-penalty)))
	 (layouts (paragraph-layouts graph :kp)))
    (mapc (lambda (layout)
	    (kp-postprocess-layout layout
	      adjacent-demerits double-hyphen-demerits
	      final-hyphen-demerits))
      layouts)
    (setq layouts (sort layouts #'!< :key #'demerits))
    (when (and (not (zerop emergency-stretch))
	       (eql (demerits (car layouts)) :+infinity))
      (setq graph (paragraph-graph lineup width :kp
		    :pass 3 :threshold tolerance
		    :line-penalty line-penalty
		    :hyphen-penalty hyphen-penalty
		    :explicit-hyphen-penalty explicit-hyphen-penalty
		    :emergency-stretch emergency-stretch))
      (setq layouts (paragraph-layouts graph :kp))
      (mapc (lambda (layout)
	      (kp-postprocess-layout layout
		adjacent-demerits double-hyphen-demerits
		final-hyphen-demerits))
	layouts)
      (setq layouts (sort layouts #'!< :key #'demerits)))
    (unless (zerop looseness)
      (let ((ideal-size (+ (size (car layouts)) looseness)))
	(setq layouts (sort layouts (lambda (size1 size2)
				      (< (abs (- size1 ideal-size))
					 (abs (- size2 ideal-size))))
			    :key #'size))))
    (kp-create-layout-lines lineup width disposition (car layouts))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass))
     &key (variant (car +kp-variants+))
	  line-penalty hyphen-penalty explicit-hyphen-penalty
	  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
	  pre-tolerance tolerance emergency-stretch looseness)
  (kp-calibrate line-penalty)
  (kp-calibrate hyphen-penalty t)
  (kp-calibrate explicit-hyphen-penalty t)
  (kp-calibrate adjacent-demerits)
  (kp-calibrate double-hyphen-demerits)
  (kp-calibrate final-hyphen-demerits)
  (when (>= pre-tolerance (caddr +kp-pre-tolerance+))
    (setq pre-tolerance :+infinity))
  (cond ((>= tolerance (caddr +kp-tolerance+))
	 (setq tolerance :+infinity))
	((< tolerance (car +kp-tolerance+))
	 (setq tolerance (car +kp-tolerance+))))
  (kp-calibrate emergency-stretch)
  (kp-calibrate looseness)
  (ecase variant
    (:graph
     (kp-graph-create-lines lineup width disposition
       line-penalty hyphen-penalty explicit-hyphen-penalty
       adjacent-demerits double-hyphen-demerits final-hyphen-demerits
       pre-tolerance tolerance emergency-stretch looseness))
    (:dynamic
     )))
