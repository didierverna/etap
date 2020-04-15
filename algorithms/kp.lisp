;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

(define-constant +kp-variants+
    '(:graph :dynamic))

(define-constant +kp-variants-help-keys+
    '(:kp-variant-graph :kp-variant-dynamic))


(define-constant +kp-line-penalty+ '(0 10 100))
(define-constant +kp-hyphen-penalty+ '(-10000 50 10000))
(define-constant +kp-explicit-hyphen-penalty+ '(-10000 50 10000))
(define-constant +kp-adjacent-demerits+ '(0 10000 10000))
(define-constant +kp-double-hyphen-demerits+ '(0 10000 10000))
(define-constant +kp-final-hyphen-demerits+ '(0 5000 10000))
(define-constant +kp-pre-tolerance+ '(-1 100 10000))
(define-constant +kp-tolerance+ '(0 200 10000))
(define-constant +kp-emergency-stretch+ '(0 0 100))
(define-constant +kp-looseness+ '(-10 0 10))

(define-constant +kp-tooltips+
    '(:kp-variant-graph "Graph-based implementation."
      :kp-variant-dynamic "Dynamic programming implementation."))


(defclass kp-edge (paragraph-edge)
  ((hyphenp :accessor hyphenp)
   (fitness-class :accessor fitness-class)
   ;; #### NOTE: these are only line-local demerits.
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start line-penalty hyphen-penalty
     &allow-other-keys
     &aux (stop (stop (boundary (node edge))))
	  (hyphenp (not (word-stop-p lineup stop)))
	  (scale (lineup-scale lineup start stop width))
	  (badness (badness lineup start stop width))
	  (penalty (if hyphenp hyphen-penalty 0)))
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
     &key pass threshold hyphen-penalty)
  (loop :with boundaries :with overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop boundary))
	:while (and boundary (not overfull))
	:for min-width := (lineup-min-width lineup start (stop boundary))
	:when (or (word-boundary-p lineup boundary)
		  (and (> pass 1) (!< hyphen-penalty :+infinity)))
	  :if (> min-width width)
	    :do (setq overfull boundary)
	  :else :if (and (not (word-boundary-p lineup boundary))
			 (eq hyphen-penalty :-infinity))
	    :do (return (list boundary))
	  :else :if (!<= (badness lineup start (stop boundary) width)
			 threshold)
	    :do (push boundary boundaries)
	:finally (return (if boundaries
			   boundaries
			   (when (> pass 1)
			     ;; Because of the final paragraph glue, we
			     ;; necessarily have an overfull if we end up
			     ;; here.
			     (assert overfull)
			     (list overfull))))))


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

(defun kp-postprocess-layout (layout lineup final-hyphen-demerits)
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

(defgeneric kp-create-lines
    (lineup width disposition variant &key &allow-other-keys)
  (:method (lineup width disposition (variant (eql :graph))
	    &key (line-penalty (cadr +kp-line-penalty+))
		 (hyphen-penalty (cadr +kp-hyphen-penalty+))
		 (final-hyphen-demerits (cadr +kp-final-hyphen-demerits+))
		 (pre-tolerance (cadr +kp-pre-tolerance+))
		 (tolerance (cadr +kp-tolerance+)))
    (cond ((< line-penalty (car +kp-line-penalty+))
	   (setq line-penalty (car +kp-line-penalty+)))
	  ((> line-penalty (caddr +kp-line-penalty+))
	   (setq line-penalty (caddr +kp-line-penalty+))))
    (cond ((<= hyphen-penalty (car +kp-hyphen-penalty+))
	   (setq hyphen-penalty :-infinity))
	  ((>= hyphen-penalty (caddr +kp-hyphen-penalty+))
	   (setq hyphen-penalty :+infinity)))
    (when (>= pre-tolerance (caddr +kp-pre-tolerance+))
      (setq pre-tolerance :+infinity))
    (cond ((>= tolerance (caddr +kp-tolerance+))
	   (setq tolerance :+infinity))
	  ((< tolerance (car +kp-tolerance+))
	   (setq tolerance (car +kp-tolerance+))))
    (let ((graph (or (when (!<= 0 pre-tolerance)
		       (paragraph-graph lineup width :kp
			 :pass 1 :threshold pre-tolerance
			 :line-penalty line-penalty))
		     (paragraph-graph lineup width :kp
		       :pass 2 :threshold tolerance
		       :line-penalty line-penalty
		       :hyphen-penalty hyphen-penalty)))
	  layouts)
      (when graph
	(setq layouts (paragraph-layouts graph :kp))
	(mapc (lambda (layout)
		(kp-postprocess-layout layout lineup final-hyphen-demerits))
	  layouts)
	(setq layouts (sort layouts #'!< :key #'demerits))
	(kp-create-layout-lines lineup width disposition (car layouts)))))
  (:method (lineup width disposition (variant (eql :dynamic)) &key)
    ))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass))
     &rest options &key (variant (car +kp-variants+)))
  (apply #'kp-create-lines lineup width disposition variant options))
