;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

(define-constant +kp-variants+
    '(:graph :dynamic))

(define-constant +kp-variants-help-keys+
    '(:kp-variant-graph :kp-variant-dynamic))


(define-constant +kp-line-penalty+ '(0 10 100))
(define-constant +kp-hyphen-penalty+ '(-1000 50 1000))
(define-constant +kp-explicit-hyphen-penalty+ '(-1000 50 1000))
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
  ((demerits :initform 0 :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start line-penalty hyphen-penalty
     &allow-other-keys
     &aux (stop (stop (boundary (node edge))))
	  (badness (badness lineup start stop width))
	  (penalty (if (word-stop-p lineup stop) 0 hyphen-penalty)))
  (assert (not (eq penalty :+infinity)))
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
  (loop :with boundaries :with last :with overfull
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
	  :else :do (setq last boundary)
	:finally (return (if boundaries
			   boundaries
			   (when (> pass 1)
			     ;; #### NOTE: according to my experiments, for
			     ;; instance by starting a line with Superkali...,
			     ;; it seems that numerical badness always leads
			     ;; to overfulls, but infinite badness makes TeX
			     ;; stop at an underfull.
			     (cond ((eq threshold :+infinity)
				    ;; There has to be an overfull here,
				    ;; because otherwise, we would have
				    ;; reached the end of the paragraph, which
				    ;; ends with a large glue, so it would
				    ;; have fitted.
				    (assert overfull)
				    (list overfull))
				   (t (list (or overfull last)))))))))


(defclass kp-layout (paragraph-layout)
  ((demerits :initform 0 :accessor demerits)))

(defmethod update-paragraph-layout ((layout kp-layout) (edge kp-edge))
  (setf (demerits layout) (!+ (demerits layout) (demerits edge))))


(defun kp-create-layout-lines
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

(defgeneric kp-create-lines
    (lineup width disposition variant &key &allow-other-keys)
  (:method (lineup width disposition (variant (eql :graph))
	    &key (line-penalty (cadr +kp-line-penalty+))
		 (hyphen-penalty (cadr +kp-hyphen-penalty+))
		 (pretolerance (cadr +kp-pre-tolerance+))
		 (tolerance (cadr +kp-tolerance+)))
    (cond ((< line-penalty (car +kp-line-penalty+))
	   (setq line-penalty (car +kp-line-penalty+)))
	  ((> line-penalty (caddr +kp-line-penalty+))
	   (setq line-penalty (caddr +kp-line-penalty+))))
    (cond ((<= hyphen-penalty (car +kp-hyphen-penalty+))
	   (setq hyphen-penalty :-infinity))
	  ((>= hyphen-penalty (caddr +kp-hyphen-penalty+))
	   (setq hyphen-penalty :+infinity)))
    (when (>= pretolerance (caddr +kp-pre-tolerance+))
      (setq pretolerance :+infinity))
    (cond ((>= tolerance (caddr +kp-tolerance+))
	   (setq pretolerance :+infinity))
	  ((< tolerance (car +kp-tolerance+))
	   (setq tolerance (car +kp-tolerance+))))
    (let ((graph (or (when (!<= 0 pretolerance)
		       (paragraph-graph lineup width :kp
			 :pass 1 :threshold pretolerance))
		     (paragraph-graph lineup width :kp
		       :pass 2 :threshold tolerance
		       :hyphen-penalty hyphen-penalty)))
	  layouts)
      (when graph
	(setq layouts (paragraph-layouts graph :kp))
	(setq layouts (sort layouts #'!< :key #'demerits))
	(kp-create-layout-lines lineup width disposition (car layouts)))))
  (:method (lineup width disposition (variant (eql :dynamic)) &key)
    ))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass))
     &rest options &key (variant (car +kp-variants+)))
  (apply #'kp-create-lines lineup width disposition variant options))
