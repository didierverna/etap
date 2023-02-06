;; This algorithm uses only the normal, fixed, inter-word spacing. Lines are
;; created sequentially, without look-ahead or backtracking: there are no
;; paragraph-wide considerations.

;; The "Underfull" variant only allows underfull lines (unless there is no
;; choice). The "Overfull" one does the opposite. The "Best" one chooses which
;; is closest to the paragraph width.

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are always preferred when there is a choice. Note that this
;; option has precedence over the "Prefer Overfulls" one below.

;; In the Best variant, when the underfull and overfull line solutions are
;; equally distant from the paragraph width, and after the "Avoid Hyphens"
;; option has been taken into account if applicable, the underfull one is
;; chosen, unless the "Prefer Overfulls" option is checked.

;; Note that because the inter-word spacing is fixed, the sloppy option has no
;; effect. Also, for the same reason, it's practically impossible to justify.
;; The only difference between the justified disposition and the flush left
;; one is that the options have no effect when a fit solution is found.

(in-package :etap)


(defparameter *fixed-variants*
  '(:underfull :best :overfull))

(defparameter *fixed-variants-help-keys*
  '(:fixed-variant-underfull :fixed-variant-best :fixed-variant-overfull))

(defparameter *fixed-options*
  '((:avoid-hyphens t) (:prefer-overfulls t)))

(defparameter *fixed-options-help-keys*
  '(:fixed-option-avoid-hyphens :fixed-option-prefer-overfulls))

(defparameter *fixed-tooltips*
  '(:fixed-variant-underfull "Always prefer underfull lines."
    :fixed-variant-best "Prefer lines closer to the paragraph
width, whether underfull or overfull."
    :fixed-variant-overfull "Always prefer overfull lines."
    :fixed-option-avoid-hyphens "Avoid hyphenating words when possible."
    :fixed-option-prefer-overfulls
    "For the Best variant, when the underfull and overfull
lines are equally distant from the paragraph width,
choose the overfull rather than the underfull one."))


;; In order to handle all variants and options, this function starts by
;; collecting the possible breakpoints, that is, the last word and hyphen
;; underfulls, a possible miraculous fit, and the first word and hyphen
;; overfulls. After that, we look at the variants and options, and decide on
;; what to return from the collected possibilities.
(defun fixed-line-boundary
    (lineup start width justification
     &key (variant (car *fixed-variants*)) avoid-hyphens prefer-overfulls)
  "Return the Fixed algorithm's view of the end of line boundary."
  (loop :with underfull :with hyphen-underfull :with word-underfull
	:with underfull-w :with hyphen-underfull-w :with word-underfull-w
	:with fit
	:with overfull :with hyphen-overfull :with word-overfull
	:with overfull-w :with hyphen-overfull-w :with word-overfull-w
	;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH NIL)
	;; first, and then NIL.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	;; #### NOTE: we're satisfied to stop at the first word overfull, even
	;; if we don't have an hyphen overfull, because the "Avoid Hyphens"
	;; options would have no effect there. On the other hand, if we
	;; already have an hyphen overfull, it's still important to collect a
	;; word overfull if possible, because of that very same option.
	:while (and boundary (not word-overfull))
	:for w := (lineup-width lineup start (stop-idx boundary))
	:for hyphenp := (hyphenation-point-p (stop-elt boundary))
	:if (< w width)
	  ;; Track the last underfulls because they're the closest to WIDTH.
	  :do (setq underfull boundary underfull-w w)
	  :and :do (if hyphenp
		     (setq hyphen-underfull boundary hyphen-underfull-w w)
		     (setq word-underfull boundary word-underfull-w w))
	:else :if (= w width)
	  :do (setq fit boundary)
	:else
	  ;; Track the first overfulls because they're the closest to WIDTH.
	  :do (unless overfull (setq overfull boundary overfull-w w))
	  :and :do (if hyphenp
		     (unless hyphen-overfull
		       (setq hyphen-overfull boundary hyphen-overfull-w w))
		     ;; No check here because we stop at the first word
		     ;; overfull.
		     (setq word-overfull boundary word-overfull-w w))
	:finally
	   (return
	     (if justification
	       (if fit
		 fit
		 (ecase variant
		   (:underfull (or underfull overfull))
		   (:overfull (or overfull underfull))
		   (:best
		    (cond
		      ;; Only one possibility, so no choice.
		      ((and underfull (not overfull))
		       underfull)
		      ((and overfull (not underfull))
		       overfull)
		      ;; Two possibilities, but one is closer to the
		      ;; paragraph's width, so still no choice.
		      ((< (- width underfull-w) (- overfull-w width))
		       underfull)
		      ((> (- width underfull-w) (- overfull-w width))
		       overfull)
		      ;; Now we have equidistance.
		      ;; If we have two, or no hyphen, the "Avoid Hyphens"
		      ;; option has no effect, but we might still prefer
		      ;; overfulls.
		      ((or (and (eq underfull hyphen-underfull)
				(eq overfull hyphen-overfull))
			   (and (eq underfull word-underfull)
				(eq overfull word-overfull)))
		       (if prefer-overfulls overfull underfull))
		      ;; Now we know we have exactly one hyphen.
		      ;; If we care, choose the other solution.
		      (avoid-hyphens
		       (if (eq underfull hyphen-underfull)
			 overfull
			 underfull))
		      ;; Finally, we might still prefer overfulls.
		      (t
		       (if prefer-overfulls overfull underfull))))))
	       (ecase variant
		 (:underfull
		  (cond ((and fit
			      (hyphenation-point-p (stop-elt fit))
			      avoid-hyphens)
			 (or word-underfull fit))
			(fit fit)
			(underfull
			 (if avoid-hyphens
			   (or word-underfull underfull)
			   underfull))
			(t overfull)))
		 (:overfull
		  (cond ((and fit
			      (hyphenation-point-p (stop-elt fit))
			      avoid-hyphens)
			 (or word-overfull fit))
			(fit fit)
			(overfull
			 (if avoid-hyphens
			   (or word-overfull overfull)
			   overfull))
			(t underfull)))
		 (:best
		  (cond (fit fit)
			((and underfull (not overfull))
			 underfull)
			((and overfull (not underfull))
			 overfull)
			(t
			 (flet ((best-*full (underfull underfull-w
					     overfull overfull-w)
				  (cond ((< (- width underfull-w)
					    (- overfull-w width))
					 underfull)
					((< (- overfull-w width)
					    (- width underfull-w))
					 overfull)
					(prefer-overfulls overfull)
					(t underfull))))
			   (if avoid-hyphens
			     (cond ((and word-underfull (not word-overfull))
				    word-underfull)
				   ((and (not word-underfull) word-overfull)
				    word-overfull)
				   ((and word-underfull word-overfull)
				    (best-*full word-underfull word-underfull-w
						word-overfull word-overfull-w))
				   (t
				    (best-*full underfull underfull-w
						overfull overfull-w)))
			     (best-*full
			      underfull underfull-w
			      overfull overfull-w)))))))))))

(defmethod make-lines
    (lineup disposition width (algorithm (eql :fixed))
     &rest keys &key variant avoid-hyphens prefer-overfulls
     &aux (justification (eq (disposition-type disposition) :justified)))
  "Typeset LINEUP with the Fixed algorithm."
  (declare (ignore variant avoid-hyphens prefer-overfulls))
  (loop :for start := 0 :then (next-start boundary)
	:while start
	:for boundary := (apply #'fixed-line-boundary
			   lineup start width justification keys)
	:collect (make-line lineup start (stop-idx boundary))))
