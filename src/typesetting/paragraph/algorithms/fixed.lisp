;; This algorithm uses only the normal, fixed, inter-word spacing. Lines are
;; created sequentially, without look-ahead or backtracking: there are no
;; paragraph-wide considerations.

;; The "Underfull" variant only allows underfull lines (unless there is no
;; choice). The "Overfull" one does the opposite. The "Best" one chooses which
;; is closest to the paragraph width.

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are always preferred when there is a choice.

;; In the Best variant, when the underfull and overfull line solutions are
;; equally distant from the paragraph width, the underfull one is chosen,
;; unless the "Prefer Overfulls" option is checked.

;; Note that because the inter-word spacing is fixed, there is no difference
;; between the Flush Left and Justified dispositions (justification is
;; practically never possible), and the sloppy option has no effect.


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


(defun fixed-line-boundary
    (lineup start width
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
	  :then (next-boundary lineup (stop boundary))
	:while (and boundary (not word-overfull))
	:for w := (lineup-width lineup start (stop boundary))
	:if (< w width)
	  :do (setq underfull boundary underfull-w w)
	  :and :do (if (word-boundary-p lineup boundary)
		     (setq word-underfull boundary word-underfull-w w)
		     (setq hyphen-underfull boundary hyphen-underfull-w w))
	:else :if (= w width)
	  :do (setq fit boundary)
	:else
	  :do (setq overfull boundary overfull-w w)
	  :and :do (if (word-boundary-p lineup boundary)
		     (setq word-overfull boundary word-overfull-w w)
		     (setq hyphen-overfull boundary hyphen-overfull-w w))
	:finally
	   (return
	     ;; #### NOTE: UNDERFULL is the last of the two possible ones, so
	     ;; the closest to the paragraph width. The closest overfull, on
	     ;; the other hand, is the first one, not the last.
	     (let ((first-overfull
		     (if (and hyphen-overfull word-overfull)
		       (if (eq overfull hyphen-overfull)
			 word-overfull
			 hyphen-overfull)
		       overfull)))
	       (ecase variant
		 (:underfull
		  (cond ((and fit
			      (not (word-boundary-p lineup fit))
			      avoid-hyphens)
			 (or word-underfull fit))
			(fit fit)
			(underfull
			 (if avoid-hyphens
			   (or word-underfull underfull)
			   underfull))
			(t first-overfull)))
		 (:overfull
		  (cond ((and fit
			      (not (word-boundary-p lineup fit))
			      avoid-hyphens)
			 (or word-overfull fit))
			(fit fit)
			(overfull
			 (if avoid-hyphens
			   (or word-overfull overfull)
			   first-overfull))
			(t underfull)))
		 (:best
		  (cond (fit fit)
			((and underfull (not overfull))
			 underfull)
			((and overfull (not underfull))
			 first-overfull)
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
			     (let ((first-overfull-w
				     (if (eq first-overfull hyphen-overfull)
				       hyphen-overfull-w
				       word-overfull-w)))
			       (best-*full
				underfull underfull-w
				first-overfull first-overfull-w))))))))))))

(defmethod make-lines
    (lineup disposition width (algorithm (eql :fixed))
     &rest keys &key variant avoid-hyphens prefer-overfulls)
  "Typeset LINEUP with the Fixed algorithm."
  (declare (ignore variant avoid-hyphens prefer-overfulls))
  (loop :for start := 0 :then (next-start boundary)
	:while start
	:for boundary := (apply #'fixed-line-boundary lineup start width keys)
	:collect (make-line lineup start (stop boundary))))
