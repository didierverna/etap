;; This algorithm uses only the normal, fixed, inter-word spacing. Lines are
;; created sequentially, without look-ahead or backtracking: there are no
;; paragraph-wide considerations.

;; The "Underfull" variant only allows underfull lines (unless there is no
;; choice). The "Overfull" one does the opposite. The "Anyfull" one normally
;; selects the solution that is the closest to the paragraph's width, whether
;; underfull or overfull. The behavior of these variants is further modulated
;; by the options described below.

;; The "Width Offset" option affects how the Anyfull variant computes the
;; proximity of a solution to the paragraph's width. When non-zero, it
;; virtually decreases the paragraph's width when doing comparisons, so that
;; underfulls appear less underfull, and overfulls appear more overfull. In
;; other words, the larger the offset, the fewer allowed overfulls.

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are preferred when there is a choice. This option takes
;; precedence over the Prefer Overfulls one (see below).

;; In the Anyfull variant, when the underfull and overfull line solutions are
;; equally distant from the paragraph's width (modulo the offset), and after
;; the Avoid Hyphens option has been taken into account, the underfull one is
;; chosen, unless the "Prefer Overfulls" option is checked.

;; Even though the inter-word spacing is fixed, there is a difference between
;; the justified disposition and the flush left one, although the chance of
;; actually witnessing it should be extremely thin.
;; In justified disposition, getting as close to the paragraph's width (modulo
;; the offset) takes precedence. Thus, the options apply only when there is no
;; fit, and two solutions (one underfull and one overfull) are equally bad.
;; In the ragged dispositions, we care less about the paragraph's width, so
;; the options take precedence. For example, if the Avoid Hyphens option is
;; checked, a word boundary will always be preferred to a hyphen boundary,
;; even if it's farther away from the paragraph's width.

;; Finally, note that because the inter-word spacing is fixed, the sloppy
;; option has no effect.


;; #### TODO: the Width Offset idea is but one possibility for modulating the
;; decision betwee under and overfulls. There are many other ideas that we
;; could implement to further tune the results. Here are two of them.
;;
;; 1. We could consider that hyphens (and why not punctuation !) use less ink
;;    than letters, so at the same distance from the paragraph's width,
;;    overfulls appear less overfull than with letters, and conversely
;;    underfulls appear more underfull than with letters. So this could also
;;    be used as some kind of offset.
;; 2. We could want raggedness thresholds. Well, not exactly since there's no
;;    paragraph-wide considerations, but at least, we could want to minimize
;;    the distance to the paragraph's width, so that for example, even when
;;    the Avoid Hyphens option is checked, a hyphen solution would still be
;;    preferred to a word one if the later really is too far away.


(in-package :etap)


(defparameter *fixed-variants*
  '(:underfull :anyfull :overfull))

(defparameter *fixed-variants-help-keys*
  '(:fixed-variant-underfull :fixed-variant-anyfull :fixed-variant-overfull))

(defparameter *fixed-options*
  '((:avoid-hyphens t) (:prefer-overfulls t)))

(defparameter *fixed-options-help-keys*
  '(:fixed-option-avoid-hyphens :fixed-option-prefer-overfulls))

(defparameter *fixed-tooltips*
  '(:fixed-variant-underfull "Always prefer underfull lines."
    :fixed-variant-anyfull "Prefer lines closer to the paragraph
width, whether underfull or overfull."
    :fixed-variant-overfull "Always prefer overfull lines."
    :fixed-option-avoid-hyphens "Avoid hyphenating words when possible."
    :fixed-option-prefer-overfulls
    "In the Anyfull variant, when the underfull and
overfull lines are equally distant from the
paragraph's width (modulo the offset), and
after the Avoid Hyphens option has been taken
into account, choose the overfull rather than
the underfull one."))


(defmacro define-fixed-caliber (name min default max)
  "Define a NAMEd Fixed caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber fixed ,name ,min ,default ,max))

;; #### FIXME: the -50pt value below is somewhat arbitrary.
(define-fixed-caliber width-offset -50 0 0)


;; #### NOTE: the WIDTH below already takes the offset into account.
(defun fixed-fallback-boundary
    (underfull underwidth overfull overwidth width prefer-overfulls
     &optional (policy :anyfull) avoid-hyphens)
  "Select UNDERFULL or OVERFULL boundary as a fallback solution, or NIL."
  (cond
    ;; No possibility, no choice.
    ((and (null underfull) (null overfull)) nil)
    ;; Only one possibility, no choice either.
    ((and underfull (not overfull)) underfull)
    ((and overfull (not underfull)) overfull)
    ;; Two possibilities from now on.
    ;; Still no choice in these two policies.
    ((eq policy :underfull) underfull)
    ((eq policy :overfull) overfull)
    ;; Anyfull policy from now on.
    ;; One solution is closer to the paragraph's width, so still no choice.
    ((< (- width underwidth) (- overwidth width)) underfull)
    ((> (- width underwidth) (- overwidth width)) overfull)
    ;; Equidistance.
    ;; If we have two, or no hyphen, the Avoid Hyphens option has no effect,
    ;; but we might still prefer overfulls.
    ((or (and (hyphenation-point-p (stop-elt underfull))
	      (hyphenation-point-p (stop-elt overfull)))
	 (and (not (hyphenation-point-p (stop-elt underfull)))
	      (not (hyphenation-point-p (stop-elt overfull)))))
     (if prefer-overfulls overfull underfull))
    ;; Exactly one hyphen. If we care, choose the other solution.
    (avoid-hyphens
     (if (hyphenation-point-p (stop-elt underfull))
       overfull
       underfull))
    ;; Finally, we might still prefer overfulls.
    (t (if prefer-overfulls overfull underfull))))


;; #### NOTE: the two functions below are very similar. The only difference is
;; that the one used for justification only collects the last underfull and
;; the first overfull, regardless of their word / hyphen nature (that's
;; because getting as close to the paragraph's width takes precedence). They
;; are implemented as two different functions because the Fit algorithm reuses
;; the ragged version below (that's also why the width function is
;; parameterized).

(defun fixed-justified-line-boundary
    (lineup start width variant avoid-hyphens prefer-overfulls width-offset)
  "Return the Fixed algorithm's view of the end of a justified line boundary."
  (loop :with underfull :with underwidth
	:with fit
	:with overfull :with overwidth
	;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH NIL)
	;; first, and then NIL.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	:while (and boundary (not overfull))
	:for w := (lineup-width lineup start (stop-idx boundary))
	:if (< w width)
	  ;; Track the last underfulls because they're the closest to WIDTH.
	  :do (setq underfull boundary underwidth w)
	:else :if (= w width)
	  :do (setq fit boundary)
	:else
	  :do (setq overfull boundary overwidth w)
	:finally (return (or fit
			     (fixed-fallback-boundary
			      underfull underwidth overfull overwidth
			      (+ width width-offset) prefer-overfulls
			      variant avoid-hyphens)))))

;; In order to handle all variants and options, this function starts by
;; collecting the interesting breakpoints, that is, the last word and hyphen
;; underfulls, a possible miraculous fit, and the first word and hyphen
;; overfulls. After that, we look at the variants and options, and decide on
;; what to return from the collected possibilities.
(defun fixed-ragged-line-boundary
    (lineup start width variant avoid-hyphens prefer-overfulls width-offset
     &optional (width-function #'lineup-width))
  "Return the Fixed algorithm's view of the end of a ragged line boundary."
  (loop :with underfull :with hyphen-underfull :with word-underfull
	:with underwidth :with hyphen-underwidth :with word-underwidth
	:with fit
	:with overfull :with hyphen-overfull :with word-overfull
	:with overwidth :with hyphen-overwidth :with word-overwidth
	;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH NIL)
	;; first, and then NIL.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	;; #### NOTE: we're satisfied to stop at the first word overfull, even
	;; if we don't have an hyphen overfull, because the Avoid Hyphens
	;; options would have no effect there. On the other hand, if we
	;; already have an hyphen overfull, it's still important to collect a
	;; word overfull if possible, because of that very same option.
	:while (and boundary (not word-overfull))
	:for w := (funcall width-function lineup start (stop-idx boundary))
	:for hyphenp := (hyphenation-point-p (stop-elt boundary))
	:if (< w width)
	  ;; Track the last underfulls because they're the closest to WIDTH.
	  :do (setq underfull boundary underwidth w)
	  :and :do (if hyphenp
		     (setq hyphen-underfull boundary hyphen-underwidth w)
		     (setq word-underfull boundary word-underwidth w))
	:else :if (= w width)
	  :do (setq fit boundary)
	:else
	  ;; Track the first overfulls because they're the closest to WIDTH.
	  :do (unless overfull (setq overfull boundary overwidth w))
	  :and :do (if hyphenp
		     (unless hyphen-overfull
		       (setq hyphen-overfull boundary hyphen-overwidth w))
		     ;; No check required here because we stop at the first
		     ;; word overfull.
		     (setq word-overfull boundary word-overwidth w))
	:finally
	   (return
	     (cond ((and fit
			 (hyphenation-point-p (stop-elt fit))
			 avoid-hyphens)
		    ;; We have a hyphen fit but we prefer to avoid hyphens.
		    ;; Choose a word solution if possible. Otherwise, fallback
		    ;; to the hyphen fit.
		    (ecase variant
		      (:underfull (or word-underfull fit))
		      (:anyfull
		       (or (fixed-fallback-boundary
			    word-underfull word-underwidth
			    word-overfull word-overwidth
			    (+ width width-offset) prefer-overfulls)
			   fit))
		      (:overfull (or word-overfull fit))))
		   ;; We have a fit and we don't care about hyphens or it's a
		   ;; word fit. Choose it.
		   (fit fit)
		   (avoid-hyphens
		    ;; We don't have a fit and we want to avoid hyphens.
		    ;; Choose a word solution if possible.
		    (ecase variant
		      (:underfull (or word-underfull underfull overfull))
		      (:anyfull
		       (or (fixed-fallback-boundary
			    word-underfull word-underwidth
			    word-overfull word-overwidth
			    (+ width width-offset) prefer-overfulls)
			   (fixed-fallback-boundary
			    underfull underwidth overfull overwidth
			    (+ width width-offset) prefer-overfulls)))
		      (:overfull (or word-overfull overfull underfull))))
		   (t
		    ;; We don't care about hyphens. Choose the best solution.
		    (fixed-fallback-boundary
		     underfull underwidth overfull overwidth
		     (+ width width-offset) prefer-overfulls variant))))))


(defmacro default-fixed (name)
  "Default Fixed NAMEd variable."
  `(default fixed ,name))

(defmacro calibrate-fixed (name &optional infinity)
  "Calibrate NAMEd Fixed variable."
  `(calibrate fixed ,name ,infinity))

(defmethod make-lines
    (lineup disposition width (algorithm (eql :fixed))
     &key variant avoid-hyphens prefer-overfulls width-offset
     &aux (get-line-boundary  (if (eq (disposition-type disposition) :justified)
				#'fixed-justified-line-boundary
				#'fixed-ragged-line-boundary)))
  "Typeset LINEUP with the Fixed algorithm."
  (default-fixed variant)
  (calibrate-fixed width-offset)
  (loop :for start := 0 :then (next-start boundary)
	:while start
	:for boundary := (funcall get-line-boundary
			   lineup start width variant
			   avoid-hyphens prefer-overfulls width-offset)
	:collect (make-line lineup start (stop-idx boundary))))
