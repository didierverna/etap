;; This algorithm uses only the normal, fixed, inter-word spacing. Lines are
;; created sequentially, without look-ahead or backtracking: there are no
;; paragraph-wide considerations.

;; The "Underfull" fallback only allows underfull lines (unless there is no
;; choice). The "Overfull" one does the opposite. The "Anyfull" one normally
;; selects the solution that is the closest to the paragraph's width, whether
;; underfull or overfull. The behavior of these fallbacks is further modulated
;; by the options described below.

;; The "Width Offset" option affects how the Anyfull fallback computes the
;; proximity of a solution to the paragraph's width. When non-zero, it
;; virtually decreases the paragraph's width when doing comparisons, so that
;; underfulls appear less underfull, and overfulls appear more overfull. In
;; other words, the larger the offset, the fewer allowed overfulls.

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are preferred when there is a choice. This option takes
;; precedence over the Prefer Overfulls one (see below).

;; In the Anyfull fallback, when the underfull and overfull line solutions are
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

;; Finally, note that because the inter-word spacing is fixed, the
;; "Overstretch" and "Overshrink" options have no effect.


;; #### TODO: the Width Offset idea is but one possibility for modulating the
;; decision between under and overfulls. There are many other ideas that we
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


;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *fixed-fallbacks*
  '(:underfull :anyfull :overfull))

(defparameter *fixed-fallbacks-help-keys*
  '(:fixed-fallback-underfull :fixed-fallback-anyfull :fixed-fallback-overfull))

(defparameter *fixed-options*
  '((:avoid-hyphens t) (:prefer-overfulls t)))

(defparameter *fixed-options-help-keys*
  '(:fixed-option-avoid-hyphens :fixed-option-prefer-overfulls))

(defparameter *fixed-tooltips*
  '(:fixed-fallback-underfull "Always prefer underfull lines."
    :fixed-fallback-anyfull "Prefer lines closer to the paragraph
width, whether underfull or overfull."
    :fixed-fallback-overfull "Always prefer overfull lines."
    :fixed-option-avoid-hyphens "Avoid hyphenating words when possible."
    :fixed-option-prefer-overfulls
    "In the Anyfull fallback, when the underfull and
overfull lines are equally distant from the
paragraph's width (modulo the offset), and
after the Avoid Hyphens option has been taken
into account, choose the overfull rather than
the underfull one."))


(define-global-variables fallback width-offset avoid-hyphens prefer-overfulls)


(defmacro define-fixed-caliber (name min default max)
  "Define a NAMEd Fixed caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber fixed ,name ,min ,default ,max))

(define-fixed-caliber width-offset -50 0 0)


(defmacro default-fixed (name)
  "Default Fixed NAMEd variable."
  `(default fixed ,name))

(defmacro calibrate-fixed (name)
  "Calibrate NAMEd Fixed variable."
  `(calibrate fixed ,name))




;; ==========================================================================
;; Boundaries
;; ==========================================================================

;; #### NOTE: the MIN-WIDTH and MAX-WIDTH accessors below are here because the
;; FIXED-FALLBACK-BOUNDARY function calls them. It makes little sense for
;; fixed boundaries, but this function may actually be passed fit boundaries
;; from the Fit algorithm in justified disposition, in which case the min,
;; max, and natural widths are indeed going to be be different.
(defclass fixed-boundary (boundary)
  ((width :documentation "This boundary's natural line width."
	  :initarg :width :reader width :reader min-width :reader max-width))
  (:documentation "The FIXED-BOUNDARY class."))

;; #### NOTE: since HARRAY-WIDTH computes the whole line properties, we might
;; just as well remember those values for other boundary classes.
(defmethod initialize-instance :around
    ((boundary fixed-boundary) &rest keys &key harray bol break-point)
  "Compute and propagate BOUNDARY's line properties to subsequent methods."
  (multiple-value-bind (width max min stretch shrink)
      (harray-width harray (bol-idx bol) (eol-idx break-point))
    (apply #'call-next-method boundary
	   :width width :max-width max :min-width min
	   :stretch stretch :shrink shrink
	   keys)))

(defmethod properties strnlcat ((boundary fixed-boundary) &key)
  "Return a string advertising Fixed BOUNDARY's natural width."
  (format nil "Natural width: ~Apt." (float (width boundary))))


;; ---------------
;; Boundary lookup
;; ---------------

;; #### NOTE: the WIDTH below already takes the width offset into account.
;; Also, it is safe to use regular (numerical) arithmetic here because we only
;; access the max width of underfulls (so necessarily a number; otherwise it
;; wouldn't be an underfull), and the min width of overfulls (also necessarily
;; a number).
(defun fixed-fallback-boundary
    (underfull overfull width
     &optional get-width
     &aux (get-max-width (or get-width #'max-width))
	  (get-min-width (or get-width #'min-width)))
  "Select UNDERFULL, OVERFULL, or NIL, as a fallback boundary.
If GET-WIDTH is non nil, it is used instead of the regular min-width and
max-width accessors. This happens when the First/Last Fit algorithm calls this
function in ragged dispositions (in which case it looks at its boundaries as
if they were fixed ones, with either min or max width as their pseudo-natural
widths."
  (cond
    ;; No possibility, no choice.
    ((and (null underfull) (null overfull)) nil)
    ;; Only one possibility, no choice either.
    ((and underfull (not overfull)) underfull)
    ((and overfull (not underfull)) overfull)
    ;; Two possibilities from now on.
    ;; Still no choice in these two policies.
    ((eq *fallback* :underfull) underfull)
    ((eq *fallback* :overfull) overfull)
    ;; Anyfull fallback from now on.
    ;; One solution is closer to the paragraph's width, so still no choice.
    ((< (- width (funcall get-max-width underfull))
	(- (funcall get-min-width overfull) width))
     underfull)
    ((> (- width (funcall get-max-width underfull))
	(- (funcall get-min-width overfull) width))
     overfull)
    ;; Equidistance.
    ;; If we have two, or no hyphen, the Avoid Hyphens option has no effect,
    ;; but we might still prefer overfulls.
    ((or (and (hyphenated underfull) (hyphenated overfull))
	 (and (not (hyphenated underfull)) (not (hyphenated overfull))))
     (if *prefer-overfulls* overfull underfull))
    ;; Exactly one hyphen. If we care, choose the other solution.
    (*avoid-hyphens*
     (if (hyphenated underfull) overfull underfull))
    ;; Finally, we might still prefer overfulls.
    (t (if *prefer-overfulls* overfull underfull))))


;; This function collects boundaries between the last underfull (included) and
;; the first overfull (included), regardless of their hyphenation status.
;; That's because getting as close to the paragraph's width takes precedence
;; in justified disposition.
(defun fixed-get-justified-boundary (harray bol width)
  "Return the boundary for a justified HARRAY line of WIDTH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This is the Fixed algorithm version."
  (loop :with underfull :with fit :with overfull
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol (not overfull))
	:for boundary := (make-instance 'fixed-boundary
			   :harray harray :bol bol :break-point eol)
	:do (cond ((< (width boundary) width) (setq underfull boundary))
		  ((= (width boundary) width) (setq fit boundary))
		  (t (setq overfull boundary)))
	:finally (return (or fit
			     (fixed-fallback-boundary
			      underfull overfull (+ width *width-offset*))))))


;; In this function, we stop at the first word overfull even if we don't have
;; an hyphen overfull yet, because the Avoid Hyphens options would have no
;; effect. On the other hand, if we already have an hyphen overfull, it's
;; still important to collect a word overfull if possible, because of that
;; very same option.
(defun fixed-get-ragged-boundary (harray bol width)
  "Return the boundary for a ragged HARRAY line of WIDTH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This is the Fixed algorithm version."
  (loop :with underfull :with underword :with fit :with overfull :with overword
	:with continue := t
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol continue)
	:for hyphenated := (hyphenation-point-p eol)
	:for boundary := (make-instance 'fixed-boundary
			   :harray harray :bol bol :break-point eol)
	:do (cond ((< (width boundary) width)
		   ;; Track the last underfulls because they're the closest to
		   ;; WIDTH.
		   (setq underfull boundary)
		   (unless hyphenated (setq underword boundary)))
		  ((= (width boundary) width) (setq fit boundary))
		  (t
		   ;; Track the first overfulls because they're the closest to
		   ;; WIDTH.
		   (unless overfull (setq overfull boundary))
		   ;; No check required here because we stop at the first word
		   ;; overfull anyway.
		   (unless hyphenated (setq overword boundary continue nil))))
	:finally
	   (return
	     (cond ((and fit (hyphenated fit) *avoid-hyphens*)
		    ;; We have a hyphenated fit but we prefer to avoid
		    ;; hyphens. Choose a word solution if possible. Otherwise,
		    ;; fallback to the fit.
		    (ecase *fallback*
		      (:underfull (or underword fit))
		      (:anyfull (or (fixed-fallback-boundary
				     underword overword
				     (+ width *width-offset*))
				    fit))
		      (:overfull (or overword fit))))
		   ;; We have a fit and we don't care about hyphens or it's a
		   ;; word fit. Choose it.
		   (fit fit)
		   (*avoid-hyphens*
		    ;; We don't have a fit and we want to avoid hyphens.
		    ;; Choose a word solution if possible.
		    (ecase *fallback*
		      (:underfull (or underword underfull overfull))
		      (:anyfull (or (fixed-fallback-boundary
				     underword overword
				     (+ width *width-offset*))
				    (fixed-fallback-boundary
				     underfull overfull
				     (+ width *width-offset*))))
		      (:overfull (or overword overfull underfull))))
		   (t
		    ;; We don't care about hyphens. Choose the best solution.
		    (fixed-fallback-boundary
		     underfull overfull (+ width *width-offset*)))))))




;; ==========================================================================
;; Breakup
;; ==========================================================================

(defmethod break-harray
    (harray disposition width (algorithm (eql :fixed))
     &key ((:fallback *fallback*))
	  ((:width-offset *width-offset*))
	  ((:avoid-hyphens *avoid-hyphens*))
	  ((:prefer-overfulls *prefer-overfulls*)))
  "Break HARRAY with the Fixed algorithm."
  (default-fixed fallback)
  (calibrate-fixed width-offset)
  (make-greedy-breakup harray disposition width
		       (case (disposition-type disposition)
			 (:justified #'fixed-get-justified-boundary)
			 (t          #'fixed-get-ragged-boundary))
		       #'make-line))
