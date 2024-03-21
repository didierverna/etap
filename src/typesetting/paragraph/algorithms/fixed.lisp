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

(defmacro calibrate-fixed (name &optional infinity)
  "Calibrate NAMEd Fixed variable."
  `(calibrate fixed ,name ,infinity))




;; ==========================================================================
;; Boundaries
;; ==========================================================================

;; #### NOTE: the MIN-WIDTH and MAX-WIDTH accessors below are here because the
;; FIXED-FALLBACK-BOUNDARY function calls them. It makes little sense for
;; fixed boundaries, but this function may actually be passed fit boundaries
;; from the Fit algorithm in justified disposition, in which case the min,
;; max, and natural widths are indeed going to be be different.
(defclass fixed-boundary (boundary)
  ((width :documentation "This boundary's line width.
This is normally the line's natural width, but it can also be its minimum or
maximum width, when the boundary is manipulated by the Fit algorithm."
	  :reader width :reader min-width :reader max-width))
  (:documentation "The FIXED-BOUNDARY class."))

;; #### NOTE: since HARRAY-WIDTH computes the whole line properties, we might
;; just as well remember those values for other boundary classes.
(defmethod initialize-instance :around
    ((boundary fixed-boundary) &rest keys &key harray start stop-idx)
  "Compute and propagate BOUNDARY's line properties to subsequent methods."
  (multiple-value-bind (natural max min stretch shrink)
      (harray-width harray start stop-idx)
    (apply #'call-next-method boundary
	   :natural-width natural :max-width max :min-width min
	   :stretch stretch :shrink shrink
	   keys)))

;; #### NOTE: the first- and last-fit algorithms use fixed boundaries with
;; non-natural widths in ragged dispositions, hence the parametrization below.
(defmethod initialize-instance :after
    ((boundary fixed-boundary)
     &key natural-width max-width min-width (width-kind :natural))
  "Initialize BOUNDARY's width with WIDTH-KIND (natural width by default)."
  (setf (slot-value boundary 'width)
	(ecase width-kind
	  (:natural natural-width)
	  (:min min-width)
	  (:max max-width))))


;; ---------------
;; Boundary lookup
;; ---------------

;; #### NOTE: the WIDTH below already takes the width offset into account.
;; Also, it is safe to use regular (numerical) arithmetic here because we only
;; access the max width of underfulls (so necessarily a number; otherwise it
;; wouldn't be an underfull), and the min width of overfulls (also necessarily
;; a number).
(defun fixed-fallback-boundary (underfull overfull width)
  "Select UNDERFULL, OVERFULL, or NIL, as a fallback boundary."
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
    ((< (- width (max-width underfull)) (- (min-width overfull) width))
     underfull)
    ((> (- width (max-width underfull)) (- (min-width overfull) width))
     overfull)
    ;; Equidistance.
    ;; If we have two, or no hyphen, the Avoid Hyphens option has no effect,
    ;; but we might still prefer overfulls.
    ((or (and (hyphenation-point-p (item underfull))
	      (hyphenation-point-p (item overfull)))
	 (and (not (hyphenation-point-p (item underfull)))
	      (not (hyphenation-point-p (item overfull)))))
     (if *prefer-overfulls* overfull underfull))
    ;; Exactly one hyphen. If we care, choose the other solution.
    (*avoid-hyphens*
     (if (hyphenation-point-p (item underfull)) overfull underfull))
    ;; Finally, we might still prefer overfulls.
    (t (if *prefer-overfulls* overfull underfull))))


;; This function collects boundaries between the last underfull (included) and
;; the first overfull (included), regardless of their hyphenation status.
;; That's because getting as close to the paragraph's width takes precedence
;; in justified disposition.
(defun fixed-justified-line-boundary (harray start width)
  "Return the Fixed algorithm's view of the end of a justified line boundary."
  (loop :with underfull :with fit :with overfull
	:for boundary
	  := (next-boundary harray start 'fixed-boundary :start start)
	    :then (next-boundary harray (stop-idx boundary) 'fixed-boundary
				 :start start)
	:while (and boundary (not overfull))
	:do (cond ((< (width boundary) width) (setq underfull boundary))
		  ((= (width boundary) width) (setq fit boundary))
		  (t (setq overfull boundary)))
	:finally
	   (return (or fit
		       (fixed-fallback-boundary
			underfull overfull (+ width *width-offset*))))))


;; #### NOTE: the function below handles infinite penalties and understands a
;; WIDTH-KIND argument because the first- and last-fit algorithms in ragged
;; dispositions use it. Also, because WIDTH-KIND can be set to :max-width, we
;; need to be prepared to handle a width of +∞.

;; In this function, we stop at the first word overfull even if we don't have
;; an hyphen overfull yet, because the Avoid Hyphens options would have no
;; effect. On the other hand, if we already have an hyphen overfull, it's
;; still important to collect a word overfull if possible, because of that
;; very same option.
(defun fixed-ragged-line-boundary
    (harray start width &optional (width-kind :natural))
  "Return the Fixed algorithm's view of the end of a ragged line boundary."
  (loop :with underfull :with underword :with fit :with overfull :with overword
	:with continue := t
	:for boundary := (next-boundary harray start 'fixed-boundary
					:start start :width-kind width-kind)
	  :then (next-boundary harray (stop-idx boundary) 'fixed-boundary
			       :start start :width-kind width-kind)
	:while continue
	:do (when ($< (penalty (item boundary)) +∞)
	      (when (eq (penalty (item boundary)) -∞) (setq continue nil))
	      (let ((hyphenp (hyphenation-point-p (item boundary))))
		(cond (($< (width boundary) width)
		       ;; Track the last underfulls because they're the
		       ;; closest to WIDTH.
		       (setq underfull boundary)
		       (unless hyphenp (setq underword boundary)))
		      (($= (width boundary) width) (setq fit boundary))
		      (t
		       ;; Track the first overfulls because they're the
		       ;; closest to WIDTH.
		       (unless overfull (setq overfull boundary))
		       ;; No check required here because we stop at the first
		       ;; word overfull anyway.
		       (unless hyphenp
			 (setq overword boundary continue nil))))))
	:finally
	   (return
	     (cond ((and fit (hyphenation-point-p (item fit)) *avoid-hyphens*)
		    ;; We have a hyphen fit but we prefer to avoid hyphens.
		    ;; Choose a word solution if possible. Otherwise, fallback
		    ;; to the hyphen fit.
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

(defun fixed-break-harray (harray disposition width beds)
  "Make fixed pinned lines from HARRAY for a DISPOSITION paragraph of WIDTH."
  (loop :with disposition := (disposition-type disposition)
	:with line-boundary := (case disposition
				 (:justified #'fixed-justified-line-boundary)
				 (t #'fixed-ragged-line-boundary))
	:with baseline-skip := (baseline-skip harray)
	:for y := 0 :then (+ y baseline-skip)
	:for start := 0 :then (start-idx boundary) :while start
	:for boundary := (funcall line-boundary harray start width)
	:for line := (make-instance 'line
		       :harray harray
		       :start-idx start :stop-idx (stop-idx boundary)
		       :beds beds)
	:for x := (case disposition
		    ((:flush-left :justified) 0)
		    (:centered (/ (- width (width line)) 2))
		    (:flush-right (- width (width line))))
	:collect (pin-line line x y)))


(defmethod break-harray
    (harray disposition width beds (algorithm (eql :fixed))
     &key ((:fallback *fallback*))
	  ((:width-offset *width-offset*))
	  ((:avoid-hyphens *avoid-hyphens*))
	  ((:prefer-overfulls *prefer-overfulls*)))
  "Break HARRAY with the Fixed algorithm."
  (default-fixed fallback)
  (calibrate-fixed width-offset)
  (make-instance 'simple-breakup
    :pinned-lines (unless (zerop (length harray))
		    (fixed-break-harray harray disposition width beds))))
