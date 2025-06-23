;; This is the classical *-fit algorithms family, making full use of
;; inter-word (elastic) glue. Lines are created sequentially, without
;; look-ahead or backtracking: there are no paragraph-wide considerations.

;; In ragged dispositions, the "First" variant stops as soon as a line fits,
;; that is, with the minimum number of characters and the maximum stretch,
;; while remaining below the paragraph width. The "Last" variant does the
;; opposite (maximum number of characters and maximum shrink). Finally, the
;; "Best" variant preserves the natural inter-word spacing. The ragged
;; dispositions actually work exactly like those of the Fixed algorithm, and
;; thus react to "Width Offset", "Avoid Hyphens", and "Prefer Overfulls" in
;; the same way. In fact, in ragged dispositions, the best-fit and the Fixed
;; algorithms are strictly equivalent.

;; In the Justified disposition, the First variant selects the first line that
;; fits the paragraph width exactly (hence, also with the minimum number of
;; characters and the maximum stretch). The Last variant does the opposite
;; (maximum number of characters and maximum shrink). The Best variant uses
;; TeX's local demerits to weight solutions. When there is no fit, all
;; versions fall back to the ragged (or Fixed algorithm) logic. That is, they
;; behave according to the settings of "Width Offset", "Avoid Hyphens", and
;; "Prefer Overfulls".

;; The "Relax" option affects the First and Last variants in ragged
;; dispositions. When checked, lines are "de-stretched" or "de-shrunk" towards
;; the natural inter-word spacing (which is why it is meaningless for the Best
;; variant), after having been created, and without changing their potential
;; contents. More specifically:
;; - For the first-fit, lines are de-stretched as much as possible, but not to
;;   the point that another chunk would fit in. The effect is thus to make the
;;   paragraph more compact.
;; - For the last-fit, lines are de-shrunk as much as possible towards the
;;   natural inter-word space, without producing overfulls (already overfull
;;   lines cannot be deshrunk, as the overfull would be even more important).
;;   The effect is thus to make the paragraph less compact.

;; The "[Explicit ]Hyphen Penalty" option affects the best-fit in justified
;; disposition in the TeX way when weighting solutions. In particular, this
;; means that infinite penalties are understood, leading to either prohibited
;; or mandatory breaks. Note however that we only use TeX's badness to weight
;; fit solutions, so the badness itself can never be infinite.

;; Even though there is no weighting of the solutions in the other variants /
;; dispositions, infinite penalties (adjusted with the same option) are
;; understood, leading to support both mandatory and prohibited breaks.
;; Numerical penalty values have no effect.

;; In the justified best-fit scenario, several solutions may turn out to have
;; the same weight, meaning different boundaries with or without varying
;; hyphen penalties. The "Discriminating Function" is used to break the tie.
;; For "Minimize Distance", we minimize the difference between the natural
;; width of the line and the desired one. For "minimize Scaling", we minimize
;; the SAR. The difference is that in the second case, we take the frequent
;; difference between maximum stretching and shrinking into account. If this
;; leads to equality again, then we necessarily have one short and one long
;; line. We normally choose the short line (in other words, we prefer
;; stretching) unless the "Prefer Shrink" option is checked.

;; Note that our notion of "fit" is different from that of Donald Knuth. In
;; the Knuth-Plass paper, what he calls "first fit" is probably the Duncan
;; algorithm, choosing the first solution that stays close to the natural
;; inter-word space.

;; #### TODO: maybe we could think of other potential weight functions for the
;; Best/Justified version, and provide a choice? See for instance some ideas
;; in the Fixed algorithm's comment section.

;; #### TODO: add a tolerance parameter. But a better way of doing it, again,
;; would be to parametrize by a full-blown cost function, which could then be
;; selected to be the Knuth-Plass one.


(in-package :etap)


;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *fit-variants*
  '(:first :best :last))

(defparameter *fit-variants-help-keys*
  '(:fit-variant-first :fit-variant-best :fit-variant-last))

(defparameter *fit-fallbacks*
  '(:underfull :anyfull :overfull))

(defparameter *fit-fallbacks-help-keys*
  '(:fit-fallback-underfull :fit-fallback-anyfull :fit-fallback-overfull))

(defparameter *fit-discriminating-functions*
  '(:minimize-distance :minimize-scaling))

(defparameter *fit-discriminating-functions-help-keys*
  '(:fit-discriminating-function-minimize-distance
    :fit-discriminating-function-minimize-scaling))

(defparameter *fit-options*
  '((:avoid-hyphens t) (:prefer-overfulls t) (:relax t) (:prefer-shrink t)))

(defparameter *fit-options-help-keys*
  '(:fit-option-avoid-hyphens :fit-option-prefer-overfulls :fit-option-relax
    :fit-option-prefer-shrink))

(defparameter *fit-tooltips*
  '(:fit-variant-first "Prefer lines with fewer words (more stretch)."
    :fit-variant-best "Minimize scaling."
    :fit-variant-last "Prefer lines with more words (more shrink)."
    :fit-fallback-underfull "Always prefer underfull lines."
    :fit-fallback-anyfull "Prefer lines closer to the paragraph
width, whether underfull or overfull."
    :fit-fallback-overfull "Always prefer overfull lines."
    :fit-discriminating-function-minimize-distance
    "In the Best/Justified version,
minimize the distance to natural spacing
for equally good solutions."
    :fit-discriminating-function-minimize-scaling
    "In the Best/Justified version,
minimize the spacing adjustment
ratio for equally good solutions."
    :fit-option-avoid-hyphens "Avoid hyphenating words when possible."
    :fit-option-prefer-overfulls
    "In the Anyfull fallback, when the underfull and
overfull lines are equally distant from the
paragraph's width (modulo the offset), and
after the Avoid Hyphens option has been taken
into account, choose the overfull rather than
the underfull one."
    :fit-option-relax "For the First and Last variants in ragged dispositions,
de-stretch or de-shrink lines afterwards."
    :fit-option-prefer-shrink "In the Best/Justified version,
prefer shrinking over stretching
for equally good solutions."))


(define-global-variables variant hyphen-penalty explicit-hyphen-penalty
  line-penalty fallback width-offset avoid-hyphens prefer-overfulls relax
  prefer-shrink discriminating-function)


(defmacro define-fit-caliber (name min default max &optional infinity)
  "Define a NAMEd Fit caliber.
See `define-caliber' for more information."
  `(define-caliber fit ,name ,min ,default ,max ,infinity))

;; #### NOTE: the LINE-PENALTY parameter has no impact on the algorithm, since
;; it's a constant which affects all line endings in the same way. It's just
;; here so that we can compute the same local demerits as in the Knuth-Plass,
;; and hence compare the two. Hopefully, this mess will go away when we
;; parametrize the cost function.
(define-fit-caliber line-penalty 0 10 100)
(define-fit-caliber hyphen-penalty -10000 50 10000 t)
(define-fit-caliber explicit-hyphen-penalty -10000 50 10000 t)
;; #### NOTE: no final-hyphen-demerits because that would not be a *-fit
;; algorithm anymore (we would need to look one line ahead).
(define-fit-caliber width-offset -50 0 0)


(defmacro default-fit (name)
  "Default Fit NAMEd variable."
  `(default fit ,name))

(defmacro calibrate-fit (name)
  "Calibrate NAMEd Fit variable."
  `(calibrate fit ,name))




;; ==========================================================================
;; HList
;; ==========================================================================

(defmethod process-hlist
    (hlist disposition (algorithm (eql :fit))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Finish setting up hyphenation points.
This means defaulting their penalties and initializing the corresponding
caliber.
Return HLIST."
  (calibrate-fit hyphen-penalty)
  (calibrate-fit explicit-hyphen-penalty)
  (mapc (lambda (item)
	  (when (hyphenation-point-p item)
	    (cond ((explicitp item)
		   (setf (penalty item) *explicit-hyphen-penalty*)
		   (setf (slot-value item 'caliber)
			 *fit-explicit-hyphen-penalty*))
		  (t
		   (setf (penalty item) *hyphen-penalty*)
		   (setf (slot-value item 'caliber)
			 *fit-hyphen-penalty*)))))
    hlist)
  hlist)




;; ==========================================================================
;; Boundaries
;; ==========================================================================

;; #### TODO: the min and max widths here (in fact, the width one as well) are
;; "natural" widths. Nothing prevents an algorithm to stretch a line more than
;; it's max width (in fact the Knuth-Plass does so with its tolerance
;; setting). So perhaps the design is not very nice for algorithms with more
;; flexibility, and those values should in fact represent the algorithm's
;; tolerance instead.

;; #### TODO: as for the TSAR property, it represents the amount required to
;; reach a target width, /not/ an algorithm's final choice. Also, this really
;; makes sense for justification, much less for ragged dispositions. But on
;; the other hand, we don't currently have any sensible ragged formatting.

(defclass fit-boundary (fixed-boundary)
  ((min-width
    :documentation "This boundary's theoretical minimum line width."
    :initarg :min-width :reader min-width)
   (max-width
    :documentation "This boundary's theoretical maximum line width."
    :initarg :max-width :reader max-width)
   (tsar
    :documentation "This boundary's Theoretical Spacing Adjustment Ratio."
    :reader tsar))
  (:documentation "The FIT-BOUNDARY class."))

(defmethod initialize-instance :after
    ((boundary fit-boundary)
     ;; #### NOTE: the Knuth-Plass' emergency-stretch amount is passed as
     ;; EXTRA stretch here.
     &key width stretch shrink extra target)
  "Initialize BOUNDARY's TSAR."
  (when extra (setq stretch ($+ stretch extra)))
  (setf (slot-value boundary 'tsar)
	;; #### NOTE: the WIDTH slot from the FIXED-BOUNDARY superclass is
	;; already initialized by now, but we're still saving a reader call by
	;; using the propagated WIDTH keyword argument.
	(sar width target stretch shrink)))

(defmethod properties strnlcat ((boundary fit-boundary) &key)
  "Return a string advertising Fit BOUNDARY's natural dimensions.
This includes the minimum, natural, and maximum theoretical line widths,
plus the theoretical SAR required to reach the target width."
  (format nil "Min: ~Apt; Max: ~Apt.~%Theoretical SAR: ~A."
    (float (min-width boundary))
    ($float (max-width boundary))
    ($float (tsar boundary))))


(defclass fit-weighted-boundary (fit-boundary)
  ((weight :documentation "This boundary's weight."
	   :initarg :weight
	   :accessor weight)
   (possibilities
    :documentation "The number of possibilities for ending this line."
    :initarg :possibilities
    :reader possibilities))
  (:documentation "The Fit algorithm's weighted boundary class."))

(defmethod properties strnlcat ((boundary fit-weighted-boundary) &key)
  "Return a string advertising Fit Weighted BOUNDARY's properties.
This includes the boundary's weight and the number possibilities to end this
line."
  (format nil "Weight: ~A, best of ~A choice~:P."
    ($float (weight boundary))
    (possibilities boundary)))


;; ---------------
;; Boundary lookup
;; ---------------

;; This function collects boundaries between the last underfull (included) and
;; the first overfull (included), regardless of their hyphenation status.
;; That's because getting as close to the paragraph's width takes precedence
;; in justified disposition.
(defun fit-get-justified-boundaries (harray bol width)
  "Return the boundaries for a justified HARRAY line of WIDTH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This function is used by the Fit algorithm and returns three values:
- a (possibly empty) list of fit boundaries from last to first,
- the last underfull boundary, or NIL,
- the first overfull boundary, or NIL."
  (loop :with underfull :with fits := (list) :with overfull
	:with continue := t
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol continue)
	:do (when ($< (penalty eol) +∞)
	      (when (eq (penalty eol) -∞) (setq continue nil))
	      (let ((boundary (make-instance 'fit-boundary
				:harray harray :bol bol :break-point eol
				:target width)))
		(cond (($< (max-width boundary) width)
		       (setq underfull boundary))
		      ((> (min-width boundary) width)
		       (setq overfull boundary continue nil))
		      (t
		       (push boundary fits))))) ;; note the reverse order
	:finally (return (values fits underfull overfull))))


(defun first/last-fit-get-justified-boundary (harray bol width)
  "Return the boundary for a justified HARRAY line of WITH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This is the First/Last Fit algorithm version."
  (multiple-value-bind (fits underfull overfull)
      (fit-get-justified-boundaries harray bol width)
    (cond (fits
	   (when *avoid-hyphens*
	     (setq fits (loop :for boundary :in fits
			      :if (hyphenated boundary)
				:collect boundary :into hyphens
			      :else
				:collect boundary :into others
			      :finally (return (or others hyphens)))))
	   (ecase *variant*
	     (:first (car (last fits)))
	     (:last (first fits))))
	  (t
	   (fixed-fallback-boundary
	    underfull overfull (+ width *width-offset*))))))

(defun best-fit-get-justified-boundary (harray bol width)
  "Return the boundary for a justified HARRAY line of WITH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This is the  Best Fit algorithm version."
  (multiple-value-bind (fits underfull overfull)
      (fit-get-justified-boundaries harray bol width)
    (cond ((and fits (null (cdr fits)))
	   (first fits))
	  (fits
	   ;; #### NOTE: since we're only working with fits here, the badness
	   ;; can only be numerical (not infinite). Also, there is at most one
	   ;; boundary with a penalty of -∞ (because of the way we collect
	   ;; boundaries). This means that we can end up with at most one
	   ;; infinitely negative weight below.
	   (let ((possibilities (length fits)))
	     (mapc (lambda (fit)
		     (change-class fit 'fit-weighted-boundary
		       :weight (local-demerits (sar-badness (tsar fit))
					       (penalty fit)
					       *line-penalty*)
		       :possibilities possibilities))
	       fits))
	   ;; Note the use of $< and EQL here, because we can have (at most)
	   ;; one infinitely negative weight.
	   (setq fits (stable-sort fits #'$< :key #'weight))
	   (setq fits (retain (weight (first fits)) fits
			:test #'eql :key #'weight))
	   (when (cdr fits)
	     ;; We have two or more equal weights, so they can only be
	     ;; numerical.
	     (let ((new-weight
		     (ecase *discriminating-function*
		       (:minimize-distance
			(lambda (fit) (abs (- width (width fit)))))
		       (:minimize-scaling
			(lambda (fit) (abs (tsar fit)))))))
	       (mapc (lambda (fit)
		       (setf (weight fit) (funcall new-weight fit)))
		 fits))
	     (setq fits (stable-sort fits #'< :key #'weight))
	     (setq fits (retain (weight (first fits)) fits
				:test #'= :key #'weight)))
	   (cond ((cdr fits)
		  (assert (= (length fits) 2))
		  (if *prefer-shrink* (first fits) (second fits)))
		 (t (first fits))))
	  (t
	   (fixed-fallback-boundary
	    underfull overfull (+ width *width-offset*))))))

;; In this function, we stop at the first word overfull even if we don't have
;; an hyphen overfull yet, because the Avoid Hyphens options would have no
;; effect. On the other hand, if we already have an hyphen overfull, it's
;; still important to collect a word overfull if possible, because of that
;; very same option.
(defun fit-get-ragged-boundary (harray bol width get-width)
  "Return the boundary for a ragged HARRAY line of WIDTH starting at BOL.
Return NIL if BOL is already at the end of HARRAY.
This is the Fit algorithm version.
GET-WIDTH is called to read the appropriate width (max width for the first
fit, natural width for the best fit, and min width for thew last fit)."
  (loop :with underfull :with underword :with fit :with overfull :with overword
	:with continue := t
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol continue)
	:do (when ($< (penalty eol) +∞)
	      (when (eq (penalty eol) -∞) (setq continue nil))
	      (let* ((boundary (make-instance 'fit-boundary
				 :harray harray :bol bol :break-point eol
				 :target width))
		     ;; Note that we already had EOL to figure this out, but
		     ;; it's more readable like that.
		     (hyphenated (hyphenated boundary)))
		(cond (($< (funcall get-width boundary) width)
		       ;; Track the last underfulls because they're the
		       ;; closest to WIDTH.
		       (setq underfull boundary)
		       (unless hyphenated (setq underword boundary)))
		      (($= (funcall get-width boundary) width)
		       (setq fit boundary))
		      (t
		       ;; Track the first overfulls because they're the
		       ;; closest to WIDTH.
		       (unless overfull (setq overfull boundary))
		       ;; No check required here because we stop at the first
		       ;; word overfull anyway.
		       (unless hyphenated
			 (setq overword boundary continue nil))))))
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
				     (+ width *width-offset*)
				     get-width)
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
				     (+ width *width-offset*)
				     get-width)
				    (fixed-fallback-boundary
				     underfull overfull
				     (+ width *width-offset*)
				     get-width)))
		      (:overfull (or overword overfull underfull))))
		   (t
		    ;; We don't care about hyphens. Choose the best solution.
		    (fixed-fallback-boundary
		     underfull overfull (+ width *width-offset*) get-width))))))




;; ==========================================================================
;; Lines
;; ==========================================================================

;; By default, lines are stretched as much as possible.
(defun first-fit-make-ragged-line (harray bol boundary width &aux (asar 1))
  "First Fit version of `make-line' for ragged lines."
  (when *relax*
    (setq asar
	  (if (eopp boundary)
	    ;; There is no constraint on destretching the last line.
	    0
	    ;; On the other hand, do not destretch any other line so much that
	    ;; another chunk would fit in.
	    (let ((sar (harray-sar
			harray
			(bol-idx bol)
			(eol-idx
			 (next-break-point harray (break-point boundary)))
			width)))
	      ;; A positive next SAR means that another chunk would fit in,
	      ;; and still be underfull (possibly not even elastic), so we can
	      ;; destretch only up to that (infinity falling back to 0).
	      ;; Otherwise, we can destretch completely.
	      (if ($> sar 0) sar 0)))))
  (make-line harray bol boundary :asar asar))

;; By default, lines are shrunk as much as possible.
(defun last-fit-make-ragged-line (harray bol boundary width &aux (asar -1))
  "Last Fit version of `make-line' for ragged lines."
  (when *relax*
    ;; There is no specific case for the last line here, because we only
    ;; deshrink up to the line's natural width.
    ;; #### WARNING: we're manipulating fixed boundaries here, so there's no
    ;; calling (TSAR BOUNDARY).
    (setq asar (let ((tsar (harray-sar
			    harray (bol-idx bol) (eol-idx boundary) width)))
		 ;; - A positive TSAR means that the line is naturally
		 ;;   underfull (maybe not even elastic), so we can deshrink
		 ;;   completely.
		 ;; - A TSAR between -1 and 0 means that the line can fit,
		 ;;   so we can deshrink up to that.
		 ;; - Finally, a TSAR < -1 means that the line cannot fit at
		 ;;   all, so we must stay at our original -1.
		 (if ($>= tsar 0) 0 ($max tsar -1)))))
  (make-line harray bol boundary :asar asar))

(defun fit-make-justified-line
    (harray bol boundary overstretch overshrink)
  "Fit version of `make-line' for justified lines."
  (multiple-value-bind (asar esar)
      (if (eopp boundary)
	;; The last line, which almost never fits exactly, needs a special
	;; treatment. Without paragraph-wide considerations, we want its
	;; scaling to be close to the general effect of the selected variant.
	(ecase *variant*
	  ;; #### FIXME: I think this is all wrong! Review.
	  (:first
	   ;; If the line needs to be shrunk, shrink it. Otherwise, stretch as
	   ;; much as possible, without overstretching.
	   (sars (tsar boundary) :overshrink overshrink))
	  (:best
	   ;; If the line needs to be shrunk, shrink it. Otherwise, keep the
	   ;; normal spacing.
	   (sars (tsar boundary) :overshrink overshrink :stretch-tolerance 0))
	  (:last
	   ;; Shrink as much as possible.
	   (sars -1 :overshrink overshrink)))
	(sars (tsar boundary)
	  :overshrink overshrink :overstretch overstretch))
    (make-line harray bol boundary :asar asar :esar esar)))




;; ==========================================================================
;; Breakup
;; ==========================================================================

(defmethod break-harray
    (harray disposition width (algorithm (eql :fit))
     &key ((:variant *variant*))
	  ((:line-penalty *line-penalty*))
	  ((:fallback *fallback*))
	  ((:width-offset *width-offset*))
	  ((:avoid-hyphens *avoid-hyphens*))
	  ((:prefer-overfulls *prefer-overfulls*))
	  ((:relax *relax*))
	  ((:prefer-shrink *prefer-shrink*))
	  ((:discriminating-function *discriminating-function*))
     &aux (disposition-type (disposition-type disposition))
	  (disposition-options (disposition-options disposition)))
  "Break HARRAY with the Fit algorithm."
  (default-fit variant)
  (calibrate-fit line-penalty)
  (default-fit fallback)
  (calibrate-fit width-offset)
  (default-fit discriminating-function)
  (let ((get-boundary
	  (case disposition-type
	    (:justified
	     (case *variant*
	       (:best #'best-fit-get-justified-boundary)
	       (t     #'first/last-fit-get-justified-boundary)))
	    (t
	     (let ((get-width (ecase *variant*
				(:first #'max-width)
				(:best #'width)
				(:last #'min-width))))
	       (lambda (harray bol width)
		 (fit-get-ragged-boundary harray bol width get-width))))))
	(make-line
	  (case disposition-type
	    (:justified
	     (let ((overstretch (getf disposition-options :overstretch))
		   (overshrink  (getf disposition-options :overshrink)))
	       (lambda (harray bol boundary)
		 (fit-make-justified-line
		  harray bol boundary overstretch overshrink))))
	    (t
	     (ecase *variant*
	       (:first
		(lambda (harray bol boundary)
		  (first-fit-make-ragged-line harray bol boundary width)))
	       (:best
		#'make-line)
	       (:last
		(lambda (harray bol boundary)
		  (last-fit-make-ragged-line harray bol boundary width))))))))
    (make-greedy-breakup harray disposition width get-boundary make-line)))
