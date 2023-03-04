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
;; TeX's "badness + hyphen penalty" to weight solutions. When there is no fit,
;; all versions fall back to the ragged (or Fixed algorithm) logic. That is,
;; they behave according to the settings of "Width Offset", "Avoid Hyphens",
;; and "Prefer Overfulls".

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
;; the scaling ratio. The difference is that in the second case, we take the
;; frequent difference between maximum stretching and shrinking into account.
;; If this leads to equality again, then we necessarily have one short and one
;; long line. We normally choose the short line (in other words, we prefer
;; stretching) unless the "Prefer Shrink" option is checked.

;; Note that our notion of "fit" is different from that of Donald Knuth. In
;; the Knuth-Plass paper, what he calls "first fit" is probably the Duncan
;; algorithm, choosing the first solution that stays close to the natural
;; inter-word space.

;; #### TODO: maybe we could think of other potential weight functions for the
;; Best/Justified version, and provide a choice? See for instance some ideas
;; in the Fixed algorithm's comment section.


(in-package :etap)


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


(defmacro define-fit-caliber (name min default max)
  "Define a NAMEd Fit caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber fit ,name ,min ,default ,max))

(define-fit-caliber hyphen-penalty -1000 50 1000)
(define-fit-caliber explicit-hyphen-penalty -1000 50 1000)
;; #### FIXME: the -50pt value below is somewhat arbitrary.
(define-fit-caliber width-offset -50 0 0)


(defclass fit-boundary (fixed-boundary)
  ((stretch :documentation "This boundary's line stretching capacity."
	    :initarg :stretch :reader stretch)
   (shrink :documentation "This boundary's line shrinking capacity."
	   :initarg :shrink :reader shrink)
   (min-width :documentation "This boundary's minimum line width."
	      :reader min-width)
   (max-width :documentation "This boundary's maximum line width."
	      :reader max-width))
  (:documentation "The FIT-BOUNDARY class."))

(defmethod initialize-instance :after
    ((boundary fit-boundary) &key width stretch shrink)
  "Compute BOUNDARY's minimum and maximum line widths."
  (setf (slot-value boundary 'min-width) (- width shrink))
  (setf (slot-value boundary 'max-width) (+ width stretch)))

(defun boundary-scale (boundary target)
  "Return the amount of scaling required to reach TARGET width from BOUNDARY.
See `scaling' for more information."
  (scaling (width boundary) target (stretch boundary) (shrink boundary)))


(defclass fit-weighted-boundary (fit-boundary)
  ((weight :documentation "This boundary's weight."
	   :initarg :weight :accessor weight))
  (:documentation "The Fit algorithm's weighted boundary class."))


;; This function collects boundaries between the last underfull (included) and
;; the first overfull (included), regardless of their hyphenation status.
;; That's because getting as close to the paragraph's width takes precedence
;; in justified disposition.
(defun fit-justified-line-boundaries (lineup start width)
  "Return possible boundaries for justification by the Fit algorithm.
Boundaries are collected for a LINEUP line beginning at START, and for a
paragraph of WIDTH.
This function returns three values:
- a (possibly empty) list of fit boundaries from last to first,
- the last underfull boundary or NIL,
- the first overfull boundary or NIL."
  (loop :with underfull :with fits := (list) :with overfull
	:with continue := t
	:for boundary
	  := (next-boundary lineup start 'fit-boundary :start start)
	    :then (next-boundary lineup (stop-idx boundary) 'fit-boundary
				 :start start)
	:while continue
	:do (when (<< (penalty (item boundary)) +∞)
	      (when (eq (penalty (item boundary)) -∞) (setq continue nil))
	      (cond ((< (max-width boundary) width)
		     (setq underfull boundary))
		    ((> (min-width boundary) width)
		     (setq overfull boundary continue nil))
		    (t
		     (push boundary fits)))) ;; note the reverse order
	:finally (return (values fits underfull overfull))))


(defun word-boundaries (boundaries)
  "Select only word boundaries from BOUNDARIES."
  (remove-if #'discretionaryp boundaries :key #'item))

(defun hyphen-boundaries (boundaries)
  "Select only hyphen boundaries from BOUNDARIES."
  (remove-if-not #'discretionaryp boundaries :key #'item))

(defgeneric fit-justified-line-boundary
    (lineup start width variant &key &allow-other-keys)
  (:documentation
   "Return the Fit algorithm's view of the end of a justified line boundary.")
  (:method (lineup start width variant
	    &key fallback width-offset avoid-hyphens prefer-overfulls)
    "Find a first-/last-fit boundary for the justified disposition."
    (multiple-value-bind (fits underfull overfull)
	(fit-justified-line-boundaries lineup start width)
      (cond (fits
	     (when avoid-hyphens
	       (setq fits (or (word-boundaries fits) (hyphen-boundaries fits))))
	     (ecase variant
	       (:first (car (last fits)))
	       (:last (first fits))))
	    (t
	     (fixed-fallback-boundary
	      underfull overfull
	      (+ width width-offset) prefer-overfulls
	      fallback avoid-hyphens)))))
  (:method (lineup start width (variant (eql :best))
            &key discriminating-function prefer-shrink
                 fallback width-offset avoid-hyphens prefer-overfulls)
    "Find a best-fit boundary for the justified disposition."
    (multiple-value-bind (fits underfull overfull)
	(fit-justified-line-boundaries lineup start width)
      (cond ((and fits (null (cdr fits)))
	     (first fits))
	    (fits
	     ;; #### NOTE: since we're only working with fits here, the
	     ;; badness can only be numerical (not infinite). Also, there is
	     ;; at most one boundary with a penalty of -∞ (because of the way
	     ;; we collect boundaries). This means that we can end up with at
	     ;; most one infinitely negative weight below.
	     (mapc (lambda (fit)
		     (change-class fit 'fit-weighted-boundary
		       :weight (++ (badness lineup start (stop-idx fit) width)
				   (penalty (item fit)))))
	       fits)
	     ;; Note the use of << and EQL here, because we can have (at most)
	     ;; one infinitely negative weight.
	     (setq fits (stable-sort fits #'<< :key #'weight))
	     (setq fits (retain (weight (first fits)) fits
			  :test #'eql :key #'weight))
	     (when (cdr fits)
	       ;; We have two or more equal weights, so they can only be
	       ;; numerical.
	       (let ((new-weight
		       (ecase discriminating-function
			 (:minimize-distance
			  (lambda (fit) (abs (- width (width fit)))))
			 (:minimize-scaling
			  (lambda (fit) (abs (boundary-scale fit width)))))))
		 (mapc (lambda (fit)
			 (setf (weight fit) (funcall new-weight fit)))
		   fits))
	       (setq fits (stable-sort fits #'< :key #'weight))
	       (setq fits (retain (weight (first fits)) fits
			    :test #'= :key #'weight)))
	     (cond ((cdr fits)
		    (assert (= (length fits) 2))
		    (if prefer-shrink (first fits) (second fits)))
		   (t (first fits))))
	    (t
	     (fixed-fallback-boundary
	      underfull overfull
	      (+ width width-offset) prefer-overfulls fallback
	      avoid-hyphens))))))


(defgeneric fit-make-line
    (lineup start boundary disposition variant &key &allow-other-keys)
  (:documentation
   "Make a Fit line from LINEUP chunk between START and BOUNDARY.")
  (:method (lineup start boundary disposition (variant (eql :first))
	    &key width relax
	    &aux (stop (stop-idx boundary))
	         ;; By default, lines are stretched as much as possible.
		 (scale 1))
    "Make a first-fit ragged line from LINEUP chunk between START and BOUNDARY."
    (when relax
      (setq scale
	    (if (last-boundary-p boundary)
	      ;; There is no constraint on destretching the last line.
	      0
	      ;; On the other hand, do not destretch any other line so much
	      ;; that another chunk would fit in.
	      (let ((scale (lineup-scale
			    lineup start (stop-idx (next-boundary lineup stop))
			    width)))
		;; A positive scale means that another chunk would fit in, and
		;; still be underfull, so we can destretch only up to that.
		;; Otherwise, we can destretch completely.
		(if (and scale (> scale 0)) scale 0)))))
    (make-line lineup start stop scale))
  (:method (lineup start boundary disposition (variant (eql :best)) &key)
    "Make a best-fit ragged line from LINEUP chunk between START and BOUNDARY."
    (make-line lineup start (stop-idx boundary)))
  (:method (lineup start boundary disposition (variant (eql :last))
	    &key width relax
	    &aux (stop (stop-idx boundary))
	         ;; By default, lines are shrunk as much as possible.
		 (scale -1))
    "Make a last-fit ragged line from LINEUP chunk between START and BOUNDARY."
    (when relax
      ;; There is no specific case for the last line here, because we only
      ;; deshrink up to the line's natural width.
      ;; #### WARNING: we're manipulating fixed boundaries here, so there's no
      ;; calling BOUNDARY-SCALE.
      (setq scale (let ((scale (lineup-scale lineup start stop width)))
		    ;; A positive scale means that the line is naturally
		    ;; underfull, so we can deshrink completely.
		    ;; A scale between -1 and 0 means that the line can fit,
		    ;; so we can deshrink up to that.
		    ;; Finally, a scale < -1 means that the line is cannot fit
		    ;; at all, so we must stay at our original -1.
		    (if scale (if (>= scale 0) 0 (max scale -1)) 0))))
    (make-line lineup start stop scale))
  (:method (lineup start boundary (disposition (eql :justified)) variant
	    &key width overstretch overshrink
	    &aux (stop (stop-idx boundary)))
    "Make an any-fit justified line from LINEUP chunk between START and STOP."
    (if (last-boundary-p boundary)
      ;; The last line, which almost never fits exactly, needs a special
      ;; treatment. Without paragraph-wide considerations, we want its scaling
      ;; to be close to the general effect of the selected variant.
      (let ((scale (boundary-scale boundary width)))
	(ecase variant
	  (:first
	   ;; If the line needs to be shrunk, shrink it. Otherwise, stretch as
	   ;; much as possible, without overstretching.
	   (when scale
	     (cond ((< scale 0)
		    (unless overshrink (setq scale (max scale -1))))
		   ((> scale 0)
		    (setq scale (min scale 1)))))
	   (make-line lineup start stop (or scale 0)))
	  (:best
	   ;; If the line needs to be shrunk, shrink it. Otherwise, keep the
	   ;; normal spacing.
	   (if (and scale (< scale 0))
	     (make-line lineup start stop (if overshrink scale (max scale -1)))
	     (make-line lineup start stop)))
	  (:last
	   ;; Shrink as much as possible.
	   (make-line lineup start stop
		      (if (and scale (< scale 0) overshrink) scale -1)))))
      (make-wide-line lineup start stop width overstretch overshrink))))


(defmacro calibrate-fit (name &optional infinity)
  "Calibrate NAMEd Fit variable."
  `(calibrate fit ,name ,infinity))

(defmacro default-fit (name)
  "Default Fit NAMEd variable."
  `(default fit ,name))

(defmethod make-lines :around
    (lineup disposition width (algorithm (eql :fit))
     &key hyphen-penalty explicit-hyphen-penalty)
  "Apply hyphen penalties to the lineup."
  (calibrate-fit hyphen-penalty t)
  (calibrate-fit explicit-hyphen-penalty t)
  (mapc (lambda (item)
	  (when (hyphenation-point-p item)
	    (setf (penalty item)
		  (if (explicitp item)
		    explicit-hyphen-penalty
		    hyphen-penalty))))
    lineup)
  (call-next-method))

(defmethod make-lines
    (lineup disposition width (algorithm (eql :fit))
     &key variant fallback
	  width-offset avoid-hyphens prefer-overfulls relax prefer-shrink
	  discriminating-function
     &aux (get-line-boundary
	   (if (eq (disposition-type disposition) :justified)
	     (lambda (start)
               (fit-justified-line-boundary lineup start width variant
                 :discriminating-function discriminating-function
                 :prefer-shrink prefer-shrink
                 :fallback fallback
                 :width-offset width-offset
                 :avoid-hyphens avoid-hyphens
                 :prefer-overfulls prefer-overfulls))
	     (lambda (start)
	       (fixed-ragged-line-boundary
		lineup start width
		fallback  width-offset avoid-hyphens prefer-overfulls
		(ecase variant
		  (:first :max) (:best :natural) (:last :min)))))))
  "Typeset LINEUP with the Fit algorithm."
  (default-fit variant)
  (default-fit fallback)
  (calibrate-fit width-offset)
  (default-fit discriminating-function)
  (loop :for start := 0 :then (start-idx boundary)
	:while start
	:for boundary := (funcall get-line-boundary start)
	:collect (apply #'fit-make-line lineup start boundary
			(disposition-type disposition) variant
			:width width
			:relax relax
			(disposition-options disposition))))
