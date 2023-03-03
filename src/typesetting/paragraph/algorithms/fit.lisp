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
;; the same way. In fact, in ragged dispositions, the Best Fit and the Fixed
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
;; - For the First Fit, lines are de-stretched as much as possible, but not to
;;   the point that another chunk would fit in. The effect is thus to make the
;;   paragraph more compact.
;; - For the Last Fit, lines are de-shrunk as much as possible towards the
;;   natural inter-word space, without producing overfulls (already overfull
;;   lines cannot be deshrunk, as the overfull would be even more important).
;;   The effect is thus to make the paragraph less compact.

;; The "[Explicit ]Hyphen Penalty" option affects the Best Fit in justified
;; disposition in the TeX way when weighting solutions. In particular, this
;; means that infinite penalties are understood, leading to either forbidden
;; or mandatory discretionary breaks. Note however that we only use TeX's
;; badness to weight actual solutions, so the badness itself can never be
;; infinite.

;; In this scenario, several solutions may turn out to have the same weight,
;; meaning different boundaries with or without varying hyphen penalties. The
;; "Discriminating Function" is used to break the tie. For "Minimize
;; Distance", we minimize the difference between the natural width of the line
;; and the desired one. For "minimize Scaling", we minimize the scaling ratio.
;; The difference is that in the second case, we take the frequent difference
;; between maximum stretching and shrinking into account. If this leads to
;; equality again, then we necessarily have one short and one long line. We
;; normally choose the short line (in other words, we prefer stretching)
;; unless the "Prefer Shrink" option is checked.

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


(defclass fit-boundary (boundary)
  ((span :documentation "The span of the line ending at this boundary."
	 :reader span))
  (:documentation "The FIT-BOUNDARY class.
A fit boundary stores the computed span of the line ending there."))

(defmethod initialize-instance :after
    ((boundary fit-boundary) &key lineup start)
  "Compute the span of LINEUP line from START to BOUNDARY."
  (setf (slot-value boundary 'span)
	(lineup-span lineup start (stop-idx boundary))))


(defclass fit-weighted-boundary (fit-boundary)
  ((weight :documentation "This boundary's weight."
	   :initarg :weight :accessor weight))
  (:documentation "The Fit algorithm's weighted boundary class."))


(defun word-boundaries (boundaries)
  "Select only word boundaries from BOUNDARIES."
  (remove-if #'discretionaryp boundaries :key #'item))

(defun hyphen-boundaries (boundaries)
  "Select only hyphen boundaries from BOUNDARIES."
  (remove-if-not #'discretionaryp boundaries :key #'item))

;; #### NOTE: this function only collects the last underfull and the first
;; overfull, regardless of their word / hyphen nature (that's because getting
;; as close to the paragraph's width takes precedence in justified
;; disposition).
(defgeneric fit-justified-line-boundary
    (lineup start width variant &key &allow-other-keys)
  (:documentation
   "Return the Fit algorithm's view of the end of a justified line boundary.")
  (:method (lineup start width variant
	    &key fallback width-offset avoid-hyphens prefer-overfulls)
    "Find a First or Last Fit boundary for the justified disposition."
    (loop :with underfull :with fits := (list) :with overfull
	  :for boundary
	    := (next-boundary lineup start 'fit-boundary :start start)
	      :then (next-boundary lineup (stop-idx boundary) 'fit-boundary
				   :start start)
	  :while (and boundary (not overfull))
	  :if (< (max-width (span boundary)) width)
	    :do (setq underfull (change-class boundary 'fixed-boundary
				  :width (max-width (span boundary))))
	  :else :if (and (<= (min-width (span boundary)) width)
			 (>= (max-width (span boundary)) width))
	    :do (push boundary fits) ;; note the reverse order!
	  :else
	    :do (setq overfull (change-class boundary 'fixed-boundary
				 :width (min-width (span boundary))))
	  :finally
	     (return
	       (cond (fits
		      (when avoid-hyphens
			(setq fits (or (word-boundaries fits)
				       (hyphen-boundaries fits))))
		      (ecase variant
			(:first (car (last fits)))
			(:last (first fits))))
		     (t
		      (fixed-fallback-boundary
		       underfull overfull
		       (+ width width-offset) prefer-overfulls
		       fallback avoid-hyphens))))))
  (:method (lineup start width (variant (eql :best))
	    &key discriminating-function prefer-shrink
		 fallback width-offset avoid-hyphens prefer-overfulls)
    "Find a Best Fit boundary for the justified disposition."
    ;; #### NOTE: below, we collect boundaries in reverse order because we
    ;; will often need to access the most recent ones, and we use STABLE-SORT
    ;; to preserve that order.
    (loop :with underfull :with fits := (list) :with overfull
	  :with continue := t
	  :for boundary
	    := (next-boundary lineup start 'fit-boundary :start start)
	      :then (next-boundary lineup (stop-idx boundary) 'fit-boundary
				   :start start)
	  :while continue
	  :do (when (<< (penalty (item boundary)) +∞)
		(when (eq (penalty (item boundary)) -∞) (setq continue nil))
		(cond ((< (max-width (span boundary)) width)
		       (setq underfull (change-class boundary 'fixed-boundary
					 :width (max-width (span boundary)))))
		      ((and (<= (min-width (span boundary)) width)
			    (>= (max-width (span boundary)) width))
		       (push boundary fits)) ;; note the reverse order!
		      (t
		       (setq continue nil
			     overfull (change-class boundary 'fixed-boundary
					:width (min-width (span boundary)))))))
	  :finally
	     (return
	       (cond ((and fits (not (cdr fits)))
		      (car fits))
		     (fits
		      (flet ((weight (fit)
			       "\
Return the weight of LINEUP chunk between START and BOUNDARY.
The weight is calculated in the TeX way, that is, badness plus possible hyphen
penalty."
			       ;; #### NOTE: infinitely negative hyphen
			       ;; penalties have already been handled by an
			       ;; immediate RETURN from FIT-LINE-BOUNDARY, so
			       ;; there's no risk of doing -∞ + +∞ here.
			       (++ (badness lineup start (stop-idx fit) width)
				   (penalty (item fit)))))
			(mapc (lambda (fit)
				(change-class fit 'fit-weighted-boundary
				  :weight (weight fit)))
			  fits))
		      (flet ((sort-fits ()
			       "Stable sort fits by weight."
			       (setq fits
				     (stable-sort fits #'<< :key #'weight)))
			     (keep-best-fits (&aux (best (weight (first fits))))
			       "Keep only fits as good as the first one."
			       (setq fits
				     (remove-if-not
					 (lambda (weight) (= weight best))
					 fits :key #'weight))))
			(sort-fits)
			(cond ((eql (weight (first fits))
				    (weight (second fits)))
			       (keep-best-fits)
			       (let ((new-weight
				       (ecase discriminating-function
					 (:minimize-distance
					  (lambda (fit)
					    (abs
					     (- width
						(normal-width (span fit))))))
					 (:minimize-scaling
					  (lambda (fit)
					    (abs
					     (lineup-scale
					      lineup start (stop-idx fit)
					      width)))))))
				 (mapc
				     (lambda (fit)
				       (setf (weight fit)
					     (funcall new-weight fit)))
				   fits))
			       (sort-fits)
			       (cond ((= (weight (first fits))
					 (weight (second fits)))
				      (keep-best-fits)
				      (assert (= (length fits) 2))
				      (if prefer-shrink
					(first fits)
					(second fits)))
				     (t
				      (first fits))))
			      (t
			       (first fits)))))
		     (t
		      (fixed-fallback-boundary
		       underfull overfull
		       (+ width width-offset) prefer-overfulls fallback
		       avoid-hyphens)))))))


(defgeneric fit-make-line
    (lineup start stop disposition variant &key &allow-other-keys)
  (:documentation "Make a Fit line from LINEUP chunk between START and STOP.")
  (:method (lineup start stop disposition (variant (eql :first))
	    &key width relax last
	    ;; By default, lines are stretched as much as possible.
	    &aux (scale 1))
    "Make a ragged first-fit line from LINEUP chunk between START and STOP."
    (when relax
      (setq scale
	    (if last
	      ;; There is no constraint on destretching the last line.
	      0
	      ;; On the other hand, do not destretch any other line so much
	      ;; that another chunk would fit in.
	      (let ((scale
		      (lineup-scale
		       lineup start (stop-idx (next-boundary lineup stop))
		       width)))
		;; A positive scale means that another chunk would fit in, and
		;; still be underfull, so we can destretch only up to that.
		;; Otherwise, we can destretch completely.
		(if (and scale (> scale 0)) scale 0)))))
    (make-line lineup start stop scale))
  (:method (lineup start stop disposition (variant (eql :best)) &key)
    "Make a ragged best-fit line from LINEUP chunk between START and STOP."
    (make-line lineup start stop))
  (:method (lineup start stop disposition (variant (eql :last))
	    &key width relax
	    ;; By default, lines are shrunk as much as possible.
	    &aux (scale -1))
    "Make a ragged last-fit line from LINEUP chunk between START and STOP."
    (when relax
      ;; There is no specific case for the last line here, because we only
      ;; deshrink up to the line's natural width.
      (setq scale (let ((scale (lineup-scale lineup start stop width)))
		    ;; A positive scale means that the line is naturally
		    ;; underfull, so we can deshrink completely.
		    ;; A scale between -1 and 0 means that the line can fit,
		    ;; so we can deshrink p to that.
		    ;; Finally, a scale < -1 means that the line is cannot fit
		    ;; at all, so we must stay at our original -1.
		    (if (>= scale 0) 0 (max scale -1)))))
    (make-line lineup start stop scale))
  (:method (lineup start stop (disposition (eql :justified)) variant
	    &key width last sloppy)
    "Make a justified Fit line from LINEUP chunk between START and STOP."
    (if last
      ;; The last line, which almost never fits exactly, needs a special
      ;; treatment. Without paragraph-wide considerations, we want its scaling
      ;; to be close to the general effect of the selected variant.
      (let ((scale (lineup-scale lineup start stop width)))
	(ecase variant
	  (:first
	   ;; Stretch as much as possible.
	   (make-line lineup start stop (if (and scale (< scale 1)) scale 1)))
	  (:best
	   ;; If the line needs to be shrunk, shrink it as much as possible.
	   ;; Otherwise, keep the normal spacing.
	   (if (and scale (< scale 0))
	     (make-line lineup start stop (max scale -1))
	     (make-line lineup start stop)))
	  (:last
	   ;; Shrink as much as possible.
	   (make-line lineup start stop -1))))
      (make-wide-line lineup start stop width sloppy))))


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
		lineup start width fallback  width-offset
		avoid-hyphens prefer-overfulls
		(ecase variant
		  (:first #'lineup-max-width)
		  (:best #'lineup-width)
		  (:last #'lineup-min-width)))))))
  "Typeset LINEUP with the Fit algorithm."
  (default-fit variant)
  (default-fit fallback)
  (calibrate-fit width-offset)
  (default-fit discriminating-function)
  (loop :for start := 0 :then (start-idx boundary)
	:while start
	:for boundary := (funcall get-line-boundary start)
	:collect (apply #'fit-make-line lineup start (stop-idx boundary)
			(disposition-type disposition) variant
			:width width
			:relax relax
			:last (null (item boundary))
			(disposition-options disposition))))
