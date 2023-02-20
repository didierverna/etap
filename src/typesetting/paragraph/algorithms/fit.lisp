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
    :fit-option-avoid-hyphens "Except for the Best/Justified version,
avoid hyphenating words when possible."
    :fit-option-prefer-overfulls "In the Best/Justified version,
prefer overfull over underfull
for equally bad solutions."
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



(defun fit-weight
    (lineup start width boundary hyphen-penalty explicit-hyphen-penalty
     &aux (badness (badness lineup start (stop-idx boundary) width)))
  "Return the weight of LINEUP chunk between START and BOUNDARY.
The weight is calculated in the TeX way, that is, badness plus possible hyphen
penalty."
  (if (discretionaryp (stop-elt boundary))
    ;; #### NOTE: infinitely negative hyphen penalties have already been
    ;; handled by an immediate RETURN from FIT-LINE-BOUNDARY, so there's no
    ;; risk of doing -∞ + +∞ here.
    (++ badness
	(if (explicitp (stop-elt boundary))
	  explicit-hyphen-penalty
	  hyphen-penalty))
    badness))

(defun fit-weights
    (lineup start width boundaries hyphen-penalty explicit-hyphen-penalty)
  "Compute the weights of LINEUP chunks between START and BOUNDARIES.
Return a list of the form ((WEIGHT . BOUNDARY) ...)."
  (mapcar
      (lambda (boundary)
	(cons (fit-weight lineup start width boundary
			  hyphen-penalty explicit-hyphen-penalty)
	      boundary))
    boundaries))

(defun fit-deltas (lineup start width boundaries)
  "Compute the deltas of LINEUP chunks between START and BOUNDARIES.
This means the difference (in absolute value) between WIDTH and each chunk's
natural width. Return a list of the form ((DELTA . BOUNDARY) ...)."
  (mapcar (lambda (boundary)
	    (cons (abs (- width
			  (lineup-width lineup start (stop-idx boundary))))
		  boundary))
    boundaries))

;; #### WARNING: this only works for elastic lines!
(defun fit-scales (lineup start width boundaries)
  "Compute the scales of LINEUP chunks between START and BOUNDARIES.
This means the scaling required for a chunk to reach WIDTH, in absolute value.
Return a list of the form ((SCALE . BOUNDARY) ...)."
  (mapcar (lambda (boundary)
	    (cons (abs (lineup-scale lineup start (stop-idx boundary) width))
		  boundary))
    boundaries))

(defun word-boundaries (boundaries)
  "Select only word boundaries from BOUNDARIES."
  (remove-if (lambda (boundary) (discretionaryp (stop-elt boundary)))
      boundaries))

(defun hyphen-boundaries (boundaries)
  "Select only hyphen boundaries from BOUNDARIES."
  (remove-if-not (lambda (boundary) (discretionaryp (stop-elt boundary)))
      boundaries))

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
    (loop :with underfull :with underwidth
	  :with fits := (list)
	  :with overfull :with overwidth
	  ;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH
	  ;; NIL) first, and then NIL.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (stop-idx boundary))
	  :while (and boundary (not overfull))
	  :for span := (lineup-span lineup start (stop-idx boundary))
	  :if (< (max-width span) width)
	    :do (setq underfull boundary underwidth (max-width span))
	  :else :if (and (<= (min-width span) width)
			 (>= (max-width span) width))
	    :do (push boundary fits) ;; note the reverse order!
	  :else
	    :do (setq overfull boundary overwidth (min-width span))
	  :finally (return
		     (cond (fits
			    (when avoid-hyphens
			      (setq fits (or (word-boundaries fits)
					     (hyphen-boundaries fits))))
			    (ecase variant
			      (:first (car (last fits)))
			      (:last (car fits))))
			   (t
			    (fixed-fallback-boundary
			     underfull underwidth overfull overwidth
			     (+ width width-offset)
			     prefer-overfulls fallback avoid-hyphens))))))
  (:method (lineup start width (variant (eql :best))
	    &key hyphen-penalty explicit-hyphen-penalty
		 discriminating-function prefer-shrink 
		 fallback width-offset avoid-hyphens prefer-overfulls)
    "Find a Best Fit boundary for the justified disposition."
    ;; #### NOTE: below, we collect boundaries in reverse order because we
    ;; will often need to access the most recent ones, and we use STABLE-SORT
    ;; to preserve that order.
    (loop :with underfull :with underwidth
	  :with fits := (list)
	  :with overfull :with overwidth
	  ;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH
	  ;; NIL) first, and then NIL.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (stop-idx boundary))
	  :while (and boundary (not overfull))
	  :for span := (lineup-span lineup start (stop-idx boundary))
	  :when (or (not (discretionaryp (stop-elt boundary)))
		    (and (explicitp (stop-elt boundary))
			 (<< explicit-hyphen-penalty +∞))
		    (and (not (explicitp (stop-elt boundary)))
			 (<< hyphen-penalty +∞)))
	    :if (and (discretionaryp (stop-elt boundary))
		     (or (and (explicitp (stop-elt boundary))
			      (eq explicit-hyphen-penalty -∞))
			 (and (not (explicitp (stop-elt boundary)))
			      (eq hyphen-penalty -∞))))
	      :do (return boundary)
	    :else :if (< (max-width span) width)
	      :do (setq underfull boundary underwidth (max-width span))
	    :else :if (and (<= (min-width span) width)
			   (>= (max-width span) width))
	      :do (push boundary fits) ;; note the reverse order!
	    :else
	      :do (setq overfull boundary overwidth (min-width span))
	  :finally
	     (return
	       (cond (fits
		      (let ((sorted-weights
			      (stable-sort
			       (fit-weights lineup start width fits
					    hyphen-penalty explicit-hyphen-penalty)
			       #'<< :key #'car)))
			(cond ((eql (caar sorted-weights) (caadr sorted-weights))
			       (setq sorted-weights
				     (remove-if-not
					 (lambda (weight)
					   (= weight (caar sorted-weights)))
					 sorted-weights
				       :key #'car))
			       (let ((sorted-scores
				       (stable-sort
					(funcall
					    (ecase discriminating-function
					      (:minimize-distance #'fit-deltas)
					      (:minimize-scaling #'fit-scales))
					  lineup start width
					  (mapcar #'cdr sorted-weights))
					#'< :key #'car)))
				 (cond ((= (caar sorted-scores)
					   (caadr sorted-scores))
					(setq sorted-scores
					      (remove-if-not
						  (lambda (delta)
						    (= delta (caar sorted-scores)))
						  sorted-scores
						:key #'car))
					(assert (= (length sorted-scores) 2))
					(if prefer-shrink
					  (cdar sorted-scores)
					  (cdadr sorted-scores)))
				       (t
					(cdar sorted-scores)))))
			      (t
			       (cdar sorted-weights)))))
		     (t
		      (fixed-fallback-boundary
		       underfull underwidth overfull overwidth
		       (+ width width-offset)
		       prefer-overfulls fallback avoid-hyphens)))))))


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
	   ;; If the line needs to be shrunk, shrink it. Otherwise, keep the
	   ;; normal spacing.
	   (if (and scale (< scale 0))
	     (make-line lineup start stop scale)
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

(defmethod make-lines
    (lineup disposition width (algorithm (eql :fit))
     &key variant fallback
	  width-offset avoid-hyphens prefer-overfulls relax prefer-shrink
	  discriminating-function hyphen-penalty explicit-hyphen-penalty 
     &aux (get-line-boundary
	   (if (eq (disposition-type disposition) :justified)
	     (lambda (start)
	       (fit-justified-line-boundary lineup start width variant
		 :hyphen-penalty hyphen-penalty
		 :explicit-hyphen-penalty explicit-hyphen-penalty
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
  (calibrate-fit hyphen-penalty t)
  (calibrate-fit explicit-hyphen-penalty t)
  (loop :for start := 0 :then (next-start boundary)
	:while start
	:for boundary := (funcall get-line-boundary start)
	:collect (apply #'fit-make-line lineup start (stop-idx boundary)
			(disposition-type disposition) variant
			:width width
			:relax relax
			:last (null (stop-elt boundary))
			(disposition-options disposition))))
