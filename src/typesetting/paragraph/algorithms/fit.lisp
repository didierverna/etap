;; This is the classical *-fit algorithms family, making full use of
;; inter-word (elastic) glue. Lines are created sequentially, without
;; look-ahead or backtracking: there are no paragraph-wide considerations.

;; In ragged dispositions, the "First" variant stops as soon as a line fits,
;; that is, with the minimum number of characters and the maximum stretch,
;; while remaining below the paragraph width. The "Last" variant does the
;; opposite (maximum number of characters and maximum shrink). Finally, the
;; "Best" variant preserves the natural inter-word spacing.

;; In the Justified disposition, the First variant selects the first line that
;; fits the paragraph width exactly (hence, also with the minimum number of
;; characters and the maximum stretch). The Last variant does the opposite
;; (maximum number of characters and maximum shrink). The Best variant uses
;; TeX's "badness + hyphen penalty" to weight solutions.

;; The "Avoid Hyphens" option affects all Boolean decision versions, that is,
;; all but the Best/Justified one. When checked, line solutions without
;; hyphenation are always preferred when there is a choice.

;; The "Relax" option affects the First and Last variants in ragged
;; dispositions. When checked, lines are "de-stretched" or "de-shrunk" towards
;; the natural inter-word spacing (which is why it is meaningless for the Best
;; variant), after having been created, and without changing their potential
;; contents. More specifically:
;; - For the First Fit, lines are de-stretched as much as possible, but not to
;;   the point that another word would fit in. The effect is thus to make the
;;   paragraph more compact.
;; - For the Last Fit, lines are de-shrunk as much as possible towards the
;;   natural inter-word space, without producing overfull lines. The effect is
;;   thus to make the paragraph less compact.

;; The "[Explicit ]Hyphen Penalty" option affects the Best/Justified version's
;; weight function in the TeX way.

;; In the Best/Justified version, several line boundaries may turn out to have
;; the same optimum weight (badness + hyphen penalty). A weight of -∞ cannot
;; occur because that would be a mandatory discretionary break, so it would
;; have been already treated. Two possibilities may thus occur:
;;
;; 1. Numerical weight. Because of the badness definition, this can only mean
;;    that we have different boundaries with or without varying hyphen
;;    penalties.
;; 2. Infinite weight. Prohibited discretionary breaks have been skipped, so
;;    we have cases of infinite badness. This means unstretchable underfulls,
;;    with maybe one final overfull.
;;
;; In case n.1, we minimize the difference between the natural width of the
;; line and the desired one if the discriminating function is "Minimize
;; Distance", or the scaling ratio otherwise ("Minimize Scaling"). The
;; difference is that in the second case, we take the frequent difference
;; between maximum stretching and shrinking into account. If this leads to
;; equality again, then we necessarily have one short and one long line. We
;; normally choose the short line (in other words, we prefer stretching)
;; unless the "Prefer Shrink" option is checked.
;;
;; In case n.2, and if there isn't an overfull, we'll select the last boundary
;; because it is the one that fills the line to the maximum possible. If
;; there's an overfull, we need to choose between the last two boundaries,
;; that is, between the last unstretchable underfull, and the overfull. We
;; normally choose the underfull line, unless the "Prefer Overfull" option is
;; checked. Note that the underfull may or may not have some shrinkability
;; (remember that TeX's definition of badness for overfull is always
;; +∞), so maybe the overfull could be compressed, but not too much. We don't
;; want to handle that as a special case, because a better way to do it would
;; be to change the definition of badness to /not/ be infinite all of a sudden
;; for overfulls, but be more subtle.

;; Note that our notion of "fit" is different from that of Donald Knuth. In
;; the Knuth-Plass paper, what he calls "first fit" is probably the Duncan
;; algorithm, choosing the first solution that stays close to the natural
;; inter-word space.

;; Note that except for the Justified disposition, the Best-Fit algorithm is
;; equivalent to the Underfull-Fixed one (which is why the Relax option has no
;; effect on it).


;; #### TODO: maybe we could think of other potential weight functions for the
;; #### Best/Justified version, and provide a choice? Note however that the
;; #### way we collect possible line endings in the Best/Justified version is
;; #### tightly coupled to our knowledge of the besting function, so this may
;; #### need to evolve as well.


(in-package :etap)


(defparameter *fit-variants*
  '(:first :best :last))

(defparameter *fit-variants-help-keys*
  '(:fit-variant-first :fit-variant-best :fit-variant-last))

(defparameter *fit-discriminating-functions*
  '(:minimize-distance :minimize-scaling))

(defparameter *fit-options*
  '((:avoid-hyphens t) (:relax t) (:prefer-shrink t) (:prefer-overfulls t)))

(defparameter *fit-options-help-keys*
  '(:fit-option-avoid-hyphens :fit-option-relax
    :fit-option-prefer-shrink :fit-option-prefer-overfulls))


(defmacro define-fit-caliber (name min default max)
  "Define a NAMEd Fit caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber fit ,name ,min ,default ,max))

(define-fit-caliber hyphen-penalty -1000 50 1000)
(define-fit-caliber explicit-hyphen-penalty -1000 50 1000)


(defparameter *fit-tooltips*
  '(:fit-variant-first "Prefer lines with fewer words (more stretch)."
    :fit-variant-best "Minimize scaling."
    :fit-variant-last "Prefer lines with more words (more shrink)."
    :fit-option-avoid-hyphens "Except for the Best/Justified version,
avoid hyphenating words when possible."
    :fit-option-relax "For the First and Last variants in ragged dispositions,
de-stretch or de-shrink lines afterwards."
    :fit-option-prefer-shrink "In the Best/Justified version,
prefer shrinking over stretching
for equally good solutions."
    :fit-option-prefer-overfulls "In the Best/Justified version,
prefer overfull over underfull
for equally bad solutions."))


(defun fit-weight
    (lineup start width boundary hyphen-penalty explicit-hyphen-penalty
     &aux (badness (badness lineup start (stop-idx boundary) width)))
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
  (mapcar
      (lambda (boundary)
	(cons (fit-weight lineup start width boundary
			  hyphen-penalty explicit-hyphen-penalty)
	      boundary))
    boundaries))

(defun fit-deltas (lineup start width boundaries)
  (mapcar (lambda (boundary)
	    (cons (abs (- width
			  (lineup-width lineup start (stop-idx boundary))))
		  boundary))
    boundaries))

;; #### WARNING: this only works for elastic lines!
(defun fit-scales (lineup start width boundaries)
  (mapcar (lambda (boundary)
	    (cons (abs (lineup-scale lineup start (stop-idx boundary) width))
		  boundary))
    boundaries))

(defun word-boundaries (boundaries)
  (remove-if (lambda (boundary) (discretionaryp (stop-elt boundary)))
      boundaries))

(defun hyphen-boundaries (boundaries)
  (remove-if-not (lambda (boundary) (discretionaryp (stop-elt boundary)))
      boundaries))

(defgeneric fit-line-boundary
    (lineup start width disposition variant &key &allow-other-keys)
  (:documentation
   "Return the Fit algorithm's view of the end of line boundary.")
  (:method (lineup start width disposition variant &key avoid-hyphens)
    (let ((lineup-width-function (ecase variant
				   (:first #'lineup-max-width)
				   (:best #'lineup-width)
				   (:last #'lineup-min-width))))
      (loop :with previous :with word-previous
	    ;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH
	    ;; NIL) first, and then NIL.
	    :for boundary := (next-boundary lineup start)
	      :then (next-boundary lineup (stop-idx boundary))
	    :while (and boundary
			(<= (funcall lineup-width-function
			      lineup start (stop-idx boundary))
			    width))
	    :do (setq previous boundary)
	    :do (unless (discretionaryp (stop-elt boundary))
		  (setq word-previous boundary))
	    :finally (return (if previous
			       (if avoid-hyphens
				 (or word-previous previous)
				 previous)
			       boundary)))))
  (:method (lineup start width (disposition (eql :justified)) variant
	    &key avoid-hyphens)
    ;; #### NOTE: here, we collect all the possible fits, but only the last
    ;; underfull and the first overfull, regardless of whether they are
    ;; hyphenated or not. The rationale is that if we can't find a fit, we'd
    ;; better stick as close to the paragraph's border as possible anyway. In
    ;; other words, "Avoid Hyphens" really means avoid, not strictly prohibit
    ;; (strictly prohibit can be achieved by not hyphenating the lineup).
    (loop :with underfull
	  :with fits := (list)
	  :with overfull
	  ;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH
	  ;; NIL) first, and then NIL.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (stop-idx boundary))
	  :while (and boundary (not overfull))
	  :for span := (lineup-span lineup start (stop-idx boundary))
	  :if (< (max-width span) width)
	    :do (setq underfull boundary)
	  :else :if (and (<= (min-width span) width)
			 (>= (max-width span) width))
	    :do (push boundary fits)
	  :else
	    :do (setq overfull boundary)
	  :finally
	     (return
	       (if (= (length fits) 1)
		 (car fits)
		 (let ((boundaries
			 ;; #### NOTE: NIL if FITS is anyway.
			 (if avoid-hyphens
			   (or (word-boundaries fits)
			       (hyphen-boundaries fits))
			   fits)))
		   (ecase variant
		     (:first
		      (cond (boundaries (car (last boundaries)))
			    (underfull underfull)
			    (t overfull)))
		     (:last
		      (cond (boundaries (car boundaries))
			    (overfull overfull)
			    (t underfull)))))))))
  (:method (lineup start width
	    (disposition (eql :justified)) (variant (eql :best))
	    &key discriminating-function hyphen-penalty explicit-hyphen-penalty
		 prefer-shrink prefer-overfulls)
    ;; #### NOTE: in order to handle all possible hyphen penalty values
    ;; (negative or positive, as well as infinite), we collect all potential
    ;; breaks until the first overfull, prematurely ending on mandatory hyphen
    ;; breaks. That's because the badness of overfull is infinite anyway, so
    ;; there's no point in collecting more than one.
    ;; #### NOTE: below, we collect boundaries in reverse order because we
    ;; will often need to access the most recent ones, and we use STABLE-SORT
    ;; to preserve that order.
    (loop :with boundaries :with overfull
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
	    :if (> (min-width span) width)
	      :do (setq overfull t)
	      :and :do (push boundary boundaries)
	    :else :if (and (discretionaryp (stop-elt boundary))
			   (or (and (explicitp (stop-elt boundary))
				    (eq explicit-hyphen-penalty -∞))
			       (and (not (explicitp (stop-elt boundary)))
				    (eq hyphen-penalty -∞))))
	      :do (return boundary)
	    :else
	      :do (push boundary boundaries)
	  :finally
	     (return
	       (if (= (length boundaries) 1)
		 (car boundaries)
		 (let ((sorted-weights
			 (stable-sort
			  (fit-weights
			   lineup start width boundaries
			   hyphen-penalty explicit-hyphen-penalty)
			  #'<< :key #'car)))
		   (cond ((eql (caar sorted-weights) (caadr sorted-weights))
			  (setq sorted-weights
				(remove-if-not
				 (lambda (weight)
				   (eql weight (caar sorted-weights)))
				 sorted-weights
				 :key #'car))
			  (cond ((numberp (caar sorted-weights))
				 (let ((sorted-scores
					 (stable-sort
					  (funcall
					      (ecase discriminating-function
						(:minimize-distance
						 #'fit-deltas)
						(:minimize-scaling
						 #'fit-scales))
					    lineup start width
					    (mapcar #'cdr sorted-weights))
					  #'< :key #'car)))
				   (cond ((= (caar sorted-scores)
					     (caadr sorted-scores))
					  (setq sorted-scores
						(remove-if-not
						 (lambda (delta)
						   (= delta
						      (caar sorted-scores)))
						 sorted-scores
						 :key #'car))
					  (assert (= (length sorted-scores) 2))
					  (if prefer-shrink
					    (cdar sorted-scores)
					    (cdadr sorted-scores)))
					 (t
					  (cdar sorted-scores)))))
				(overfull
				 (if prefer-overfulls
				   (cdar sorted-weights)
				   (cdadr sorted-weights)))
				(t
				 (cdar sorted-weights))))
			 (t
			  (cdar sorted-weights)))))))))

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
		    ;; A negative scale means that the line would naturally be
		    ;; overfull, so we can only deshrink up to that scale.
		    ;; Otherwise, we can deshrink completely.
		    (if (and scale (< scale 0)) scale 0))))
    (make-line lineup start stop scale))
  (:method (lineup start stop (disposition (eql :justified)) variant
	    &key width sloppy last)
    "Make a justified Fit line from LINEUP chunk between START and STOP."
    (if last
      ;; The last line, which almost never fits exactly, needs a special
      ;; treatment. Without paragraph-wide considerations, we want its scaling
      ;; to be close to the general effect of the selected variant.
      (let ((scale (lineup-scale lineup start stop width)))
	(ecase variant
	  (:first
	   ;; Stretch as much as possible.
	   (make-line lineup start stop 1))
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
     &key variant discriminating-function
	  hyphen-penalty explicit-hyphen-penalty
	  relax avoid-hyphens prefer-shrink prefer-overfulls)
  "Typeset LINEUP with the Fit algorithm."
  (default-fit variant)
  (default-fit discriminating-function)
  (calibrate-fit hyphen-penalty t)
  (calibrate-fit explicit-hyphen-penalty t)
  (loop :for start := 0 :then (next-start boundary)
	:while start
	:for boundary
	  := (fit-line-boundary
		 lineup start width (disposition-type disposition) variant
	       :avoid-hyphens avoid-hyphens
	       :discriminating-function discriminating-function
	       :hyphen-penalty hyphen-penalty
	       :explicit-hyphen-penalty explicit-hyphen-penalty
	       :prefer-shrink prefer-shrink
	       :prefer-overfulls prefer-overfulls)
	:collect (apply #'fit-make-line lineup start (stop-idx boundary)
			(disposition-type disposition) variant
			:width width
			:relax relax
			:last (null (stop-elt boundary))
			(disposition-options disposition))))
