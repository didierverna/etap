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

;; The "Prefer Shrink" option only affects the Best/Justified version. When
;; two line solutions (one stretched and one shrunk) get the same weight, the
;; stretched one is preferred unless this option is checked.

;; The "Prefer Overfulls" option only affects the Best/Justified version. When
;; two line solutions are equally bad (that is, one underfull and one
;; overfull, with infinite weight and same absolute distance to the desired
;; width), the underfull one is preferred, unless this option is checked.

;; The "Hyphen Penalty" slider affects the Best/Justified version's weight
;; function in the obvious TeX way.

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


(define-constant +fit-variants+
    '(:first :best :last))

(define-constant +fit-variants-help-keys+
    '(:fit-variant-first :fit-variant-best :fit-variant-last))

(define-constant +fit-options+
    '((:avoid-hyphens t) (:relax t) (:prefer-shrink t) (:prefer-overfulls t)))

(define-constant +fit-options-help-keys+
    '(:fit-option-avoid-hyphens :fit-option-relax
      :fit-option-prefer-shrink :fit-option-prefer-overfulls))

(define-constant +fit-default-hyphen-penalty+ 50)
(define-constant +fit-min-hyphen-penalty+ -1000)
(define-constant +fit-max-hyphen-penalty+ 1000)

(define-constant +fit-tooltips+
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


(defun fit-weight (lineup start width boundary hyphen-penalty
		   &aux (badness (badness lineup start (stop boundary) width)))
  (if (word-boundary-p lineup boundary)
    badness
    ;; #### NOTE: infinitely negative hyphen-penalties have already been
    ;; handled by an immediate RETURN from FIT-LINE-BOUNDARY, so there's no
    ;; risk of doing -infinity + +infinity here.
    (!+ badness hyphen-penalty)))

(defun fit-weights (lineup start width boundaries hyphen-penalty)
  (mapcar (lambda (boundary)
	    (cons (fit-weight lineup start width boundary hyphen-penalty)
		  boundary))
    boundaries))

(defgeneric fit-line-boundary
    (lineup start width disposition variant &key &allow-other-keys)
  (:method (lineup start width disposition variant &key avoid-hyphens)
    (let ((lineup-width-function (ecase variant
				   (:first #'lineup-max-width)
				   (:best #'lineup-width)
				   (:last #'lineup-min-width))))
      ;; #### NOTE: this works even the first time because at worst, BOUNDARY
      ;; is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only afterwards.
      (loop :with previous :with word-previous
	    :for boundary := (next-boundary lineup start)
	      :then (next-boundary lineup (next-start boundary))
	    :while (and boundary
			(<= (funcall lineup-width-function
			      lineup start (stop boundary))
			    width))
	    :do (setq previous boundary)
	    :do (when (word-boundary-p lineup boundary)
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
	  ;; #### NOTE: this works even the first time because at worst,
	  ;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	  ;; afterwards.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (next-start boundary))
	  :while (and boundary (not overfull))
	  :for span := (lineup-span lineup start (stop boundary))
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
			   (or (word-boundaries lineup fits)
			       (hyphen-boundaries lineup fits))
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
	    &key hyphen-penalty prefer-shrink prefer-overfulls)
    (cond ((= hyphen-penalty +fit-min-hyphen-penalty+)
	   (setq hyphen-penalty :-infinity))
	  ((= hyphen-penalty +fit-max-hyphen-penalty+)
	   (setq hyphen-penalty :+infinity)))
    ;; #### NOTE: in order to handle all possible hyphen penalty values
    ;; (negative or positive, as well as infinite), we collect all potential
    ;; breaks until the first overfull, prematurely ending on mandatory hyphen
    ;; breaks. That's because the badness of overfull is infinite anyway, so
    ;; there's no point in collecting more than one.
    (loop :with overfull
	  ;; #### NOTE: this works even the first time because at worst,
	  ;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	  ;; afterwards.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (next-start boundary))
	  :while (and boundary (not overfull))
	  :for span := (lineup-span lineup start (stop boundary))
	  :unless (and (not (word-boundary-p lineup boundary))
		       (eq hyphen-penalty :+infinity))
	    :if (> (min-width span) width)
	      :do (setq overfull t)
	      :and :collect boundary :into boundaries
	    :else :if (and (not (word-boundary-p lineup boundary))
			   (eq hyphen-penalty :-infinity))
	      :do (return boundary)
	    :else
	      :collect boundary :into boundaries
	  :finally
	     (return
	       (if (= (length boundaries) 1)
		 (car boundaries)
		 (let ((sorted-weights
			 (stable-sort
			  (fit-weights
			   lineup start width boundaries hyphen-penalty)
			  #'!< :key #'car)))
		   (cond ((eql (caar sorted-weights) (caadr sorted-weights))
			  (setq sorted-weights
				(remove-if-not
				 (lambda (weight)
				   (eql weight (caar sorted-weights)))
				 sorted-weights
				 :key #'car))
			  (if overfull
			    (cdar (last sorted-weights))
			    (cdar (last sorted-weights))))
			 (t
			  ;; Here, we simply take the (only) best one.
			  (cdar sorted-weights)))))))))

(defgeneric fit-create-line
    (lineup start stop disposition variant &key &allow-other-keys)
  (:method (lineup start stop disposition (variant (eql :first))
	    &key width next-start relax
	    &aux (scale 1))
    (when relax
      (setq scale
	    (if (< stop (length lineup))
	      (let ((scale
		      (lineup-scale
		       lineup start (stop (next-boundary lineup next-start))
		       width)))
		(if (and scale (> scale 0)) scale 0))
	      0)))
    (create-line lineup start stop scale))
  (:method (lineup start stop disposition (variant (eql :best)) &key)
    (create-line lineup start stop))
  (:method (lineup start stop disposition (variant (eql :last))
	    &key width relax
	    &aux (scale -1))
    (when relax
      (setq scale (let ((scale (lineup-scale lineup start stop width)))
		    (if (and scale (< scale 0)) scale 0))))
    (create-line lineup start stop scale))
  (:method (lineup start stop (disposition (eql :justified)) variant
	    &key width sloppy)
    (create-justified-line lineup start stop width sloppy)))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :fit))
     &key (variant (car +fit-variants+))
	  (hyphen-penalty +fit-default-hyphen-penalty+)
	  relax avoid-hyphens prefer-shrink prefer-overfulls)
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary
	  := (fit-line-boundary
		 lineup start width (disposition-type disposition) variant
	       :avoid-hyphens avoid-hyphens
	       :hyphen-penalty hyphen-penalty
	       :prefer-shrink prefer-shrink
	       :prefer-overfulls prefer-overfulls)
	:collect (apply #'fit-create-line lineup start (stop boundary)
			(disposition-type disposition) variant
			:width width :next-start (next-start boundary)
			:relax relax
			(disposition-options disposition))))
