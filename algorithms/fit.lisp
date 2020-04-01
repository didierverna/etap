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

;; The "Prefer Overfull Lines" option only affects the Best/Justified version.
;; When two line solutions are equally bad (that is, one underfull and one
;; overfull, with infinite weight), the underfull one is preferred, unless
;; this option is checked.

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
;; #### Best/Justified version, and provide a choice?

(in-package :etap)


(defun fit-weight (lineup start width boundary hyphen-penalty
		   &aux (weight (badness lineup start (stop boundary) width)))
  (unless (word-boundary-p lineup boundary)
    (setq weight
	  (!+ weight (unless (= hyphen-penalty 10000) hyphen-penalty))))
  weight)

(defun fit-weights (lineup start width boundaries hyphen-penalty)
  (mapcar (lambda (boundary)
	    (cons (fit-weight lineup start width boundary hyphen-penalty)
		  boundary))
    boundaries))


;; #### NOTE: this function returns all the possible fits, but only the last
;; underfull and the first overfull, regardless of whether they are hyphenated
;; or not. I think this makes sense for algorithms that don't do
;; paragraph-wide optimization. The rationale is that if we can't find a fit,
;; we'd better stick as close to the paragraph's border as possible anyway,
;; regardless of the Avoid Hyphens option, or the weight of hyphens in the
;; besting function.
(defun fit-next-boundaries (lineup start width)
  (loop :with underfull
	:with fits := (list)
	:with overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-search boundary))
	:while (and boundary (not overfull))
	:for span := (lineup-span lineup start (stop boundary))
	:if (< (max-width span) width)
	  :do (setq underfull boundary)
	:else :if (and (<= (min-width span) width) (>= (max-width span) width))
	  :do (push boundary fits)
	:else
	  :do (setq overfull boundary)
	:finally
	   (return (values underfull fits overfull))))

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
	      :then (next-boundary lineup (next-search boundary))
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
    (multiple-value-bind (underfull fits overfull)
	(fit-next-boundaries lineup start width)
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
		   (t underfull))))))))
  (:method (lineup start width
	    (disposition (eql :justified)) (variant (eql :best))
	    &key hyphen-penalty prefer-shrink prefer-overfull-lines)
    (multiple-value-bind (underfull fits overfull)
	(fit-next-boundaries lineup start width)
      ;; #### NOTE: fits and *fulls get merged here, because the besting
      ;; function will choose. For example, with a very high hyphen penalty,
      ;; we may end up preferring underfull lines. If one day we generalize
      ;; the besting function, we can expect any other decision selecting
      ;; *fulls over fits.
      (let ((boundaries (when underfull (list underfull))))
	(when fits (setq boundaries (append fits boundaries)))
	(when overfull (push overfull boundaries))
	(if (= (length boundaries) 1)
	  (car boundaries)
	  (let ((weights
		  (stable-sort
		   (fit-weights lineup start width boundaries hyphen-penalty)
		   #'!<
		   :key #'car)))
	    (cond ((and (numberp (caar weights)) (numberp (caadr weights))
			(= (caar weights) (caadr weights)))
		   (if prefer-shrink (cdar weights) (cdadr weights)))
		  ((and (null (caar weights)) (null (caadr weights)))
		   (if prefer-overfull-lines (cdar weights) (cdadr weights)))
		  (t
		   (cdar weights)))))))))

(defgeneric fit-create-line
    (lineup start stop disposition variant &key &allow-other-keys)
  (:method (lineup start stop disposition (variant (eql :first))
	    &key width search relax
	    &aux (scale 1))
    (when relax
      (setq scale
	    (if (< stop (length lineup))
	      (let ((scale
		      (lineup-scale
		       lineup start (stop (next-boundary lineup search))
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
     &key (variant :first) (hyphen-penalty 50)
	  relax avoid-hyphens prefer-shrink prefer-overfull-lines)
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary
	  := (fit-line-boundary lineup start width (car disposition) variant
	       :hyphen-penalty hyphen-penalty
	       :avoid-hyphens avoid-hyphens
	       :prefer-shrink prefer-shrink
	       :prefer-overfull-lines prefer-overfull-lines)
	:collect (fit-create-line lineup start (stop boundary)
		     (car disposition) variant
		   :width width :search (next-search boundary)
		   :relax relax :sloppy (cadr (member :sloppy disposition)))))
