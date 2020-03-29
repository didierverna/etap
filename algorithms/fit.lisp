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
;; (maximum number of characters and maximum shrink).

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are always preferred when there is a choice.

;; The "Relax" option only affects the First and Last variants, in ragged
;; dispositions. It essentially decreases the raggedness. When checked, lines
;; are "de-stretched" or "de-shrunk" towards the natural inter-word spacing
;; (which is why it is meaningless for the Best variant), after having been
;; created, but without changing their potential contents.
;; More specifically:
;; - for the First Fit, lines are de-stretched as much as possible, but not to
;;   the point that another word would fit in. The effect is thus to make the
;;   paragraph more compact.
;; - for the Last Fit, lines are de-shrunk as much as possible towards the
;;   natural inter-word space, without producing overfull lines. The effect is
;;   thus to make the paragraph less compact.

;; The "Prefer Shrink" option only affects the Best Fit variant in Justified
;; disposition. When two lines (one stretched and one shrunk) fit the
;; paragraph width with the same amount of scaling, the stretched one is
;; preferred unless this option is checked.

;; #### TODO: scaling equality in absolute value may not be the most pertinent
;; #### criterion. Indeed, the aesthetic cost of shrinking and stretching are
;; #### not the same, as in most fonts, the maximum stretch and shrink ratios
;; #### are not equal. Maybe a more pertinent measure would be the amount of
;; #### stretch / shrink relative to the maximum value.

;; Note that our notion of "fit" is different from that of Donald Knuth. In
;; the Knuth-Plass paper, what he calls "first fit" is probably the Duncan
;; algorithm, choosing the first solution that stays close to the natural
;; inter-word space.

;; Note that except for the Justified disposition, the Best-Fit algorithm is
;; equivalent to the Underfull-Fixed one (which is why the Relax option has no
;; effect on it).


(in-package :etap)

(defun sorted-scales (lineup start width boundaries)
  (sort (boundary-scales lineup start width boundaries) #'<
    :key (lambda (scale) (abs (cdr scale)))))

(defun width-delta (lineup start width boundary)
  (when boundary (abs (- width (lineup-width lineup start (stop boundary))))))

(defgeneric fit-line-boundary
    (lineup start width disposition variant &key &allow-other-keys)
  (:method (lineup start width disposition variant &key avoid-hyphens)
    (let ((lineup-width-function (case variant
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
	    &key avoid-hyphens prefer-shrink prefer-overfull-lines)
    (multiple-value-bind (underfull-boundary fit-boundaries overfull-boundary)
	(next-boundaries lineup start width)
      (if (= (length fit-boundaries) 1)
	(car fit-boundaries)
	(let ((boundaries
		;; #### NOTE: NIL if FIT-BOUNDARIES is anyway.
		(if avoid-hyphens
		  (or (word-boundaries lineup fit-boundaries)
		      (hyphen-boundaries lineup fit-boundaries))
		  fit-boundaries)))
	  (case variant
	    (:first
	     (cond (boundaries (car (last boundaries)))
		   (underfull-boundary underfull-boundary)
		   (t overfull-boundary)))
	    (:last
	     (cond (boundaries (car boundaries))
		   (overfull-boundary overfull-boundary)
		   (t underfull-boundary)))
	    (:best
	     (cond ((= (length boundaries) 1)
		    ;; #### NOTE: this test again because we may have filtered
		    ;; FIT-BOUNDARIES.
		    (car boundaries))
		   (boundaries
		    (let ((sorted-scales
			    (sorted-scales lineup start width boundaries)))
		      (if (= (abs (cdr (first sorted-scales)))
			     (abs (cdr (second sorted-scales))))
			(if prefer-shrink
			  (car (first sorted-scales))
			  (car (second sorted-scales)))
			(car (first sorted-scales)))))
		   (t
		    (let ((underfull-delta
			    (width-delta lineup start width underfull-boundary))
			  (overfull-delta
			    (width-delta lineup start width overfull-boundary)))
		      (cond ((and underfull-delta overfull-delta)
			     (cond ((= underfull-delta overfull-delta)
				    (if prefer-overfull-lines
				      overfull-boundary
				      underfull-boundary))
				   ((< underfull-delta overfull-delta)
				    underfull-boundary)
				   (t overfull-boundary)))
			    (underfull-delta underfull-boundary)
			    (t overfull-boundary))))))))))))

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
     &key (variant :first)
	  relax avoid-hyphens prefer-shrink prefer-overfull-lines)
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary
	  := (fit-line-boundary lineup start width (car disposition) variant
	       :avoid-hyphens avoid-hyphens
	       :prefer-shrink prefer-shrink
	       :prefer-overfull-lines prefer-overfull-lines)
	:collect (fit-create-line lineup start (stop boundary)
		     (car disposition) variant
		   :width width :search (next-search boundary)
		   :relax relax :sloppy (cadr (member :sloppy disposition)))))
