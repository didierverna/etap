;; This is the classical *-fit algorithms family, making full use of
;; inter-word (elastic) glue. As their name suggest (and as in the case of the
;; Fixed algorithm), there is no paragraph-wide optimization. Lines are
;; constructed sequentially, with no backtracking.

;; The "First" variant stops as soon as a line fits, that is, with the minimum
;; number of characters, and the maximum stretch. The "Last" one does the
;; opposite. The "Best" one tries to makes lines as close as possible to the
;; natural inter-word space.

;; The "Relax" option only affects the First and Last variants, in the ragged
;; dispositions. It essentially decreases the raggedness. When checked, lines
;; are "de-stretched" or "de-shrunk", after having been created, but without
;; changing their potential contents. More specifically:
;; - for the First Fit, lines are de-stretched as much as possible towards the
;;   natural inter-word space, but not to the point that another word would
;;   fit in. The effect is thus to make the paragraph more compact.
;; - for the Last Fit, lines are de-shrunk as much as possible towards the
;;   natural inter-word space, without producing overfull lines. The effect is
;;   thus to make the paragraph less compact.

;; The "Sloppy" option only affects the Justified disposition. When checked,
;; lines are always stretched or shrunk to the paragraph width (hence
;; eliminating all overfull or underfull lines), after they are created. In
;; other words, the maximum inter-word stretch and shrink values are ignored,
;; and the resulting lines may be over-stretched or over-shrunk.
;; #### TODO: the sloppy option is Boolean. For a finer grain of sloppiness,
;; #### we could instead modify the inter-word glue.

;; The "Avoid Hyphens" option only affects the Justified disposition. When
;; checked, line solutions without hyphenation are always preferred when there
;; is a choice. Note that in ragged dispositions, this option would
;; essentially be equivalent to turning hyphenation off.

;; The "Prefer Shrink" option only affects the Best Fit variant in Justified
;; disposition. When two lines (one stretched and one shrunk) fit the
;; paragraph width with the same amount of scaling, the stretched one is
;; preferred unless this option is checked.
;; #### TODO: scaling equality in absolute value may not be the most pertinent
;; #### criterion. Indeed, the aesthetic cost of shrinking and stretching are
;; #### not the same, as in most fonts, the maximum stretch and shrink ratios
;; #### are not equal. Maybe a more pertinent measure would be the amount of
;; #### stretch / shrink relative to the maximum value.

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
  (:method (lineup start width disposition variant &key)
    (let ((lineup-width-function (case variant
				   (:first #'lineup-max-width)
				   (:best #'lineup-width)
				   (:last #'lineup-min-width))))
      ;; #### NOTE: this works even the first time because at worst, BOUNDARY
      ;; is gonna be #S(LENGTH LENGTH :ENGTH) first, and NIL only afterwards.
      (loop :with previous-boundary
	    :for boundary := (next-boundary lineup start)
	      :then (next-boundary lineup (next-search boundary))
	    :while (and boundary
			(<= (funcall lineup-width-function
			      lineup start (stop boundary))
			    width))
	    :do (setq previous-boundary boundary)
	    :finally (return previous-boundary))))
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
    (let ((scale (lineup-scale lineup start stop width)))
      (if scale
	(create-line lineup start stop
		     (cond (sloppy scale)
			   ((zerop scale) 0)
			   ((< scale 0) (max scale -1))
			   ((> scale 0) (min scale 1))))
	(create-line lineup start stop)))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :fit))
     &key (variant :first) relax sloppy avoid-hyphens
	  prefer-shrink prefer-overfull-lines)
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary
	  := (fit-line-boundary lineup start width disposition variant
	       :avoid-hyphens avoid-hyphens
	       :prefer-shrink prefer-shrink
	       :prefer-overfull-lines prefer-overfull-lines)
	:collect (fit-create-line lineup start (stop boundary)
		     disposition variant
		   :width width :search (next-search boundary)
		   :relax relax :sloppy sloppy)))
