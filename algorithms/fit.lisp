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

(defun word-boundaries (lineup boundaries)
  (remove-if-not (lambda (boundary) (word-boundary-p lineup boundary))
		 boundaries))

(defun hyphen-boundaries (lineup boundaries)
  (remove-if (lambda (boundary) (word-boundary-p lineup boundary))
	     boundaries))

(defgeneric fit-line-boundary
    (start lineup width disposition variant &key &allow-other-keys)
  (:method (start lineup width disposition variant &key)
    (let ((lineup-width-function (case variant
				   (:first #'lineup-max-width)
				   (:best #'lineup-width)
				   (:last #'lineup-min-width))))
      ;; #### NOTE: this works even the first time because at worst,
      ;; NEXT-SEARCH is gonna be (length lineup) first, and NIL only
      ;; afterwards.
      (loop :with previous-boundary
	    :for (end next-start next-search)
	      := (next-boundary lineup start)
		:then (next-boundary lineup next-search)
	    :for w := (funcall lineup-width-function lineup start end)
	    :while (and next-search (<= w width))
	    :do (setq previous-boundary (list end next-start next-search))
	    :finally (return previous-boundary))))
  (:method (start lineup width (disposition (eql :justified)) variant
	    &key avoid-hyphens prefer-shrink)
    (loop :with underfull-boundary
	  :with fit-boundaries := (list)
	  :with overfull-boundary
	  ;; #### NOTE: this works even the first time because at worst,
	  ;; NEXT-SEARCH is gonna be (length lineup) first, and NIL only
	  ;; afterwards.
	  :for boundary := (next-boundary lineup start)
	    :then (next-boundary lineup (caddr boundary))
	  :for span := (multiple-value-bind (width stretch shrink)
			   (lineup-width lineup start (car boundary))
			 (list width (+ width stretch) (- width shrink)))
	  :while (and (caddr boundary) (not overfull-boundary))
	  :if (< (cadr span) width)
	    :do (setq underfull-boundary boundary)
	  :else :if (and (<= (caddr span) width) (>= (cadr span) width))
	    :do (push boundary fit-boundaries)
	  :else
	    :do (setq overfull-boundary boundary)
	  :finally
	     (return
	       (if (= (length fit-boundaries) 1)
		 (car fit-boundaries)
		 (case variant
		   (:first
		    (cond (fit-boundaries
			   (if avoid-hyphens
			     (let ((word-boundaries
				     (word-boundaries lineup fit-boundaries))
				   (hyphen-boundaries
				     (hyphen-boundaries lineup fit-boundaries)))
			       (if word-boundaries
				 (car (last word-boundaries))
				 (car (last hyphen-boundaries))))
			     (car (last fit-boundaries))))
			  (underfull-boundary underfull-boundary)
			  (t overfull-boundary)))
		   (:last
		    (cond (fit-boundaries
			   (if avoid-hyphens
			     (let ((word-boundaries
				     (word-boundaries lineup fit-boundaries))
				   (hyphen-boundaries
				     (hyphen-boundaries lineup fit-boundaries)))
			       (if word-boundaries
				 (car word-boundaries)
				 (car hyphen-boundaries)))
			     (car fit-boundaries)))
			  (overfull-boundary overfull-boundary)
			  (t underfull-boundary)))
		   (:best
		    (if fit-boundaries
		      (if avoid-hyphens
			(let ((word-boundaries
				(word-boundaries lineup fit-boundaries))
			      (hyphen-boundaries
				(hyphen-boundaries lineup fit-boundaries)))
			  (if word-boundaries
			    (let ((sorted-scales
				    (sort
				     (mapcar
					 (lambda (boundary)
					   (cons boundary
						 (lineup-scale lineup
							       start
							       (car boundary)
							       width)))
				       word-boundaries)
				     #'<
				     :key (lambda (elt) (abs (cdr elt))))))
			      (if (and (> (length sorted-scales) 1)
				       (= (abs (cdr (first sorted-scales)))
					  (abs (cdr (second sorted-scales)))))
				(if prefer-shrink
				  (car (first sorted-scales))
				  (car (second sorted-scales)))
				(car (first sorted-scales))))
			    (let ((sorted-scales
				    (sort
				     (mapcar
					 (lambda (boundary)
					   (cons boundary
						 (lineup-scale lineup
							       start
							       (car boundary)
							       width)))
				       hyphen-boundaries)
				     #'<
				     :key (lambda (elt) (abs (cdr elt))))))
			      (if (and (> (length sorted-scales) 1)
				       (= (abs (cdr (first sorted-scales)))
					  (abs (cdr (second sorted-scales)))))
				(if prefer-shrink
				  (car (first sorted-scales))
				  (car (second sorted-scales)))
				(car (first sorted-scales))))))
			(let ((sorted-scales
				(sort
				 (mapcar
				     (lambda (boundary)
				       (cons boundary
					     (lineup-scale lineup
							   start
							   (car boundary)
							   width)))
				   fit-boundaries)
				 #'<
				 :key (lambda (elt) (abs (cdr elt))))))
			  (if (= (abs (cdr (first sorted-scales)))
				 (abs (cdr (second sorted-scales))))
			    (if prefer-shrink
			      (car (first sorted-scales))
			      (car (second sorted-scales)))
			    (car (first sorted-scales)))))
		      (let ((underfull-delta
			      (when underfull-boundary
				(- width
				   (lineup-width
				    lineup start (car underfull-boundary)))))
			    (overfull-delta
			      (when overfull-boundary
				(- (lineup-width
				    lineup start (car overfull-boundary))
				   width))))
			(cond ((and underfull-delta overfull-delta)
			       (if (< underfull-delta overfull-delta)
				 underfull-boundary
				 overfull-boundary))
			      (underfull-delta underfull-boundary)
			      (t overfull-boundary)))))))))))

(defgeneric fit-create-line
    (lineup start end search width disposition variant &key &allow-other-keys)
  (:method (lineup start end search width disposition (variant (eql :first))
	    &key relax &aux (scale 1))
    (when relax
      (setq scale
	    (if (< end (length lineup))
	      (let ((scale
		      (lineup-scale
		       lineup start (car (next-boundary lineup search))
		       width)))
		(if (and scale (> scale 0)) scale 0))
	      0)))
    (create-line lineup start end scale))
  (:method
      (lineup start end search width disposition (variant (eql :best)) &key)
    (create-line lineup start end))
  (:method (lineup start end search width disposition (variant (eql :last))
	    &key relax &aux (scale -1))
    (when relax
      (setq scale (let ((scale (lineup-scale lineup start end width)))
		    (if (and scale (< scale 0)) scale 0))))
    (create-line lineup start end scale))
  (:method
      (lineup start end search width (disposition (eql :justified)) variant
       &key sloppy)
    (let ((scale (lineup-scale lineup start end width)))
      (if scale
	(create-line lineup start end
		     (cond (sloppy scale)
			   ((zerop scale) 0)
			   ((< scale 0) (max scale -1))
			   ((> scale 0) (min scale 1))))
	(create-line lineup start end)))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :fit))
     &key (variant :first) relax sloppy avoid-hyphens prefer-shrink)
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start next-search)
	  := (fit-line-boundary start lineup width disposition variant
	       :avoid-hyphens avoid-hyphens :prefer-shrink prefer-shrink)
	:collect (fit-create-line lineup start end next-search width
		     disposition variant
		   :relax relax :sloppy sloppy)))
