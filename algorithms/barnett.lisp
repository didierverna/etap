;; This is the Barnett algorithm from: Michael P. Barnett, Computer
;; Typesetting: Experiments and Prospects, M.I.T. Press, Cambridge,
;; Massachusetts, 1965.

;; I don't have the book, but Knuth describes it as follows in the Knuth-Plass
;; paper: Keep appending words to the current line, assuming the normal
;; spacing, until reaching a word that does not fit. Break after this word, if
;; it is possible to do so without compressing the spaces to less than the
;; given minimum; otherwise break before this word, if it is possible to do so
;; without expanding the spaces to more than the given maximum. Otherwise
;; hyphenate the offending word, putting as much of it on the current line as
;; will fit; if no suitable hyphenation points can be found, this may result
;; in a line whose spaces exceed the given maximum.

;; From what I gather, this algorithm looks like a mixture of different *-Fit
;; policies, but not quite, for the following reasons.
;; 1. It is similar to the Best Fit, in that it tries the normal spacing
;;    first, but if the line with the additional word can be shrunk, it will
;;    do so even if stretching without that word would have been closer to the
;;    normal spacing.
;; 2. When hyphenation is necessary, it appears to put in "as much as will
;;    fit", which is a form of Last Fit (maybe putting a bit less in would
;;    have given a result closer to the normal spacing).
;; 3. Finally, it also seems to always prefer overfull lines over underfull
;;    ones when no perfect match is found.

;; #### FIXME: I don't know if Barnett is restricted to the Justified
;; #### disposition, or if it does something for the ragged ones.


(in-package :etap)

(defun barnett-line-boundary (lineup start width)
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
	       (let ((word-scales
		       ;; #### NOTE: NIL if FIT-BOUNDARIES is anyway.
		       (boundary-scales
			lineup start width
			(word-boundaries lineup fit-boundaries)))
		     (hyphen-boundaries
		       ;; #### NOTE: NIL if FIT-BOUNDARIES is anyway.
		       (hyphen-boundaries lineup fit-boundaries)))
		 (cond (word-scales
			;; #### NOTE: this test again because we may have
			;; filtered FIT-BOUNDARIES.
			(if (or (= (length word-scales) 1)
				(>= (cdar word-scales) 0))
			  (caar word-scales)
			  (loop :for scales :on word-scales
				:until (or (null (cdr scales))
					   (> (cdadr scales) 0))
				:finally (return (caar scales)))))
		       (hyphen-boundaries
			(car hyphen-boundaries))
		       (t
			(or overfull-boundary underfull-boundary))))))))

(defun barnett-create-line (lineup start end width sloppy)
  (let ((scale (lineup-scale lineup start end width)))
    (if scale
      (create-line lineup start end
		   (cond (sloppy scale)
			 ((zerop scale) 0)
			 ((< scale 0) (max scale -1))
			 ((> scale 0) (min scale 1))))
      (create-line lineup start end))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :barnett)) &key sloppy)
  (declare (ignore disposition))
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start next-search)
	  := (barnett-line-boundary lineup start width)
	:collect (barnett-create-line lineup start end width sloppy)))
