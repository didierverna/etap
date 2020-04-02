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
  (loop :with word-underfull :with hyphens :with word-overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-search boundary))
	:while (and boundary (not word-overfull))
	:for w := (lineup-width lineup start (stop boundary))
	:if (and (word-boundary-p lineup boundary) (< w width))
	  :do (setq word-underfull boundary hyphens nil)
	:else :if (and (word-boundary-p lineup boundary) (>= w width))
	  :do (setq word-overfull boundary)
	:else
	  :do (push boundary hyphens)
	:finally
	   (return
	     (cond ((and word-overfull
			 (let ((scale (lineup-scale lineup start
						    (stop word-overfull)
						    width)))
			   (and scale (>= scale -1))))
		    word-overfull)
		   ((and word-underfull
			 (let ((scale (lineup-scale lineup start
						    (stop word-underfull)
						    width)))
			   (and scale (<= scale 1))))
		    word-underfull)
		   (hyphens
		    (loop :with last-overfull
			  :for hyphen :in hyphens
			  :for scale
			    := (lineup-scale lineup start (stop hyphen) width)
			  :while (if scale
				   (<= scale 0)
				   (>= (lineup-width lineup
						     start (stop hyphen))
				       width))
			  :if (or (null scale) (< scale -1))
			    :do (setq last-overfull hyphen)
			  :else
			    :do (return hyphen)
			  :finally
			     (return
			       (or last-overfull word-overfull))))
		   (t
		    (or word-overfull word-underfull))))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :barnett)) &key)
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary := (barnett-line-boundary lineup start width)
	:collect (create-justified-line
		  lineup start (stop boundary) width
		  (cadr (member :sloppy (disposition-options disposition))))))
