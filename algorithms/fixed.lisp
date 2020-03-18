;; This algorithm uses only the normal, fixed, inter-word spacing (hence, it
;; can practically never justify properly). The "Underfull" variant only
;; allows underfull lines (unless there is no choice). The "Overfull" one does
;; the opposite. The "Best" one chooses which is closest to the paragraph
;; width.

;; In the Best variant, when the underfull and overfull lines are equally
;; distant from the paragraph width, the underfull one is chosen, unless the
;; "Prefer Overfull Lines" option is checked.

;; This algorithm doesn't have a notion of break-point cost. In particular,
;; hyphens are seen as just additional break opportunities.

;; Note that because the inter-word spacing is fixed, there is no difference
;; between the Flush Left and Justified dispositions.

(in-package :etap)

(defun fixed-line-boundary (lineup start width variant prefer-overfull-lines)
  (loop :with underfull-boundary
	:with underfull-w
	:with fit-boundary
	:with overfull-boundary
	:with overfull-w
	;; #### NOTE: this works even the first time because at worst,
	;; NEXT-SEARCH is gonna be (length lineup) first, and NIL only
	;; afterwards.
	:for (end next-start next-search) := (next-boundary lineup start)
	  :then (next-boundary lineup next-search)
	:for w := (lineup-width lineup start end)
	:while (and next-search (not overfull-boundary))
	:if (< w width)
	  :do (setq underfull-boundary (list end next-start) underfull-w w)
	:else :if (= w width)
	  :do (setq fit-boundary (list end next-start))
	:else
	  :do (setq overfull-boundary (list end next-start) overfull-w w)
	:finally
	   (return
	     (cond (fit-boundary fit-boundary)
		   ((and underfull-w (not overfull-w)) underfull-boundary)
		   ((and overfull-w (not underfull-w)) overfull-boundary)
		   (t
		    (case variant
		      (:underfull
		       underfull-boundary)
		      (:overfull
		       overfull-boundary)
		      (:best
		       (cond ((< (- width underfull-w) (- overfull-w width))
			      underfull-boundary)
			     ((< (- overfull-w width) (- width underfull-w))
			      overfull-boundary)
			     (prefer-overfull-lines
			      overfull-boundary)
			     (t underfull-boundary)))))))))

(defmethod create-lines
    (lineup disposition width (algorithm (eql :fixed))
     &key (variant :underfull) prefer-overfull-lines)
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start)
	  := (fixed-line-boundary
	      lineup start width variant prefer-overfull-lines)
	:collect (create-line lineup start end)))
