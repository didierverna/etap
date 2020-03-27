;; This algorithm uses only the normal, fixed, inter-word spacing (hence, it
;; can practically never justify properly). The "Underfull" variant only
;; allows underfull lines (unless there is no choice). The "Overfull" one does
;; the opposite. The "Best" one chooses which is closest to the paragraph
;; width.

;; When the "Avoid Hyphens" option is checked, line solutions without
;; hyphenation are always preferred when there is a choice.

;; In the Best variant, when the underfull and overfull line solutions are
;; equally distant from the paragraph width, the underfull one is chosen,
;; unless the "Prefer Overfull Lines" option is checked.

;; Note that because the inter-word spacing is fixed, there is no difference
;; between the Flush Left and Justified dispositions.


(in-package :etap)

(defun first-overfull (overfull hyphen-overfull word-overfull)
  (if (and hyphen-overfull word-overfull)
    (if (eq overfull hyphen-overfull)
      word-overfull
      hyphen-overfull)
    overfull))

(defun best-*full
    (underfull underfull-w overfull overfull-w width prefer-overfull-lines)
  (cond ((< (- width underfull-w) (- overfull-w width))
	 underfull)
	((< (- overfull-w width) (- width underfull-w))
	 overfull)
	(prefer-overfull-lines
	 overfull)
	(t underfull)))

(defun fixed-line-boundary
    (lineup start width
     &key (variant :underfull) avoid-hyphens prefer-overfull-lines)
  (loop :with underfull :with hyphen-underfull :with word-underfull
	:with underfull-w :with hyphen-underfull-w :with word-underfull-w
	:with fit
	:with overfull :with hyphen-overfull :with word-overfull
	:with overfull-w :with hyphen-overfull-w :with word-overfull-w
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-search boundary))
	:while (and boundary (not word-overfull))
	:for w := (lineup-width lineup start (stop boundary))
	:if (< w width)
	  :do (setq underfull boundary underfull-w w)
	  :and :do (if (word-boundary-p lineup boundary)
		     (setq word-underfull boundary word-underfull-w w)
		     (setq hyphen-underfull boundary hyphen-underfull-w w))
	:else :if (= w width)
	  :do (setq fit boundary)
	:else
	  :do (setq overfull boundary overfull-w w)
	  :and :do (if (word-boundary-p lineup boundary)
		     (setq word-overfull boundary word-overfull-w w)
		     (setq hyphen-overfull boundary hyphen-overfull-w w))
	:finally
	   (return
	     (ecase variant
	       (:underfull
		(cond ((and fit
			    (not (word-boundary-p lineup fit))
			    avoid-hyphens)
		       (or word-underfull fit))
		      (fit fit)
		      (underfull
		       (if avoid-hyphens
			 (or word-underfull underfull)
			 ;; #### NOTE: this underfull the last one.
			 underfull))
		      (t
		       (first-overfull
			overfull hyphen-overfull word-overfull))))
	       (:overfull
		(cond ((and fit
			    (not (word-boundary-p lineup fit))
			    avoid-hyphens)
		       (or word-overfull fit))
		      (fit fit)
		      (overfull
		       (if avoid-hyphens
			 (or word-overfull overfull)
			 (first-overfull
			  overfull hyphen-overfull word-overfull)))
		      ;; #### NOTE: this underfull the last one.
		      (t underfull)))
	       (:best
		(cond (fit fit)
		      ((and underfull (not overfull))
		       ;; #### NOTE: this underfull the last one.
		       underfull)
		      ((and overfull (not underfull))
		       (first-overfull overfull hyphen-overfull word-overfull))
		      (t
		       (if avoid-hyphens
			 (cond ((and word-underfull (not word-overfull))
				word-underfull)
			       ((and (not word-underfull) word-overfull)
				word-overfull)
			       ((and word-underfull word-overfull)
				(best-*full word-underfull word-underfull-w
					    word-overfull word-overfull-w
					    width prefer-overfull-lines))
			       (t
				(best-*full underfull underfull-w
					    overfull overfull-w
					    width prefer-overfull-lines)))
			 (let* ((first-overfull
				  (first-overfull
				   overfull hyphen-overfull word-overfull))
				(first-overfull-w
				  (if (eq first-overfull hyphen-overfull)
				    hyphen-overfull-w
				    word-overfull-w)))
			   (best-*full underfull underfull-w
				       first-overfull first-overfull-w
				       width prefer-overfull-lines))))))))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :fixed))
     &rest keys
     &key variant avoid-hyphens prefer-overfull-lines)
  (declare (ignore variant avoid-hyphens prefer-overfull-lines))
  (loop :for start := 0 :then (next-start boundary)
	:until (= start (length lineup))
	:for boundary := (apply #'fixed-line-boundary lineup start width keys)
	:collect (create-line lineup start (stop boundary))))
