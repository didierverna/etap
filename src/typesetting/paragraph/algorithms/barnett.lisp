;; This is the Barnett algorithm from: Michael P. Barnett, Computer
;; Typesetting: Experiments and Prospects, M.I.T. Press, Cambridge,
;; Massachusetts, 1965.

;; I don't have the book, but Knuth describes it as follows in the Knuth-Plass
;; paper: "Keep appending words to the current line, assuming the normal
;; spacing, until reaching a word that does not fit. Break after this word, if
;; it is possible to do so without compressing the spaces to less than the
;; given minimum; otherwise break before this word, if it is possible to do so
;; without expanding the spaces to more than the given maximum. Otherwise
;; hyphenate the offending word, putting as much of it on the current line as
;; will fit; if no suitable hyphenation points can be found, this may result
;; in a line whose spaces exceed the given maximum".

;; From what I gather, this algorithm looks like a mixture of different *-Fit
;; policies, but not quite, for the following reasons.
;; 1. It is similar to the Best Fit, in that it tries the normal spacing
;;    first, but if the line with the additional word can be shrunk, it will
;;    do so even if stretching without that word would have been closer to the
;;    normal spacing.
;; 2. When hyphenation is necessary, it appears to put in "as much as will
;;    fit", which is a form of Last Fit (maybe putting a bit less in would
;;    have given a result closer to the normal spacing).
;; 3. Finally, the last sentence seems to suggest that when no solution is
;;    found, this algorithm falls back to an underfull line, but overstretches
;;    it to reach the paragraph's width. That's why the "Overstretch" option
;;    has no effect (it's always on).

;; #### FIXME: I don't know if Barnett is restricted to the Justified
;; disposition, or if it does something for the ragged ones. Currently, I'm
;; just creating lines intended for justification, and putting them back to
;; normal spacing otherwise. Given what this algorithm does, it results in
;; many overfulls.

(in-package :etap)


;; ==========
;; Boundaries
;; ==========

(defclass barnett-boundary (fixed-boundary)
  ((scale :documentation "This boundary's required scaling."
	  :reader scale))
  (:documentation "The Barnett algorithm's boundary class."))

(defmethod initialize-instance :after
    ((boundary barnett-boundary) &key natural-width width stretch shrink)
  "Initialize BOUNDARY's scale."
  (setf (slot-value boundary 'scale)
	(scaling natural-width width stretch shrink)))



;; =========
;; Algorithm
;; =========

(defun barnett-line-boundary (lineup start width)
  "Return the Barnett algorithm's view of the end of line boundary."
  (loop :with underword :with hyphens := (list) :with overword
	:for boundary := (next-boundary lineup start 'barnett-boundary
					:start start :width width)
	  :then (next-boundary lineup (stop-idx boundary) 'barnett-boundary
			       :start start :width width)
	:while (and boundary (not overword))
	;; #### NOTE: keeping hyphen solutions in reverse order is exactly
	;; what we need for putting "as much as will fit" on the line.
	:do (cond ((hyphenation-point-p (item boundary))
		   (push boundary hyphens))
		  ((<= (width boundary) width)
		   (setq underword boundary hyphens nil))
		  ((> (width boundary) width)
		   (setq overword boundary)))
	:finally
	   (return
	     (cond
	       ;; A word overfull that fits.
	       ((and overword (>== (scale overword) -1)) overword)
	       ;; A word underfull that fits.
	       ((and underword (<== (scale underword) 1)) underword)
	       ;; For hyphens, we stop at the first solution that needs not
	       ;; too much shrinking. We don't care if it needs too much
	       ;; stretching, because that would be less than what's needed
	       ;; for the word underfull, and this algorithm overstretches by
	       ;; definition.
	       (hyphens
		(loop :for hyphen :in hyphens
		      :when (>== (scale hyphen) -1) :do (return hyphen)
			:finally (return (or underword overword))))
	       (t
		(or underword overword))))))



;; ===========
;; Entry Point
;; ===========

;; #### NOTE: I'm handling the overshrink option below as in the other
;; algorithms, but I think that by construction, the only overfulls that we
;; can get are when there is no elasticity, so this option should have no
;; effect.
(defmethod make-lines
    (lineup disposition width (algorithm (eql :barnett))
     &key
     &aux (justified (eq (disposition-type disposition) :justified))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  "Typeset LINEUP with the Barnett algorithm."
  (loop :for start := 0 :then (start-idx boundary)
	:while start
	:for boundary := (barnett-line-boundary lineup start width)
	:for stop := (stop-idx boundary)
	:for scale := (scale boundary)
	:if (and justified (last-boundary-p boundary))
	  ;; Justified last line: maybe shrink it but don't stretch it.
	  :collect (if (<< scale 0)
		     (make-scaled-line lineup start stop scale overshrink nil)
		     (make-line lineup start stop))
	:else :if justified
	  ;; Justified regular line: make it always overstreched, and maybe
	  ;; overshrunk.
	  :collect (make-scaled-line lineup start stop scale overshrink t)
	:else
	  ;; Other dispositions: just switch back to normal spacing.
	  :collect (make-line lineup start stop)))
