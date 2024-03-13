;; With the default text, there is 130 possible break points with hyphenation,
;; and 111 without. The total number of break possibilities is 2^n which is
;; huge, but most of those are silly when you try to justify to a certain
;; width.

(in-package :etap)

;; Remember that we may have null entries for subgraphs that were attempted,
;; but lead to a dead-end.
(defun hash-table-count-non-null (hashtable)
  "Count non null entries in HASHTABLE."
  (loop :with count := 0
	:for value :being :the :hash-values :of hashtable
	:unless (null value)
	  :do (incf count)
	:finally (return count)))

(defun graph-sizes ()
  "Collect and print the solutions graphs sizes per paragraph width.
Numbers are collected for every paragraph width from *PARAGRAPH-MIN-WIDTH*
to *PARAGRAPH-MAX-WIDTH*, and for each of the four categories below:
- no fulls (so maybe no solution at all),
- fallback fulls (last underfull and first overfull if no line fit is found),
- preventive fulls (systematic last underfull and first overfull).

The output, suitable to Gnuplot, is of the following form:
Width Fulls/None Fulls/Fallback Fulls/Preventive
width1 no-fulls-size-1 fallback-fulls-size-1 preventive-fulls-size-1
width2 no-fulls-size-2 fallback-fulls-size-2 preventive-fulls-size-2
..."
  (let* ((*context* *experiments-context*)
	 ;; #### NOTE: the lineup is algorithm-dependent, but in this case, we
	 ;; will default to Fixed, which essentially does nothing special on
	 ;; it.
	 (lineup (make-lineup)))
    (loop :initially
      (format t "~&Width Fulls/None Fulls/Fallback Fulls/Preventive~%")
	  :for width :from *paragraph-min-width* :to *paragraph-max-width*
	  :for (nil hash1) := (multiple-value-list
			       (make-graph (harray lineup) width))
	  :for (nil hash2) := (multiple-value-list
			       (make-graph (harray lineup) width
					   :fulls t))
	  :for (nil hash3) := (multiple-value-list
			       (make-graph (harray lineup) width
					   :fulls :preventive))
	  :do (format t "~&~S ~S ~S ~S~%"
		width
		(hash-table-count-non-null hash1)
		(hash-table-count-non-null hash2)
		(hash-table-count-non-null hash3)))))

(defun graph-solutions ()
  "Collect and print the number of solutions per paragraph width.
Numbers are collected for every paragraph width from *PARAGRAPH-MIN-WIDTH*
to *PARAGRAPH-MAX-WIDTH*, and for each of the four categories below:
- no hyphens, justified last line,
- hyphens, justified last line,
- no hyphens, underfull last line,
- hyphens, underfull last line.

The output, suitable to Gnuplot, is of the following form:
Width Strict Strict/Hyphens Regular Regular/Hyphens
width1 strict1 strict/hyphens1 regular1 regular/hyphens1
width2 strict2 strict/hyphens2 regular2 regular/hyphens2
..."
  (let* ((*context* *experiments-context*)
	 ;; #### NOTE: the lineup is algorithm-dependent, but in this case, we
	 ;; will default to Fixed, which essentially does nothing special on
	 ;; it.
	 (lineup (make-lineup :hyphenation nil))
	 (hyphenated-lineup (make-lineup)))
    (loop :initially
      (format t "~&Width Strict Strict/Hyphens Regular Regular/Hyphens~%")
	  :for width :from *paragraph-min-width* :to *paragraph-max-width*
	  :for stricts
	    := (graph-layouts (make-graph (harray lineup) width
					  :strict t))
	  :for hyphenated-stricts
	    := (graph-layouts (make-graph (harray hyphenated-lineup) width
					  :strict t))
	  :for regulars
	    := (graph-layouts (make-graph (harray lineup) width))
	  :for hyphenated-regulars
	    := (graph-layouts (make-graph (harray hyphenated-lineup) width))
	  :do (format t "~S ~S ~S ~S ~S~%"
		width
		(length stricts) (length hyphenated-stricts)
		(length regulars) (length hyphenated-regulars)))))
