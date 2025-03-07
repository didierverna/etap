;; With the default text, there is 130 possible break points with hyphenation,
;; and 111 without. The total number of break possibilities is 2^n which is
;; huge, but most of those are silly when you try to justify to a certain
;; width.

(in-package :etap)

;; #### TODO: for experimentation, we could make PREVENTIVE-*FULLS a number
;; instead of just a Boolean, for keeping more than 1 *full.

(defun get-boundaries (harray bol width &key fulls strict)
  "Get boundaries for an HARRAY line of WIDTH beginning at BOL.
A boundary is acceptable if the required scaling remains within [-1,1].

If no acceptable boundary is found, return NIL, unless FULLS, in which case
return the last underfull and the first overfull (if any) as a fallback
solution. If FULLS is :PREVENTIVE, also return these fulls even if acceptable
boundaries are found.

If STRICT, consider that even the last line must fit exactly. Otherwise
(the default), consider a final underfull as a fit.

The possible endings are listed in reverse order (from last to first)."
  (loop :with underfull :with fits := (list) :with overfull
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol (not overfull))
	:for boundary := (make-instance 'fit-boundary
			   :harray harray :bol bol :break-point eol
			   :target width)
	:do (cond (($< (max-width boundary) width)
		   (if (and (eopp eol) (not strict))
		     (push boundary fits)
		     (setq underfull boundary)))
		  ((> (min-width boundary) width)
		   (setq overfull boundary))
		  (t ;; note the reverse order
		   (push boundary fits)))
	:finally
	   (return (cond ((eq fulls :preventive)
			  (append (when overfull (list overfull))
				  fits
				  (when underfull (list underfull))))
			 (fulls
			  (or fits
			      (append (when overfull (list overfull))
				      (when underfull (list underfull)))))
			 (t fits)))))

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
	  :for hash1 := (make-graph (harray lineup) width
				    #'get-boundaries)
	  :for hash2 := (make-graph (harray lineup) width
				    (lambda (harray bol width)
				      (get-boundaries harray bol width
					:fulls t)))
	  :for hash3 := (make-graph (harray lineup) width
				    (lambda (harray bol width)
				      (get-boundaries harray bol width
					:fulls :preventive)))
	  :do (format t "~&~S ~S ~S ~S~%"
		width
		(hash-table-counts hash1)
		(hash-table-counts hash2)
		(hash-table-counts hash3)))))

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
	    := (make-graph-paths (make-graph (harray lineup) width
					     (lambda (harray bol width)
					       (get-boundaries harray bol width
						 :strict t))))
	  :for hyphenated-stricts
	    := (make-graph-paths (make-graph (harray hyphenated-lineup) width
					     (lambda (harray bol width)
					       (get-boundaries harray bol width
						 :strict t))))
	  :for regulars
	    := (make-graph-paths (make-graph (harray lineup) width
					     #'get-boundaries))
	  :for hyphenated-regulars
	    := (make-graph-paths (make-graph (harray hyphenated-lineup) width
					     #'get-boundaries))
	  :do (format t "~S ~S ~S ~S ~S~%"
		width
		(length stricts) (length hyphenated-stricts)
		(length regulars) (length hyphenated-regulars)))))
