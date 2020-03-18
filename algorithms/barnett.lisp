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

(in-package :etap)

(defun barnett-create-line (lineup start end search width sloppy)
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
  (loop :for start := 0 :then next-start
	:until (= start (length lineup))
	:for (end next-start next-search)
	  := (barnett-line-boundary start lineup width)
	:collect (barnett-create-line lineup start end next-search width
				      sloppy)))
