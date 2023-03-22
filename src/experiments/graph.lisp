(in-package :etap)

(defun graph-statistics ()
  (let* ((*context* *experiments-context*)
	 (lineup (make-lineup :hyphenation nil))
	 (hyphenated-lineup (make-lineup)))
    (setq lineup (make-array (length lineup) :initial-contents lineup))
    (setq hyphenated-lineup (make-array (length hyphenated-lineup)
					:initial-contents hyphenated-lineup))
    (loop :initially
      (format t "~&Width Strict Strict/Hyphens Regular Regular/Hyphens~%")
	  :for width :from *paragraph-min-width* :to *paragraph-max-width*
	  :for stricts
	    := (layouts (make-graph lineup width :strict t))
	  :for hyphenated-stricts
	    := (layouts (make-graph hyphenated-lineup width :strict t))
	  :for regulars
	    := (layouts (make-graph lineup width))
	  :for hyphenated-regulars
	    := (layouts (make-graph hyphenated-lineup width))
	  :do (format t "~S ~S ~S ~S ~S~%"
		width
		(length stricts) (length hyphenated-stricts)
		(length regulars) (length hyphenated-regulars)))))
