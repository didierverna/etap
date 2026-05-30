(in-package :etap)
(in-readtable :etap)

(defparameter *moby-dick-paragraphs*
  (with-open-file (input (asdf:system-relative-pathname
			  :etap "share/text/moby-dick"
			  :type "txt"))
    (loop :for par := (read input nil input)
	  :until (eql par input)
	  :collect par))
  "The Moby Dick paragraphs (a list of strings).
This list is read read from share/text/moby-dick.txt in the distribution.")


(defvar *text*
  "In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful; and the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
Close by the king's castle lay a great dark forest, and under an old lime-tree
in the forest was a well, and when the day was very warm, the king's child
went out into the forest and sat down by the side of the cool fountain; and
when she was bored she took a golden ball, and threw it up on high and caught
it; and this ball was her favorite plaything."
  "The default text.")
