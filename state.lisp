(in-package :etap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (net.didierverna.tfm:nickname-package))

(defconstant +initial-text+
  "In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful; and the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
Close by the king's castle lay a great dark forest, and under an old lime-tree
in the forest was a well, and when the day was very warm, the king's child
went out into the forest and sat down by the side of the cool fountain; and
when she was bored she took a golden ball, and threw it up on high and caught
it; and this ball was her favorite plaything.")

(defconstant +font-file+
  (asdf:system-relative-pathname :etap #p"share/ec-lmr10.tfm"))

(defclass state ()
  ((font :initform (tfm:load-font +font-file+) :reader font)
   (hyphenation-rules :initform (create-hyphenation-rules)
		      :reader hyphenation-rules)
   (algorithm :initform '(:fixed) :accessor algorithm)
   (disposition :initform :flush-left :accessor disposition)
   (features :initform (list) :accessor features)
   ;; 284.52756pt = 10cm
   (paragraph-width :initform 284 :accessor paragraph-width)
   (text :initform +initial-text+ :accessor text)))

(defun make-state ()
  (make-instance 'state))
