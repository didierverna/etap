(in-package :etap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (net.didierverna.tfm:nickname-package))

(define-constant +initial-text+
  "In olden times when wishing still helped one, there lived a king whose
daughters were all beautiful; and the youngest was so beautiful that the sun
itself, which has seen so much, was astonished whenever it shone in her face.
Close by the king's castle lay a great dark forest, and under an old lime-tree
in the forest was a well, and when the day was very warm, the king's child
went out into the forest and sat down by the side of the cool fountain; and
when she was bored she took a golden ball, and threw it up on high and caught
it; and this ball was her favorite plaything.")

(define-constant +font-file+
  (asdf:system-relative-pathname :etap #p"share/ec-lmr10.tfm"))

(defclass state ()
  ((font :initform (tfm:load-font +font-file+ :freeze t) :reader font)
   (hyphenation-rules :initform (create-hyphenation-rules)
		      :reader hyphenation-rules)
   (algorithm :initform '(:fixed) :initarg :algorithm :accessor algorithm)
   (disposition :initform '(:flush-left) :initarg :disposition
		:accessor disposition)
   (features :initform (list) :initarg :features :accessor features)
   ;; 284.52756pt = 10cm
   (paragraph-width :initform 284 :initarg :paragraph-width
		    :accessor paragraph-width)
   (text :initform +initial-text+ :initarg :text :accessor text)))

(defun make-state
    (&rest keys &key algorithm disposition features paragraph-width text)
  (apply #'make-instance 'state keys))
