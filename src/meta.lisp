(in-package :cl-user)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:local-nicknames (:tfm :net.didierverna.tfm))
  (:use #+lispworks :capi #-lispworks :cl)
  (:shadow :$+ :$/
   ;; CAPI exports these and it sucks.
   :item :layout)
  (:export
    :*language* :*text* :*paragraph-width*
    :*context* :make-context
    :make-hlist :make-lineup :make-breakup :make-paragraph
    #+lispworks :run))
