(in-package :cl-user)

;; #### FIXME: this is NOT the way to do it!
(net.didierverna.tfm:nickname-package)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:shadow :$+ :$/
   ;; CAPI exports these and it sucks.
   :item :layout)
  (:export
    :*language*
    :*text*
    :*context* :make-context
    :make-hlist
    :make-lineup
    :make-paragraph
    #+lispworks :run))
