(in-package :cl-user)

;; #### FIXME: this is NOT the way to do it!
(net.didierverna.tfm:nickname-package)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:shadow :++
   ;; CAPI exports this and it sucks.
   :item)
  (:export
    :make-context :*context*
    :make-lineup
    :make-paragraph
    #+lispworks :run))
