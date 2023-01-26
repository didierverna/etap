(in-package :cl-user)

;; #### FIXME: this is NOT the way to do it!
(net.didierverna.tfm:nickname-package)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:export
    :make-context :*context*
    :make-lineup
    :make-paragraph
    #+lispworks :run))

