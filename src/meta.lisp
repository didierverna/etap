(in-package :cl-user)

;; #### FIXME: this is NOT the way to do it!
(net.didierverna.tfm:nickname-package)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:shadow :i+ :i/
   ;; CAPI exports these and it sucks.
   :item :layout)
  (:export
    :make-context :*context*
    :make-lineup
    :make-paragraph
    #+lispworks :run))
