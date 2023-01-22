(in-package :cl-user)

;; #### FIXME: this is NOT the way to do it!
(net.didierverna.tfm:nickname-package)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:export
    :make-context :*context* :make-context-lineup :make-context-paragraph
    :report-solutions
    #+lispworks :run))

