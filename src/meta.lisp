(in-package :cl-user)

(defpackage :etap
  (:local-nicknames (:tfm :net.didierverna.tfm))
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  #+sbcl
  (:import-from :sb-mop :validate-superclass)
  #+lispworks
  (:shadow
   ;; From the Lispworks package:
   :when-let
   ;; From the CAPI package (this sucks):
   :item :layout)
  (:export
    :*language* :*text* :*paragraph-width*
    :*context* :make-context
    :make-hlist :make-lineup :make-breakup
    #+lispworks :update-interface #+lispworks :run))
