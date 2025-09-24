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
    ;; From font/font.lisp:
    :*font-file* :*font*
    ;; From language/hyphenation.lisp:
    :*lefthyphenmin* :*righthyphenmin*
    ;; From language/language.lisp:
    :*language*
    ;; From language/text.lisp:
    :*text*
    ;; From typesetting/paragraph/common.lisp:
    :disposition-type :disposition-options
    ;; From typesetting/paragraph/algorithms/common.lisp:
    :algorithm-type :algorithm-options
    ;; From context.lisp:
    :font :algorithm :disposition :features :paragraph-width :text :language
    :make-context :*context*
    ;; From interface/capi.lisp:
    #+lispworks :state #+lispworks :set-state #+lispworks :run
    ;; From entry.lisp:
    :make-lineup :make-breakup))
