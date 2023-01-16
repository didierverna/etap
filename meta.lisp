(in-package :cl-user)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:export
    :make-context :*context* :make-context-lineup :make-context-paragraph
    :report-solutions
    #+lispworks :run))
