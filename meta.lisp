(in-package :cl-user)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:export
    :make-context :create-lineup :create-paragraph
    :report-solutions
    #+lispworks :run))
