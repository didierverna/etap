(in-package :cl-user)

(defpackage :etap
  #+lispworks (:add-use-defaults t)
  (:use #+lispworks :capi #-lispworks :cl)
  (:export #+lispworks :run :report-solutions))
