(in-package :etap)
(in-readtable :etap)

(defpackage :etap-user
  (:use :cl :etap)
  (:export :bold :italic :bold-italic))

(in-package :etap-user)

(defun bold (string)
  "Typeset STRING in bold font."
  (let ((*font* (first (second *fonts*)))) (etap::slice string)))

(defun italic (string)
  "Typeset STRING in italic font."
  (let ((*font* (first (third *fonts*)))) (etap::slice string)))

(defun bold-italic (string)
  "Typeset STRING in bold italic font."
  (let ((*font* (first (fourth *fonts*)))) (etap::slice string)))
