(in-package :etap)

(defparameter *font-file*
  (asdf:system-relative-pathname :etap #p"share/ec-lmr10.tfm")
  "The TFM font file.")

(defparameter *font*
  (tfm:load-font *font-file* :freeze t)
  "The TFM font.")
