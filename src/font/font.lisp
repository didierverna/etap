(in-package :etap)

(defvar *font-file*
  (asdf:system-relative-pathname :etap #p"share/ec-lmr10.tfm")
  "The TFM font file.")

(defvar *font*
  (tfm:load-font *font-file* :freeze t)
  "The TFM font.")
