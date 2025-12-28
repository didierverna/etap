(in-package :etap)
(in-readtable :etap)

(defparameter *fonts*
  (mapc (lambda (description)
	  (setf (car description)
		(tfm:load-font
		 (asdf:system-relative-pathname :etap
		   (make-pathname :directory '(:relative "share/fonts/")
				  :name (car description)
				  :type "tfm"))
		 :freeze t)))
    '(("ec-lmr10"
       :family "Latin Modern Roman" :slant :roman :weight :normal :size 10)
      ("ec-lmbx10"
       :family "Latin Modern Roman" :slant :roman :weight :bold :size 10)
      ("ec-lmri10"
       :family "Latin Modern Roman" :slant :italic :weight :normal :size 10)
      ("ec-lmbxi10"
       :family "Latin Modern Roman" :slant :italic :weight :bold :size 10)))
  "The list of fonts known to ETAP.
Each entry is of the form (FONT . DESCRIPTION) where FONT is a TFM font, and
DESCRIPTION is a property list describing the font and suitable to a CAPI
graphics port.")

(defvar *font* (first (first *fonts*))
  "The current TFM font.")
