(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Calibers
;; ==========================================================================

(defmacro define-paragraph-caliber (name min default max)
  "Define a NAMEd paragraph caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber paragraph ,name ,min ,default ,max))

;; 142.26378pt = 5cm, 284.52756pt = 10cm, 569.0551pt = 20cm
(define-paragraph-caliber width 142 284 569)


(defmacro calibrate-paragraph (name)
  "Calibrate NAMEd paragraph variable."
  `(calibrate paragraph ,name))
