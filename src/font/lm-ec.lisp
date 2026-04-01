(in-package :etap)
(in-readtable :etap)

(defparameter *lm-ec*
  (let ((map (make-array (length *lm-ec-encoding*))))
    (loop :for i :from 0 :upto (1- (length map))
	  :do (setf (svref map i)
		    (gethash (svref *lm-ec-encoding* i) *glyph-list*)))
    map)
  "The Latin Modern Extended Cork character map.
This is an array of characters in the LM EC encoding's order.")
