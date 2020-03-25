;; This is the Knuth-Plass algorithm from: Knuth, Donald E.; Plass, Michael F.
;; (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)


;; #### FIXME: same as Duncan, Barnett, and Fit Justified.
(defun kp-create-line (lineup start end width sloppy)
  (let ((scale (lineup-scale lineup start end width)))
    (if scale
      (create-line lineup start end
		   (cond (sloppy scale)
			 ((zerop scale) 0)
			 ((< scale 0) (max scale -1))
			 ((> scale 0) (min scale 1))))
      (create-line lineup start end))))


(defmethod create-lines
    (lineup disposition width (algorithm (eql :knuth-plass)) &key sloppy)
)
