(in-package :etap)

(defgeneric create-lines
    (lineup width disposition algorithm &key &allow-other-keys))

(defun badness (lineup start stop width
		&aux (scale (lineup-scale lineup start stop width)))
  (unless (or (null scale) (< scale -1))
    (* 100 (expt (abs scale) 3))))

(defun !< (x y)
  (cond ((and (numberp x) (numberp y)) (< x y))
	((and (null x) (null y)) nil)
	((null x) nil)
	((null y) t)))

(defun !+ (x y)
  (cond ((and (numberp x) (numberp y)) (+ x y))
	(t nil)))
