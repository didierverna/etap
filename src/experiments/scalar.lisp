(in-package :etap)

(defvar *experiments-context*
  (make-context
   :disposition :justified
   :features '(:kerning t :ligatures t :hyphenation t)))

(defvar *experiments-algorithms*
  '(("Best-Fit" :fit :variant :best :fallback :anyfull)
    ("Barnett" :barnett)
    ("Duncan" :duncan)
    ("Knuth-Plass/Graph" :knuth-plass :variant :graph)
    ("Knuth-Plass/Dynamic" :knuth-plass :variant :dynamic)))

(defun collect-fulls (lineup widths algorithm &rest options)
  "Collect ALGORITHM's number of under/full lines per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (loop :with fulls := 0
		  :for lines :on (apply #'make-lines lineup :justified width
					algorithm options)
		  :for w := (width (car lines))
		  :when (or (> w width)
			    ;; Do not count an underfull last line.
			    (and (< w width) (cdr lines)))
		    :do (incf fulls)
		  :finally (return fulls)))
    widths))

(defun collect-hyphenation (lineup widths algorithm &rest options)
  "Collect ALGORITHM's number of hyphenated lines per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (reduce #'+ (apply #'make-lines lineup :justified width
			       algorithm options)
	      :key (lambda (line) (if (hyphenated line) 1 0))))
    widths))

(defun collect-scales-mean (lineup widths algorithm &rest options)
  "Collect ALGORITHM's average line scale per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let ((scales
		    (mapcar #'scale
		      (apply #'make-lines lineup :justified width
			     algorithm options))))
	      (float (/ (reduce #'+ scales) (length scales)))))
    widths))

(defun collect-scales-variance (lineup widths algorithm &rest options)
  "Collect ALGORITHM's line scale variance per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let* ((scales
		     (mapcar #'scale
		       (apply #'make-lines lineup :justified width
			      algorithm options)))
		   (length (length scales))
		   (mean (float (/ (reduce #'+ scales) length))))
	      (sqrt (/ (reduce #'+
			   (mapcar (lambda (scale)
				     (expt (- scale mean) 2))
			     scales))
		       length))))
    widths))

;; #### FIXME: the way we make lines currently implies that all lineup /
;; boundary information is lost. As a consequence, we can only handle global
;; options here. For example, there's no way to track specific penalties that
;; would be different from the global value (this currently doesn't happen
;; except for the last glue in KP, but it is unbreakable).
;; In general there are two solutions to this problem: either we change our
;; line representation to keep track of the lineup elements, or we enrich the
;; line class with what we need.
(defun collect-demerits
    (lineup widths algorithm
     &rest options
     &key hyphen-penalty explicit-hyphen-penalty line-penalty
	  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     &allow-other-keys)
  "Collect TeX's demerits evaluation per paragraph WIDTHS."
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  (mapcar (lambda (width)
	    (let* ((lines (apply #'make-lines lineup :justified width
				 algorithm options))
		   (length (length lines))
		   (demerits (local-demerits
			      (scale-badness (scale (car lines)))
			      (case (hyphenated (car lines))
				(:explicit explicit-hyphen-penalty)
				(:implicit hyphen-penalty)
				(nil 0))
			      line-penalty)))
	      (loop :for line1 :in lines :for line2 :in (cdr lines)
		    :do (progn
			  (setq demerits
				(++ demerits
				    (local-demerits
				     (scale-badness (scale line2))
				     (case (hyphenated line2)
				       (:explicit explicit-hyphen-penalty)
				       (:implicit hyphen-penalty)
				       (nil 0))
				     line-penalty)))
			  (when (and (hyphenated line1) (hyphenated line2))
			    (setq demerits
				  (++ demerits double-hyphen-demerits)))
			  (when (> (abs (- (scale-fitness-class (scale line1))
					   (scale-fitness-class (scale line2))))
				   1)
			    (setq demerits (++ demerits adjacent-demerits)))))
	      (when (and (> length 1) (hyphenated (nth (- length 2) lines)))
		(setq demerits (++ demerits final-hyphen-demerits)))
	      (float demerits)))
    widths))

(defun statistics (collect)
  "COLLECT and print statistics per paragraph width and algorithm."
  (format t "Width~{ ~A~}~%" (mapcar #'car *experiments-algorithms*))
  (let* ((widths (loop :for width
		       :from *paragraph-min-width*
			 :to *paragraph-max-width*
		       :collect width))
	 (*context* *experiments-context*)
	 (values (mapcar (lambda (algorithm)
			   (apply collect (make-lineup) widths (cadr algorithm)
				  (cddr algorithm)))
		   *experiments-algorithms*)))
    (apply #'mapc (lambda (width &rest values)
		    (format t "~S~{ ~S~}~%" width values))
	   widths values))
  (values))
