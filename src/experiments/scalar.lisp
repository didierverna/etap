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
		  :for lines :on (pinned-lines
				  (apply #'break-harray (harray lineup)
					 :justified width nil algorithm
					 options))
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
	    (reduce #'+ (pinned-lines
			 (apply #'break-harray (harray lineup)
				:justified width nil algorithm
				options))
	      :key (lambda (line) (if (hyphenated line) 1 0))))
    widths))

(defun collect-scales-mean (lineup widths algorithm &rest options)
  "Collect ALGORITHM's average line scale per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let ((scales
		    (mapcar #'scale
		      (pinned-lines
		       (apply #'break-harray (harray lineup)
			      :justified width nil algorithm
			      options)))))
	      (float (/ (reduce #'+ scales) (length scales)))))
    widths))

(defun collect-scales-variance (lineup widths algorithm &rest options)
  "Collect ALGORITHM's line scale variance per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let* ((scales
		     (mapcar #'scale
		       (pinned-lines
			(apply #'break-harray lineup
			       :justified width nil algorithm
			       options))))
		   (length (length scales))
		   (mean (float (/ (reduce #'+ scales) length))))
	      (sqrt (/ (reduce #'+
			   (mapcar (lambda (scale)
				     (expt (- scale mean) 2))
			     scales))
		       length))))
    widths))

(defun collect-demerits
    (lineup widths algorithm
     &rest options
     &key
     ((:line-penalty *line-penalty*))
     ((:hyphen-penalty *hyphen-penalty*))
     ((:explicit-hyphen-penalty *explicit-hyphen-penalty*))
     ((:adjacent-demerits *adjacent-demerits*))
     ((:double-hyphen-demerits *double-hyphen-demerits*))
     ((:final-hyphen-demerits *final-hyphen-demerits*))
     &allow-other-keys)
  "Collect TeX's demerits evaluation per paragraph WIDTHS.
Infinite demerits are collected as \"NaN\", in order for subsequent plotting
to be able to handle that gracefully."
  (declare (special *line-penalty* *hyphen-penalty* *explicit-hyphen-penalty*
		    *adjacent-demerits* *double-hyphen-demerits*
		    *final-hyphen-demerits*))
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  ;; #### WARNING: the lineup is not prepared for collecting demerits here in
  ;; most cases, so we need to mimic the effect of PROCESS-HLIST first
  ;; (although on the harray this time).
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
  (loop :with harray := (harray lineup)
	:for i :from 0 :upto (1- (length harray))
	:for item := (aref harray i)
	:when (hyphenation-point-p item)
	  :do (setf (penalty item)
		    (if (explicitp item)
		      *explicit-hyphen-penalty*
		      *hyphen-penalty*)))
  (mapcar (lambda (width)
	    (let* ((lines (pinned-lines
			   (apply #'break-harray (harray lineup)
				  :justified width nil algorithm
				  options)))
		   (length (length lines))
		   (demerits (local-demerits
			      (scale-badness (scale (car lines)))
			      (penalty (car lines))
			      *line-penalty*)))
	      (when (= (scale-fitness-class (scale (car lines))) 0)
		(setf demerits ($+ demerits *adjacent-demerits*)))
	      (loop :for line1 :in lines :for line2 :in (cdr lines)
		    :do (progn
			  (setq demerits
				($+ demerits
				    (local-demerits
				     (scale-badness (scale line2))
				     (penalty line2)
				     *line-penalty*)))
			  (when (and (hyphenated line1) (hyphenated line2))
			    (setq demerits
				  ($+ demerits *double-hyphen-demerits*)))
			  (when (> (abs (- (scale-fitness-class (scale line1))
					   (scale-fitness-class (scale line2))))
				   1)
			    (setq demerits ($+ demerits *adjacent-demerits*)))))
	      (when (and (> length 1) (hyphenated (nth (- length 2) lines)))
		(setq demerits ($+ demerits *final-hyphen-demerits*)))
	      (if (numberp demerits) (float demerits) "NaN")))
    widths))

(defun scalar-statistics
    (collect &optional (algorithms *experiments-algorithms*))
  "COLLECT and print scalar statistics on standard output.
Statistics are collected for every paragraph width from *PARAGRAPH-MIN-WIDTH*
to *PARAGRAPH-MAX-WIDTH*, and for each of the provided ALGORITHMS
(*experiments-algorithms* by default).

COLLECT is a function which collects the actual results. Currently available
functions return the number of fulls, hyphens, average scale, scale variance,
and TeX demerits.

The output, suitable to Gnuplot, is of the following form:
Width Algorithm-1-name Algorithm-2-name ...
width1 scalar1 scalar2 ...
width2 scalar1 scalar2 ...
..."
  (format t "Width~{ ~A~}~%" (mapcar #'car algorithms))
  (let* ((widths (loop :for width
		       :from *paragraph-min-width*
			 :to *paragraph-max-width*
		       :collect width))
	 (*context* *experiments-context*)
	 (values
	   (mapcar
	       (lambda (algorithm)
		 (apply collect
		   (make-lineup :algorithm (cdr algorithm))
		   widths
		   (cadr algorithm)
		   (cddr algorithm)))
	     algorithms)))
    (apply #'mapc (lambda (width &rest values)
		    (format t "~A~{ ~A~}~%" width values))
	   widths values))
  (values))
