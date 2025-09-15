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

;; #### NOTE: currently, no algorithm refuses to typeset a non-empty text
;; (they all return a last-resort fallback solution) so we can save a couple
;; of checks below.

(defun collect-fulls (lineup widths)
  "Collect ALGORITHM's number of under/full lines per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (loop :with fulls := 0
		  :for lines
		    :on (lines
			 (get-layout
			  0
			  (make-breakup :lineup lineup :width width)))
		  :for w := (width (car lines))
		  :when (or (> w width)
			    ;; Do not count an underfull last line.
			    (and (< w width) (cdr lines)))
		    :do (incf fulls)
		  :finally (return fulls)))
    widths))

(defun collect-hyphenation (lineup widths)
  "Collect ALGORITHM's number of hyphenated lines per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (reduce #'+ (lines
			 (get-layout
			  0
			  (make-breakup :lineup lineup :width width)))
	      :key (lambda (line) (if (hyphenated line) 1 0))))
    widths))

(defun collect-asar-mean (lineup widths)
  "Collect LINEUP's average ASAR per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let ((asars
		    (mapcar #'asar
		      (lines
		       (get-layout
			0
			(make-breakup :lineup lineup :width width))))))
	      (float (/ (reduce #'+ asars) (length asars)))))
    widths))

(defun collect-asars-variance (lineup widths)
  "Collect ALGORITHM's line scale variance per paragraph WIDTHS."
  (mapcar (lambda (width)
	    (let* ((asars
		     (mapcar #'asar
		       (lines
			(get-layout
			 0
			 (make-breakup :lineup lineup :width width)))))
		   (length (length asars))
		   (mean (float (/ (reduce #'+ asars) length))))
	      (sqrt (/ (reduce #'+
			   (mapcar (lambda (asar)
				     (expt (- asar mean) 2))
			     asars))
		       length))))
    widths))

(defun collect-demerits (lineup widths)
  "Collect TeX's demerits evaluation per paragraph WIDTHS."
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
  (calibrate-kp hyphen-penalty)
  (calibrate-kp explicit-hyphen-penalty)
  (loop :with harray := (harray lineup)
	:for i :from 0 :upto (1- (length harray))
	:for item := (aref harray i)
	:when (discretionaryp item)
	  :do (setf (penalty item)
		    (if (pre-break item)
		      *hyphen-penalty*
		      *explicit-hyphen-penalty*)))
  (mapcar (lambda (width)
	    (let* ((lines (lines
			   (get-layout
			    0
			    (make-breakup :lineup lineup :width  width))))
		   (length (length lines))
		   (demerits
		     (let ((badness (sar-badness (asar (car lines)))))
		       (if (numberp badness)
			 (local-demerits
			  badness (penalty (car lines)) *line-penalty*)
			 0))))
	      (when (= (sar-fitness-class (asar (car lines))) 0)
		(setf demerits (+ demerits *adjacent-demerits*)))
	      (loop :for line1 :in lines :for line2 :in (cdr lines)
		    :do (progn
			  (setq demerits
				(+ demerits
				   (let ((badness
					   (sar-badness (asar line2))))
				     (if (numberp badness)
				       (local-demerits
					badness (penalty line2) *line-penalty*)
				       0))))
			  (when (and (hyphenated line1) (hyphenated line2))
			    (setq demerits
				  (+ demerits *double-hyphen-demerits*)))
			  (when (> (abs (- (sar-fitness-class (asar line1))
					   (sar-fitness-class (asar line2))))
				   1)
			    (setq demerits (+ demerits *adjacent-demerits*)))))
	      (when (and (> length 1) (hyphenated (nth (- length 2) lines)))
		(setq demerits (+ demerits *final-hyphen-demerits*)))
	      (float demerits)))
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
		 (funcall collect
		   (make-lineup :algorithm (cdr algorithm)) widths))
	     algorithms)))
    (apply #'mapc (lambda (width &rest values)
		    (format t "~A~{ ~A~}~%" width values))
	   widths values))
  (values))
