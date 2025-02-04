(in-package :etap)

(defabstract breakup ()
  ((disposition
    :documentation "The breakup's disposition."
    :initarg :disposition :reader disposition)
   (width :documentation "The breakup's width."
	  :initarg :width :reader width))
  (:documentation "The BREAKUP class.
A breakup is the result of running a paragraph formatting algorithm on an
harray for a specific paragraph width."))

;; #### NOTE: this function is called with all the algorithm options, without
;; knowing in advance whether they're going to be used or not, so we need to
;; relax keyword argument checking. Also, each algorithm is responsible for
;; instantiating the appropriate breakup class, so we cannot short-circuit
;; anything here in case of an empty harray.
(defgeneric break-harray
    (harray disposition width beds algorithm &key &allow-other-keys)
  (:documentation
   "Break HARRAY as a DISPOSITION paragraph of WIDTH with ALGORITHM.
Maybe include river BEDS.
Return the resulting breakup."))

(defun %make-breakup (lineup disposition width beds algorithm)
  "Make a new breakup out of LINEUP for a DISPOSITION paragraph of WITH.
Use ALGORITHM to do so. Maybe include river BEDS."
  (apply #'break-harray (harray lineup) disposition width beds
	 (algorithm-type algorithm) (algorithm-options algorithm)))


;; #### TODO: the two protocols below will change when we give the interface
;; the ability to visualize different breakup results.

(defgeneric pinned-lines (breakup)
  (:documentation "Return BREAKUP's pinned lines."))

(defun lines-# (breakup)
  "Return BREAKUP's number of lines."
  (length (pinned-lines breakup)))

(defmethod properties strnlcat
    ((breakup breakup) &aux (lines-# (lines-# breakup)))
  "Addvertise BREAKUP's number of lines."
  (unless (zerop lines-#) (format nil "~A line~:P." (lines-# breakup))))


;; ---------------
;; Simple Breakups
;; ---------------

(defclass simple-breakup (breakup)
  ((pinned-lines :documentation "The pinned lines."
		 :initform nil :reader pinned-lines))
  (:documentation "The Simple Breakup class.
This class allows the storage of a single breaking solution. It is thus
adequate for greedy algorithms making only discrete choices. Current
algorithms using it are Fixed, Fit, and Barnett."))

(defmethod initialize-instance :after ((breakup simple-breakup) &key lines)
  "Pin LINES in BREAKUP."
  (when lines
    (setf (slot-value breakup 'pinned-lines)
	  (loop :with width := (width breakup)
		:with baseline-skip := (baseline-skip (harray (car lines)))
		:with x := (case (disposition-type (disposition breakup))
			     ((:flush-left :justified)
			      (lambda (line) (declare (ignore line)) 0))
			     (:centered
			      (lambda (line) (/ (- width (width line)) 2)))
			     (:flush-right
			      (lambda (line) (- width (width line)))))
		:for y := 0 :then (+ y baseline-skip)
		:for line :in lines
		:collect (pin-line line (funcall x line) y)))))
