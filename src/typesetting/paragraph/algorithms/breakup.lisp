(in-package :etap)

(defclass breakup ()
  ()
  (:documentation "The BREAKUP class.
This is the base class for breakups."))

;; #### NOTE: this function is called with all the algorithm options, without
;; knowing in advance whether they're going to be used or not, so we need to
;; relax keyword argument checking. Also, each algorithm is responsible for
;; instantiating the appropriate breakup class, so we cannot short-circuit
;; anything here in case of an empty harray.
(defgeneric break-harray
    (harray disposition width beds algorithm &key &allow-other-keys)
  (:documentation
   "Break HARRAY as a DISPOSITION paragraph of WIDTH with ALGORITHM.
Maybe include river BEDS."))

(defun %make-breakup (lineup disposition width beds algorithm)
  "Make a new breakup out of LINEUP for a DISPOSITION paragraph of WITH.
Use ALGORITHM to do so. Maybe include river BEDS."
  (apply #'break-harray (harray lineup) disposition width beds
	 (algorithm-type algorithm) (algorithm-options algorithm)))


;; #### TODO: the two protocols below will change when we give the interface
;; the ability to visualize different breakup results.

(defgeneric pinned-lines (breakup)
  (:documentation "Return BREAKUP's number of pinned lines."))

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
		 :initform nil :initarg :pinned-lines :reader pinned-lines))
  (:documentation "The Simple Breakup class.
This class allows the storage of a single breaking solution. It is thus
adequate for greedy algorithms making only discrete choices. Current
algorithms using it are Fixed and Barnett."))
