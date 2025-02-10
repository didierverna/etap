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


;; #### NOTE: this function doesn't make a distinction between no rendition
;; because of an empty harray, and no rendition because no solution. This is
;; not currently a problem because our algorithms never refuse to typeset.
(defgeneric renditions-# (breakup)
  (:documentation "Return the number of renditions in BREAKUP."))

(defgeneric get-rendition (nth breakup)
  (:documentation "Get the Nth rendition from BREAKUP."))
