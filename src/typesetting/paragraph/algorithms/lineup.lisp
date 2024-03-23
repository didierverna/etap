;; A `lineup' is an object representing the paragraph material ready for
;; typesetting. The actual material is stored into a so-called `harray' (the
;; equivalent of an hlist, in array form).

;; In addition to the harray, the lineup also stores the computation of the
;; break points and theoretical solutions numbers. We associate those numbers
;; with the harray rather than with the hlist because some algorithms have
;; control over the availability of the original break points (e.g. by putting
;; an infinite penalty on hyphenation points).

(in-package :etap)

(defclass lineup ()
  ((harray
    :documentation "The lineup's harray."
    :initarg :harray :reader harray)
   (break-points-#
    :documentation "The number of break points."
    :initform 0 :reader break-points-#)
   (theoretical-solutions-#
    :documentation "The number of theoretical solutions (2^n)."
    :initform 0 :reader theoretical-solutions-#))
  (:documentation "The LINEUP class."))

(defmethod initialize-instance :after
    ((lineup lineup) &key &aux (harray (harray lineup)))
  "Compute LINEUP's number of break points and theoretical solutions."
  ;; #### NOTE: for clarity, we want to make a distinction between and empty
  ;; paragraph and a non-empty one with no break points. In the former case,
  ;; we state that we have 0 solutions, while in the later case we have one.
  (unless (zerop (length harray))
    (let ((break-points-#
	    (count-if (lambda (item)
			(and (break-point-p item) ($< (penalty item) +∞)))
		harray)))
      (setf (slot-value lineup 'break-points-#)
	    break-points-#
	    (slot-value lineup 'theoretical-solutions-#)
	    (expt 2 break-points-#)))))

;; #### NOTE: this function is called with all the algorithm options, without
;; knowing in advance whether they're going to be used or not, so we need to
;; relax keyword argument checking.
(defgeneric process-hlist (hlist disposition algorithm &key &allow-other-keys)
  (:documentation
   "Process HLIST for DISPOSITION in an ALGORITHM-specific way.
All primary methods must return a (possibly modified) HLIST.")
  (:method (hlist disposition algorithm &key)
    "Return hlist as-is. This is the default method."
    hlist))

;; #### WARNING: the DISPOSITION argument is currently unused, but will be
;; when we update the KP algorithm to handle ragged dispositions properly.
(defun %make-lineup
    (hlist disposition algorithm
     &aux (processed-hlist (when hlist
			     (apply #'process-hlist hlist disposition
				    (algorithm-type algorithm)
				    (algorithm-options algorithm)))))
  (make-instance 'lineup
    :harray (make-array (length processed-hlist)
	      :initial-contents processed-hlist)))
