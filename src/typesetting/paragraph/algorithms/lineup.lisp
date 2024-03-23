;; A `lineup' is an object representing the paragraph material ready for
;; typesetting. The actual material is stored into a so-called `harray' (the
;; equivalent of an hlist, in array form).

;; An harray can be as simple as an array version of the original hlist, but
;; contrary to the hlist, it depends on the final paragraph disposition and
;; typesetting algorithm in use. Most algorithms post-process the hlist (for
;; example by adjusting penalties or adding glues).

;; In addition to the harray, the lineup also stores the computation of the
;; break points and theoretical solutions numbers. We associate those numbers
;; with the harray rather than with the hlist because some algorithms have
;; control over the availability of the original break points (e.g. by putting
;; an infinite penalty on hyphenation points).

(in-package :etap)


;; ==========================================================================
;; HArray Manipulation
;; ==========================================================================

;; ------
;; Access
;; ------

(defun haref (harray i start stop &aux (element (aref harray i)))
  "Return HARRAY element at position I, between START and STOP boundaries.
If element is a discretionary, return the appropriate pre/no/post break part."
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the hlist, including
    ;; ligatures / kerning management in the presence of hyphenation points,
    ;; we may end up with harrays beginning or ending with discretionaries (or
    ;; even consecutive discretionaries for that matter). When discretionaries
    ;; begin or end the harray, we must not consider them as post- or
    ;; pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- stop)) (not (= stop (length harray))))
	   (pre-break element))
	  (t (no-break element)))
    element))


;; --------
;; Geometry
;; --------

(defun harray-width (harray start stop)
  "Compute HARRAY's width between START and STOP.
Return five values: the natural, maximum, and minimum width, followed by the
stretch and shrink amounts."
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- stop)
	;; #### FIXME: this works for now, but it is not quite right in the
	;; general case. When ELEMENT is a list (typically the contents of a
	;; discretionary, there could be anything inside, including, e.g.,
	;; glues. See also the long comment above the KERNING function.
	:for element := (haref harray i start stop)
	:do (incf width (width element))
	:when (gluep element)
	  :do (setq stretch ($+ stretch (stretch element))
		    shrink (+ shrink (shrink element)))
	:finally (return (values width ($+ width stretch) (- width shrink)
				 stretch shrink))))

(defun harray-max-width (harray start stop)
  "Return HARRAY's width between START and STOP, with maximal stretching."
  (multiple-value-bind (natural max) (harray-width harray start stop)
    (declare (ignore natural))
    max))

(defun harray-min-width (harray start stop)
  "Return HARRAY's width between START and STOP, with maximal shrinking."
  (multiple-value-bind (natural max min) (harray-width harray start stop)
    (declare (ignore natural max))
    min))


;; -------
;; Scaling
;; -------

(defun scaling (width target stretch shrink)
  "Return the amount of scaling required to reach TARGET from WIDTH.
The amount in question is 0 if WIDTH is equal to TARGET.
Otherwise, it's a possibly infinite stretching (positive) or shrinking
(negative) ratio relative to the elasticity provided by STRETCH and SHRINK."
  (cond ((= width target) 0)
	((< width target) ($/ (- target width) stretch))
	((< target width) ($/ (- target width) shrink))))

(defun harray-scale (harray start stop target &optional extra)
  "Return the amount of scaling required for HARRAY chunk between START and
STOP to reach TARGET width, possibly with EXTRA stretch.
See `scaling' for more information."
  (multiple-value-bind (width max min stretch shrink)
      (harray-width harray start stop)
    (declare (ignore max min))
    (when extra (setq stretch ($+ stretch extra)))
    (scaling width target stretch shrink)))


;; ----------
;; Boundaries
;; ----------

(defclass boundary ()
  ((item
    :documentation "The harray item at that boundary."
    :initarg :item :reader item)
   (stop-idx
    :documentation "The harray index for an end of line at that boundary."
    :initarg :stop-idx :reader stop-idx)
   (start-idx
    :documentation "The harray index for a beginning of line at that boundary."
    :initarg :start-idx :reader start-idx))
  (:default-initargs :allow-other-keys t) ;; allow :harray
  (:documentation "Base class for boundaries.
A boundary represents a possible break point in an harray.
The end of the harray is represented by a special boundary with a null item
and start index (the stop index being the harray's length).

Greedy algorithms may extend this class in order to memoize various aspects of
line computation (see `next-boundary'), essentially because all boundaries
subject to comparison at one point in time relate to the same beginning of
line. Non-greedy algorithms should not."))

(defmethod hyphenated ((boundary boundary))
  "Return BOUNDARY's hyphenation status."
  (hyphenated (item boundary)))

(defun last-boundary-p (boundary)
  "Return T if BOUNDARY is the last one."
  (null (item boundary)))

(defun next-boundary (harray from &optional (boundary-class 'boundary)
				  &rest keys &key &allow-other-keys
				  &aux (length (length harray)))
  "Return the next boundary in HARRAY FROM position, or NIL.
The returned object is an instance of BOUNDARY-CLASS (BOUNDARY by default).
This function understands the terminal case where FROM = HARRAY's length
(possibly coming from the end of harray special boundary), in which case it
signals that there is no more boundary to find by returning NIL."
  (unless (= from length)
    (let* ((idx (position-if #'break-point-p harray :start (1+ from)))
	   (item (when idx (aref harray idx)))
	   stop-idx start-idx)
      (etypecase item
	(glue (setq stop-idx idx start-idx (1+ idx)))
	(discretionary (setq stop-idx (1+ idx) start-idx idx))
	(null (setq stop-idx length)))
      (apply #'make-instance boundary-class
	     :item item :stop-idx stop-idx :start-idx start-idx :harray harray
	     keys))))




;; ==========================================================================
;; Lineup Creation
;; ==========================================================================

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
