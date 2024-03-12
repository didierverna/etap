;; A lineup is the result of post-processing a hash by a specific algorithm.
;; While a hash is a list of constituents, a lineup is an array of such. A
;; lineup can be as simple as an array version of the original hash, but some
;; algorithms perform additional treatment on the constituents, such as
;; adjusting penalties, inserting glues, etc.

(in-package :etap)


;; ==========================================================================
;; Lineup Creation
;; ==========================================================================

(defclass lineup ()
  ((hash
    :documentation "The lineup's original hash."
    :initarg :hash :reader hash)
   (contents
    :documentation "The lineup's contents."
    :initarg :contents :reader contents)
   (break-points-#
    :documentation "The number of break points."
    :initform 0 :reader break-points-#)
   (theoretical-solutions-#
    :documentation "The number of theoretical solutions (2^n)."
    :initform 0 :reader theoretical-solutions-#))
  (:documentation "The LINEUP class."))

(defmethod initialize-instance :after
    ((lineup lineup) &key &aux (contents (contents lineup)))
  "Compute LINEUP's number of break points and theoretical solutions."
  ;; #### NOTE: for clarity, we want to make a distinction between and empty
  ;; paragraph and a non-empty one with no break points. In the former case,
  ;; we state that we have 0 solutions, while in the later case we have one.
  (unless (zerop (length contents))
    (let ((break-points-#
	    (count-if (lambda (item)
			(and (break-point-p item) ($< (penalty item) +∞)))
		contents)))
      (setf (slot-value lineup 'break-points-#)
	    break-points-#
	    (slot-value lineup 'theoretical-solutions-#)
	    (expt 2 break-points-#)))))

;; #### WARNING: the DISPOSITION argument is currently unused, but will be
;; when we update the KP algorithm to handle ragged dispositions properly.
(defun %make-lineup
    (hash disposition algorithm
     &aux (contents (when hash
		      (apply #'process-hash hash disposition
			     (algorithm-type algorithm)
			     (algorithm-options algorithm)))))
  (make-instance 'lineup
    :hash hash
    :contents (make-array (length contents) :initial-contents contents)))

;; #### NOTE: this function's interface doesn't have an NLSTRING keyword
;; argument on purpose: it's more convenient to provide access to TEXT and
;; LANGUAGE directly, or rely on CONTEXT for either, or both of these.
(defun make-lineup
    (&key (context *context*)
	  (text (if context (text (nlstring context)) *text*))
	  (language (if context (language (nlstring context)) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (hash (%make-hash text language font kerning ligatures hyphenation)))
  "Make a new lineup.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, and FONT are defaulted from the corresponding
global variables, KERNING, LIGATURES, and HYPHENATION are defaulted from
FEATURES, DISPOSITION is defaulted to :flush-left, and ALGORITHM to :fixed."
  (%make-lineup hash disposition algorithm))




;; ==========================================================================
;; Lineup Manipulation
;; ==========================================================================

;; ---------
;; Utilities
;; ---------

(defun lineup-aref (lineup i start stop &aux (element (aref lineup i)))
  "Return LINEUP element at position I, between START and STOP boundaries.
If element is a discretionary, return the appropriate pre/no/post break part."
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the lineup,
    ;; including ligatures / kerning management in the presence of hyphenation
    ;; points, we may end up with lineups beginning or ending with
    ;; discretionaries (or even consecutive discretionaries for that matter).
    ;; When discretionaries begin or end the lineup, we must not consider them
    ;; as post- or pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- stop)) (not (= stop (length lineup))))
	   (pre-break element))
	  (t (no-break element)))
    element))


;; -------------
;; Lineup widths
;; -------------

(defun lineup-width (lineup start stop)
  "Compute LINEUP's width between START and STOP.
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
	:for element := (lineup-aref lineup i start stop)
	:do (incf width (width element))
	:when (gluep element)
	  :do (setq stretch ($+ stretch (stretch element))
		    shrink (+ shrink (shrink element)))
	:finally (return (values width ($+ width stretch) (- width shrink)
				 stretch shrink))))

(defun lineup-max-width (lineup start stop)
  "Return LINEUP's width between START and STOP, with maximal stretching."
  (multiple-value-bind (natural max) (lineup-width lineup start stop)
    (declare (ignore natural))
    max))

(defun lineup-min-width (lineup start stop)
  "Return LINEUP's width between START and STOP, with maximal shrinking."
  (multiple-value-bind (natural max min) (lineup-width lineup start stop)
    (declare (ignore natural max))
    min))


;; -------------
;; Lineup scales
;; -------------

(defun scaling (width target stretch shrink)
  "Return the amount of scaling required to reach TARGET from WIDTH.
The amount in question is 0 if WIDTH is equal to TARGET.
Otherwise, it's a possibly infinite stretching (positive) or shrinking
(negative) ratio relative to the elasticity provided by STRETCH and SHRINK."
  (cond ((= width target) 0)
	((< width target) ($/ (- target width) stretch))
	((< target width) ($/ (- target width) shrink))))

(defun lineup-scale (lineup start stop target &optional extra)
  "Return the amount of scaling required for LINEUP chunk between START and
STOP to reach TARGET width, possibly with EXTRA stretch.
See `scaling' for more information."
  (multiple-value-bind (width max min stretch shrink)
      (lineup-width lineup start stop)
    (declare (ignore max min))
    (when extra (setq stretch ($+ stretch extra)))
    (scaling width target stretch shrink)))


;; -----------------
;; Lineup boundaries
;; -----------------

(defclass boundary ()
  ((item
    :documentation "The lineup item at that boundary."
    :initarg :item :reader item)
   (stop-idx
    :documentation "The lineup index for an end of line at that boundary."
    :initarg :stop-idx :reader stop-idx)
   (start-idx
    :documentation
    "The lineup index for a beginning of line at that boundary."
    :initarg :start-idx :reader start-idx))
  (:default-initargs :allow-other-keys t) ;; allow :lineup
  (:documentation "Base class for lineup boundaries.
A boundary represents a possible break point in the lineup.
The end of the lineup is represented by a special boundary with a null item
and start index (the stop index being the lineup's length).

Greedy algorithms may extend this class in order to memoize various aspects of
line computation (see `next-boundary'), essentially because all boundaries
subject to comparison at one point in time relate to the same beginning of
line. Non-greedy algorithms should not.

All boundaries respond to the following pseudo-accessors, which see:
- `hyphenated'."))

(defmethod hyphenated ((boundary boundary))
  "Return BOUNDARY's hyphenation status."
  (hyphenated (item boundary)))

(defun last-boundary-p (boundary)
  "Return T if BOUNDARY is the last one."
  (null (item boundary)))

(defun next-boundary (lineup from &optional (boundary-class 'boundary)
				   &rest keys &key &allow-other-keys
				   &aux (length (length lineup)))
  "Return the next boundary in LINEUP FROM position, or NIL.
The returned object is an instance of BOUNDARY-CLASS (BOUNDARY by default).
This function understands the terminal case where FROM = LINEUP's
length (possibly coming from the end of lineup special boundary), in which
case it signals that there is no more boundary to find by returning NIL."
  (unless (= from length)
    (let* ((idx (position-if #'break-point-p lineup :start (1+ from)))
	   (item (when idx (aref lineup idx)))
	   stop-idx start-idx)
      (etypecase item
	(glue (setq stop-idx idx start-idx (1+ idx)))
	(discretionary (setq stop-idx (1+ idx) start-idx idx))
	(null (setq stop-idx length)))
      (apply #'make-instance boundary-class
	     :item item :stop-idx stop-idx :start-idx start-idx
	     :lineup lineup keys))))
