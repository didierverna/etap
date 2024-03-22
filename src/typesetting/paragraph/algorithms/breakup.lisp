(in-package :etap)

(defclass breakup ()
  ()
  (:documentation "The BREAKUP class.
This is the base class for breakups."))

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

(defun make-breakup
    (&key (context *context*)
	  (text (if context (text (nlstring context)) *text*))
	  (language (if context (language (nlstring context)) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (beds (when context (beds context)))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (width (if context (paragraph-width context) *paragraph-width*))
	  ;; #### WARNING: no mutual coherency checks for these three.
	  (hlist (%make-hlist text language font kerning ligatures hyphenation))
	  (lineup (%make-lineup hlist disposition algorithm)))
  "Make a new breakup.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed. Unless provided, HLIST, LINEUP, and BREAKUP are
subsequently computed."
  (%make-breakup lineup disposition width beds algorithm))


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
