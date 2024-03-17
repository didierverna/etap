(in-package :etap)

;; ==========================================================================
;; Parametrization Utilities
;; ==========================================================================

;; For easy exchange with the interface and manipulation as function keyword
;; parameters.

(defun algorithm-type (algorithm)
  "Return ALGORITHM type.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (car-or-symbol algorithm))

(defun algorithm-options (algorithm)
  "Return ALGORITHM options.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (cdr-or-nil algorithm))


(defstruct (caliber (:constructor make-caliber (min default max)))
  "The CALIBER structure.
A caliber represents values that have a mininum, a maximum, and a default."
  min default max)

(defmacro define-caliber (prefix name min default max)
  "Define a *PREFIX-NAME* caliber with MIN, DEFAULT, and MAX values."
  `(defparameter ,(intern (format nil "*~A-~A*" prefix name))
     (make-caliber ,min ,default, max)))

(defmacro calibrate
    (prefix name
     &optional infinity
     &aux (variable (intern (format nil "*~A*" name)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate *NAME* according to the *PREFIX-NAME* caliber.
- If *NAME* is null, set it to the caliber's default.
- If *NAME* is already properly calibrated, leave it be.
- If *NAME* is out of bounds (large inequality), clamp it or set it to an
  infinity value of the same sign, according to INFINITY. INFINITY may be NIL
  (the default), T, :positive, or :negative."
  `(cond ((null ,variable)
	  (setq ,variable (caliber-default ,caliber)))
	 ((<= ,variable (caliber-min ,caliber))
	  (setq ,variable ,(if (member infinity '(t :negative))
			     -∞
			     `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if (member infinity '(t :positive))
			     +∞
			     `(caliber-max ,caliber))))))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "*~A*" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If *NAME* is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))


(defmacro define-global-variables (&rest names)
  "Define global variables for all NAMES. Earmuffs are added to all NAMES."
  `(progn
     ,@(mapcar (lambda (name) (list 'defvar (intern (format nil "*~A*" name))))
	 names)))




;; ==========================================================================
;; Quality measurements
;; ==========================================================================

;; #### NOTE: the two functions below normally belong to the Knuth-Plass
;; algorithm. They're here because the Best-Fit / Justified one uses them as
;; well.

;; #### NOTE: according to #108, TeX clamps badness values to 10000 which is
;; an approximation of 2^13, and called "infinitely bad", but there's in fact
;; more to it than that when the badness function is used in the paragraph
;; breaking algorithm.

;; According to #853 (and this is explained in #851), an "infinitely bad" + 1
;; value is returned for lines which can't shrink enough, knowing that it is
;; strictly prohibited to shrink more than what's available; that is, for a
;; scaling < -1. Note that this includes lines which have no shrinkability at
;; all.

;; According to #852, an "infinitely bad" value is returned for the whole
;; bunch of lines which would require too much stretching (because of the
;; clamping), but this also includes lines which have no stretchability at
;; all.

;; In other words, for reasonable lines, the badness doesn't make a
;; distinction between shrinking and stretching. However, there is a
;; distinction for unreasonable lines. First of all, the definition of
;; "unreasonable" is different for stretching and shrinking (see the 2
;; paragraphs above). And then, the badness is different for unreasonably
;; shrunk and unreasonably stretched lines.

;; In fact, it seems that TeX uses this distinction only to decide whether or
;; not to deactivate a node (again, as explained in #851). On the other hand,
;; in our implementation, we look at the scaling instead of the badness to
;; make that decision. So it turns out that we can get rid of the clamping
;; altogether. Note also that the tolerance calibration in our implementation
;; turns 10000 into +∞, so we 'll indeed get the same effect as in TeX (cf.
;; #828), that is, to accept arbitrarily bad lines.

;; Consequently, our version of badness below returns +∞ for strictly
;; prohibited scaling (i.e. no scaling available, or negative below -1) and a
;; numerical positive value otherwise.

;; #### TODO: we could generalize the notion of badness to allow /some/
;; overshrinking, possibly with an exponential cost (at least it should be
;; much more important than for stretching). Maybe one difficulty would be
;; that if we want to switch to +∞ when there is no more space between words,
;; the badness computation would then depend on the glue's natural width.

;; #### TODO: we could sign the badness (like the scaling) in order to keep
;; track of whether we're stretching or shrinking.

(defun scale-badness (scale)
  (if (or ($< scale -1) ($= scale +∞))
    +∞
    (* 100 (expt (abs scale) 3))))

(defun local-demerits (badness penalty line-penalty)
  "Return a line's local demerits.
Local demerits do not account for contextual information such as hyphens
adjacency or fitness class difference (that's what they are called \"local\").
They are computed from the line scale's BADNESS, a possible PENALTY where the
line ends, and also include the LINE-PENALTY parameter."
  (cond ((and (numberp penalty) (<= 0 penalty))
	 ($+ ($^ ($+ line-penalty badness) 2) (expt penalty 2)))
	((and (numberp penalty) (< penalty 0))
	 ($+ ($^ ($+ line-penalty badness) 2) (- (expt penalty 2))))
	(t ;; -∞
	 ($^ ($+ line-penalty badness) 2))))




;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun actual-scales (scale &key (shrink-tolerance -1) (stretch-tolerance 1)
				 (overshrink nil) (overstretch nil))
  "Compute the actual scales for a line, based on required SCALE.
This function returns two values.
- The theoretical scale computed by the algorithm in use. This value depends
  on the algorithm s SHRINK / STRETCH-TOLERANCE (-1 / 1 by default).
- The effective scale, used to pin the line's objects. This value further
  depends on the OVERSHRINK / OVERSTRETCH options (nil by default)."
  (let ((theoretical-scale scale) (effective-scale scale))
    (cond (($< scale 0)
	   (setq theoretical-scale ($max theoretical-scale shrink-tolerance))
	   (unless overshrink (setq effective-scale theoretical-scale)))
	  (($> scale 0)
	   (setq theoretical-scale ($min theoretical-scale stretch-tolerance))
	   (unless overstretch (setq effective-scale theoretical-scale))))
    (values theoretical-scale effective-scale)))




;; ==========================================================================
;; Entry Points
;; ==========================================================================

(defgeneric break-harray
    (harray disposition width beds algorithm &key &allow-other-keys)
  (:documentation
   "Break HARRAY as a DISPOSITION paragraph of WIDTH with ALGORITHM.
Also maybe include river BEDS."))

;; #### FIXME: the design of this is not satisfactory. Every method calls
;; MAKE-PARAGRAPH with its own paragraph class, but also has to pass a number
;; of identical keywords (such as :disposition or :lineup), hence, there is
;; some duplication. Paragraph making and lineup typesetting should probably
;; be separated.
(defgeneric typeset-lineup
    (lineup disposition width beds algorithm &key &allow-other-keys)
  (:documentation
   "Typeset LINEUP as a DISPOSITION paragraph of WIDTH with ALGORITHM.
Also maybe include river BEDS."))
