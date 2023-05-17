(in-package :etap)

;; =========================
;; Parametrization Utilities
;; =========================

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
     &aux (variable (intern (format nil "~A" name)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate NAMEd variable according to the *PREFIX-NAME* caliber.
If the variable's value is out of bounds, either clamp it (the default),
or use INFINITY values."
  `(cond ((null ,variable)
	  (setq ,variable (caliber-default ,caliber)))
	 ((<= ,variable (caliber-min ,caliber))
	  (setq ,variable ,(if infinity -∞ `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if infinity +∞ `(caliber-max ,caliber))))))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "~A" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If NAMEd variable is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))



;; ====================
;; Quality measurements
;; ====================

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
  (if (or (<< scale -1) (== scale +∞))
    +∞
    (* 100 (expt (abs scale) 3))))

;; #### FIXME: this should be called LINEUP-BADNESS, and maybe this should
;; even be obsolescent, like most lineup- functions.
(defun badness (lineup start stop width &optional emergency-stretch)
  (scale-badness (lineup-scale lineup start stop width emergency-stretch)))



;; ===========
;; Entry Point
;; ===========

(define-method-combination make-lines ()
  ((before-around (:before-around) :order :most-specific-last)
   (around (:around))
   (after-around (:after-around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  "The MAKE-LINES method combination.
Similar to the standard one, with the addition of two new method groups,
:before-around and :after-around, which are sorted in most specific last /
first respectively, and which require explicit chaining."
  (flet ((call-methods (methods)
	   (mapcar #'(lambda (method)
		       `(call-method ,method))
	     methods)))
    (let ((form (if (or before after (rest primary))
		  `(multiple-value-prog1
		       (progn ,@(call-methods before)
			      (call-method ,(first primary)
					   ,(rest primary)))
		     ,@(call-methods (reverse after)))
		  `(call-method ,(first primary))))
	  (around (append before-around around after-around)))
      (if around
	`(call-method ,(first around)
		      (,@(rest around)
		       (make-method ,form)))
	form))))

(defgeneric make-lines
    (lineup disposition width algorithm &key &allow-other-keys)
  (:method-combination make-lines)
  (:documentation
   "Typeset LINEUP as a DISPOSITION paragraph of WIDTH with ALGORITHM.")
  (:method :before-around (lineup disposition width algorithm &key)
    "Proceed only if LINEUP is not null."
    (when lineup (call-next-method)))
  (:method :after-around (lineup disposition width algorithm &rest args)
    "Transform LINEUP into an array."
    (setq lineup (make-array (length lineup) :initial-contents lineup))
    (apply #'call-next-method lineup disposition width algorithm args)))
