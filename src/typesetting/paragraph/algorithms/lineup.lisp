;; A `lineup' is an object representing the paragraph material ready for
;; typesetting. The actual material is stored into a so-called `harray' (the
;; equivalent of an hlist, in array form).

;; In addition to the harray, the lineup also stores the computation of the
;; potential break points and theoretical solutions numbers. We associate
;; those numbers with the lineup's harray rather than with the original hlist
;; in order to let algorithms prepare the harray in any way they see fit (this
;; might affect the number of break points). Note that the computed number of
;; break-points is theoretical. In particular it doesn't reflect dynamically
;; adjusted penalties.

(in-package :etap)
(in-readtable :etap)


;; ==========================================================================
;; Algorithm HList Processing
;; ==========================================================================

;; #### WARNING: the DISPOSITION argument is currently unused, but will be
;; when we update the KP algorithm to handle ragged dispositions properly.

;; #### NOTE: this function is called with all the algorithm options, without
;; knowing in advance whether they're going to be used or not, so we need to
;; relax keyword argument checking.

(defgeneric process-hlist
    (hlist disposition algorithm &key &allow-other-keys)
  (:documentation
   "Process HLIST for DISPOSITION in an ALGORITHM-specific way.
All primary methods must return a (possibly modified) HLIST.")
  (:method (hlist disposition algorithm &key)
    "Return a new glued HLIST. This is the default method."
    (glue-hlist hlist)))




;; ==========================================================================
;; Lineups
;; ==========================================================================

;; #### NOTE: contrary to breakups, there is currently no need for
;; algorithm-specific lineup subclasses, even though it would make the
;; specification (and the generic dispatch) of BREAK-LINEUP much cleaner.

(defclass lineup ()
  ((buffer
    :documentation "The lineup's buffer."
    :initarg :buffer :reader buffer)
   (language
    :documentation "The lineup's default language."
    :initarg :language :reader language)
   (font
    :documentation "The lineup's default font."
    :initarg :font :reader font)
   (features
    :documentation "The lineup's features."
    :initarg :features :reader features)
   (disposition
    :documentation "The lineup's disposition."
    :initarg :disposition :reader disposition)
   (algorithm
    :documentation "The lineup's algorithm."
    :initarg :algorithm :reader algorithm)
   (harray
    :documentation "The lineup's harray."
    :reader harray)
   (break-points-#
    :documentation "The number of theoretical break points."
    :initform 0 :reader break-points-#)
   (theoretical-solutions-#
    :documentation "The number of theoretical solutions (2^n)."
    :initform 0 :reader theoretical-solutions-#))
  (:documentation "The LINEUP class."))

(defmethod initialize-instance :after
    ((lineup lineup)
     &key
     &aux (features (features lineup))
	  (algorithm (algorithm lineup)))
  "Finalize LINEUP.
This currently involves:
- creating the harray and initializing the break point indexes,
- computing the total number of (usable) break points, and theoretical
  solutions."
  (let ((*language* (language lineup))
	(*font* (font lineup))
	(*kerning* (getf features :kerning))
	(*ligaturing* (getf features :ligatures))
	(*hyphenation* (getf features :hyphenation)))
    (load-buffer (buffer lineup))
    ;; #### NOTE: I'm doing this here instead of in MAKE-HLIST in prevision
    ;; for this function being called recursively.
    (when (eq (first *hlist*) :blank) (setq *hlist* (rest *hlist*)))
    (when (eq (first (last *hlist*)) :blank) (setq *hlist* (nbutlast *hlist*)))
    (when *hlist*
      ;; #### NOTE: the order is important: Hyphenation, ligaturing, and
      ;; finally kerning.
      (when *hyphenation* (setq *hlist* (hyphenate-hlist *hlist*)))
      (when *ligaturing* (setq *hlist* (ligature-hlist *hlist*)))
      (when *kerning* (setq *hlist* (kern-hlist *hlist*)))
      (setq *hlist* (apply #'process-hlist *hlist*
			   (disposition lineup)
			   (algorithm-type algorithm)
			   (algorithm-options algorithm)))))
  (with-slots (harray) lineup
    (setq harray (make-array (length *hlist*) :initial-contents *hlist*))
    ;; #### NOTE: for clarity, we want to make a distinction between and empty
    ;; paragraph and a non-empty one with no break points. In the former case,
    ;; we state that we have 0 solutions, while in the later case we have one.
    (unless (zerop (length harray))
      (loop :with break-points-# := 0
	    :for item :across harray
	    :for i :from 0
	    :when (break-point-p item)
	      :do (setf (slot-value item 'idx) i)
	      :and :do (incf break-points-#)
	    :finally (setf (slot-value lineup 'break-points-#)
			   break-points-#
			   (slot-value lineup 'theoretical-solutions-#)
			   (expt 2 break-points-#))))))

(defmethod properties strnlcat ((lineup lineup) &key)
  "Return a string advertising LINEUP's properties.
This includes the total number of break points, and the theoretical number of
breaking solutions."
  (format nil "~A breakpoints, ~A theoretical solutions (2^n)."
    (break-points-# lineup)
    (theoretical-solutions-# lineup)))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun %make-lineup (buffer language font features disposition algorithm)
  "Make a new lineup. See `context' for an explanation of the arguments."
  (make-instance 'lineup
    :buffer buffer :language language :font font
    :features features :disposition disposition :algorithm algorithm))
