(in-package :etap)


;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *paragraph-min-width* 142 ;; 142.26378pt = 5cm
  "The paragraph's minimum width in points.")

;; #### NOTE: *paragraph-width* is defined in context.lisp

(defparameter *paragraph-max-width* 569 ;; 569.0551pt = 20cm
  "The paragraph's maximum width in points.")




;; ==========================================================================
;; Paragraphs
;; ==========================================================================

;; #### NOTE: contrary to other parameters that were used to generate the
;; paragraph (disposition, etc.), and even though it's not necessary for
;; drawing, we remember the width in the class below. That's because we
;; consider the paragraph's bounding box to be an inherent part of it, and we
;; need to remember the paragraph's width to get it. GUIs use other parameters
;; from the original context in order to draw various clues on top of the
;; actual paragraph's contents. The paragraph width is in fact also available
;; in the original context, but GUIs are still invited to use the slot below
;; instead, for consistency.
(defclass paragraph ()
  ((width :documentation "The paragraph's width."
	  :initarg :width :reader width)
   (hlist :documentation "The paragraph's original hlist."
	  :initform nil :initarg :hlist
	  :reader hlist)
   (lineup :documentation "The paragraph's lineup."
	   :initform nil :initarg :lineup
	   :reader lineup)
   (breakup :documentation "The paragraph's breakup."
	    :initarg :breakup
	    :reader breakup))
  (:documentation "The PARAGRAPH class."))

(defmethod pinned-lines ((paragraph paragraph))
  "Return PARAGRAPH's pinned lines."
  (pinned-lines (breakup paragraph)))

(defmethod height ((paragraph paragraph))
  "Return paragraph's height.
This is in fact the height of the first line (or 0), since we consider that
the paragraph's baseline is the first line's baseline. Not to be confused with
the height of the whole paragraph."
  (if (pinned-lines paragraph)
    (height (first (pinned-lines paragraph)))
    0))

(defmethod depth
    ((paragraph paragraph) &aux (last (car (last (pinned-lines paragraph)))))
  "Return paragraph's depth.
We consider that the paragraph's baseline is the first line's baseline."
  (if last (+ (y last) (depth last)) 0))

(defmethod break-points-# ((paragraph paragraph))
  "Return PARAGRAPH's number of break points."
  (break-points-# (lineup paragraph)))

(defmethod theoretical-solutions-# ((paragraph paragraph))
  "Return PARAGRAPH's number of theoretical break solutions."
  (theoretical-solutions-# (lineup paragraph)))

(defun paragraph-properties (paragraph)
  "Return a string advertising PARAGRAPH's properties.
Currently, these are the ones not visible on the GUI:
  - vertical dimensions,
  - number of break points,
  - number of theoretical solutions,
  - ... followed by breakup properties (see `breakup-properties'.)"
  (unless (zerop (length (hlist paragraph)))
    (strnlcat (format nil "Vertical size: ~Apt (height: ~Apt, depth: ~Apt).~@
			   ~A breakpoints, ~A theoretical solutions (2^n)."
		      (float (+ (height paragraph) (depth paragraph)))
		      (float (height paragraph))
		      (float (depth paragraph))
		      (break-points-# paragraph)
		      (theoretical-solutions-# paragraph))
	      (breakup-properties (breakup paragraph)))))

(defun make-paragraph
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
	  (lineup (%make-lineup hlist disposition algorithm))
	  (breakup (%make-breakup lineup disposition width beds algorithm)))
  "Make a new paragraph.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed. Unless provided, HLIST, LINEUP, and BREAKUP are
subsequently computed."
  (make-instance 'paragraph
    :width width :hlist hlist :lineup lineup :breakup breakup))
