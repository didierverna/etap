(in-package :etap)

;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *paragraph-min-width* 142 ;; 142.26378pt = 5cm
  "The paragraph's minimum width in points.")

(defvar *paragraph-width* 284 ;; 284.52756pt = 10cm
  "The default paragraph width.")

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

(defun %make-paragraph (width hlist lineup breakup)
  "Make a new paragraph of WIDTH, for HLIST, LINEUP, and BREAKUP."
  (make-instance 'paragraph
    :width width :hlist hlist :lineup lineup :breakup breakup))

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

(defmethod properties strnlcat ((paragraph paragraph))
  "Return a string advertising PARAGRAPH's properties.
Currently, these are the ones not visible on the GUI:
  - vertical dimensions,
  - number of break points,
  - number of theoretical solutions,
  - [breakup properties...]."
  (unless (zerop (length (hlist paragraph)))
    (strnlcat (format nil "Vertical size: ~Apt (height: ~Apt, depth: ~Apt).~@
			   ~A breakpoints, ~A theoretical solutions (2^n)."
		      (float (+ (height paragraph) (depth paragraph)))
		      (float (height paragraph))
		      (float (depth paragraph))
		      (break-points-# paragraph)
		      (theoretical-solutions-# paragraph))
	      (properties (breakup paragraph)))))
