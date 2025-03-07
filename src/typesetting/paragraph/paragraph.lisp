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

(defclass paragraph ()
  ((hlist :documentation "The paragraph's original hlist."
	  :initform nil :initarg :hlist
	  :reader hlist)
   (lineup :documentation "The paragraph's lineup."
	   :initform nil :initarg :lineup
	   :reader lineup)
   (breakup :documentation "The paragraph's breakup."
	    :initarg :breakup
	    :reader breakup))
  (:documentation "The PARAGRAPH class."))

(defun %make-paragraph (hlist lineup breakup)
  "Make a new paragraph out of HLIST, LINEUP, and BREAKUP."
  (make-instance 'paragraph :hlist hlist :lineup lineup :breakup breakup))


(defmethod disposition ((paragraph paragraph))
  "Return PARAGRAPH's disposition."
  (disposition (breakup paragraph)))

(defmethod width ((paragraph paragraph))
  "Return PARAGRAPH's width."
  (width (breakup paragraph)))

(defmethod properties strnlcat ((paragraph paragraph) &key layout-#)
  "Return a string advertising PARAGRAPH's lineup and breakup properties.
When LAYOUT-#, also advertise the paragraph breakup's LAYOUT-#th layout
properties."
  (strnlcat (properties (lineup paragraph))
	    (properties (breakup paragraph) :layout-# layout-#)))
