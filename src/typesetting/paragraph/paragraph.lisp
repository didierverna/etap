(in-package :etap)


;; For the interface

(defparameter *paragraph-min-width* 142 ;; 142.26378pt = 5cm
  "The paragraph's minimum width in points.")

(defparameter *paragraph-max-width* 569 ;; 569.0551pt = 20cm
  "The paragraph's maximum width in points.")




;; ==========================================================================
;; Paragraphs
;; ==========================================================================

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
	  ;; #### WARNING: no mutual coherency checks for these two.
	  (hlist (%make-hlist text language font kerning ligatures hyphenation))
	  (lineup (%make-lineup hlist disposition algorithm)))
  "Make a new paragraph.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed."
  (apply #'typeset-lineup
    hlist lineup disposition width beds (algorithm-type algorithm)
    (algorithm-options algorithm)))
