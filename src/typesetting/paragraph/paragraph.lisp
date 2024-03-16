(in-package :etap)


;; For the interface

(defparameter *paragraph-min-width* 142 ;; 142.26378pt = 5cm
  "The paragraph's minimum width in points.")

(defparameter *paragraph-max-width* 569 ;; 569.0551pt = 20cm
  "The paragraph's maximum width in points.")



;; ==========================================================================
;; Pinned lines
;; ==========================================================================

(defclass pinned-line (pinned)
  ((line :documentation "The corresponding line."
	 :initarg :line
	 :reader line))
  (:documentation "The PINNED-LINE class."))

(defmethod width ((line pinned-line))
  "Return pinned LINE's width."
  (width (line line)))

(defmethod height ((line pinned-line))
  "Return pinned LINE's height."
  (height (line line)))

(defmethod depth ((line pinned-line))
  "Return pinned LINE's depth."
  (depth (line line)))

(defmethod scale ((line pinned-line))
  "Return pinned LINE's scale."
  (scale (line line)))

(defmethod effective-scale ((line pinned-line))
  "Return pinned LINE's effective scale factor."
  (effective-scale (line line)))

(defmethod hyphenated ((line pinned-line))
  "Return pinned LINE's hyphenation status."
  (hyphenated (line line)))

(defmethod penalty ((line pinned-line))
  "Return pinned LINE's penalty."
  (penalty (line line)))

;; #### NOTE: we don't have having nesting feature right now, so no board for
;; pinned lines (toplevel objects).
(defun pin-line (line x y)
  "Pin LINE at position (X, Y)."
  (let ((pinned-line
	  (make-instance 'pinned-line :line line :x x :y y)))
    ;; #### FIXME: gross hack alert. Pinned objects have their line as the
    ;; board. But a line is not a pinned object, so it has no 2D coordinates,
    ;; and there is no back pointer from a line to a pinned line. For rivers
    ;; detection, I'm thus changing the beds boards to their pinned line for
    ;; now. Of course, this is completely broken.
    (mapc (lambda (object)
	    (when (bedp object)
	      (setf (slot-value object 'board) pinned-line)))
      (pinned-objects line))
    pinned-line))




;; ==========================================================================
;; Paragraphs
;; ==========================================================================

(defmethod initialize-instance :after
    ((paragraph paragraph)
     &key lines
     &aux (width (width paragraph))
	  (disposition (disposition-type (disposition paragraph))))
  "Pin LINES in PARAGRAPH."
  ;; #### TODO: this is gross but it works for now (we use a single font). 1.2
  ;; (expressed in ratio to avoid going all floats) is what TeX uses with the
  ;; Computer Modern fonts. But we should get the appropriate value somewhere
  ;; (it's up to the font designers, but it's not in the TFM format for
  ;; example).
  (let ((baselineskip
	  (if lines
	    (* 12/10
	       (tfm:design-size
		(tfm:font
		 (character-metrics
		  (find-if #'pinned-character-p
			   (pinned-objects (first lines)))))))
	    0)))
    (setf (slot-value paragraph 'pinned-lines)
	  (loop :for line :in lines
		:for x := (case disposition
			    ((:flush-left :justified) 0)
			    (:centered (/ (- width (width line)) 2))
			    (:flush-right (- width (width line))))
		;; #### TODO: nothing fancy about interline spacing yet.
		:for y := 0 :then (+ y baselineskip)
		:collect (pin-line line x y)))))

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
	  (hlist nil hlistp)
	  (lineup nil lineupp))
  "Make a new paragraph.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed."
  (unless lineup
    (when lineupp (warn "creating a lineup anyway (can't be null)."))
    (setq lineup
	  (%make-lineup
	   (if hlistp
	     hlist
	     (%make-hlist text language font kerning ligatures hyphenation))
	   disposition algorithm)))
  (apply #'typeset-lineup
    lineup disposition width beds (algorithm-type algorithm)
    (algorithm-options algorithm)))
