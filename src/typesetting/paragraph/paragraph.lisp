(in-package :etap)


;; For the interface

(defparameter *paragraph-min-width* 142 ;; 142.26378pt = 5cm
  "The paragraph's minimum width in points.")

(defparameter *paragraph-max-width* 569 ;; 569.0551pt = 20cm
  "The paragraph's maximum width in points.")



;; ============
;; Pinned lines
;; ============

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
  "Whether pinned LINE is hyphenated.
Possible values are nil, :explicit, or :implicit."
  (hyphenated (line line)))

(defmethod penalty ((line pinned-line))
  "Return pinned LINE's penalty."
  (penalty (line line)))

(defun pin-line (line board x y)
  "Pin LINE on BOARD at position (X, Y)."
  (let ((pinned-line
	  (make-instance 'pinned-line :line line :board board :x x :y y)))
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



;; ==========
;; Paragraphs
;; ==========

(defmethod initialize-instance :before ((paragraph paragraph) &key lineup)
  "Compute and store the number of break points and theoretical solutions."
  (when lineup
    (let ((break-points-number
	    (count-if (lambda (item)
			(and (break-point-p item) ($< (penalty item) +∞)))
		lineup)))
      (setf (slot-value paragraph 'break-points-number)
	    break-points-number
	    (slot-value paragraph 'theoretical-solutions-number)
	    (expt 2 break-points-number)))))

(defmethod initialize-instance :after
    ((paragraph paragraph)
     &key lines
     &aux (width (width paragraph))
	  (disposition (disposition-type (disposition paragraph)))
	  ;; #### TODO: this is gross but it works for now (we use a single
	  ;; font). 1.2 (expressed in ratio to avoid going all floats) is what
	  ;; TeX uses with the Computer Modern fonts. But we should get the
	  ;; appropriate value somewhere (it's up to the font designers, but
	  ;; it's not in the TFM format for example).
	  (baselineskip
	   (if lines
	       (* 12/10
		  (tfm:design-size
		   (tfm:font
		    (character-metrics
		     (find-if #'pinned-character-p
			      (pinned-objects (first lines)))))))
	       0)))
  "Pin LINES in PARAGRAPH."
  (setf (slot-value paragraph 'pinned-lines)
	(loop :for line :in lines
	      :for x := (case disposition
			  ((:flush-left :justified) 0)
			  (:centered (/ (- width (width line)) 2))
			  (:flush-right (- width (width line))))
	      ;; #### TODO: nothing fancy about interline spacing yet.
	      :for y := 0 :then (+ y baselineskip)
	      :collect (pin-line line paragraph x y))))

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
    (&rest keys
     &key (context *context*)
	  (text *text*)
	  (language *language*)
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (beds (when context (beds context)))
	  (lineup nil lineupp)
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (width (if context (paragraph-width context) 284)))
  "Make a new paragraph.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, and FONT are defaulted from the corresponding
global variables, KERNING, LIGATURES, and HYPHENATION are defaulted from
FEATURES, DISPOSITION is defaulted to :flush-left, ALGORITHM to :fixed, and
WIDTH to 284pt."
  (declare (ignore text language font kerning ligatures hyphenation))
  (unless lineupp
    (setq lineup
	  (apply #'make-lineup
	    (select-keys keys :context :text :language :font
			      :features :kerning :ligatures :hyphenation))))
  (setq lineup (apply #'prepare-lineup
		 lineup disposition (algorithm-type algorithm)
		 (algorithm-options algorithm)))
  (apply #'typeset-lineup
    lineup disposition width beds (algorithm-type algorithm)
    (algorithm-options algorithm)))
