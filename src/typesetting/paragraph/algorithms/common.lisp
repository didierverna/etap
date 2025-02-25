(in-package :etap)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun scaling (width target stretch shrink)
  "Return the amount of scaling required to reach TARGET from WIDTH.
The amount in question is 0 if WIDTH is equal to TARGET.
Otherwise, it's a possibly infinite stretching (positive) or shrinking
(negative) ratio relative to the elasticity provided by STRETCH and SHRINK."
  (cond ((= width target) 0)
	((< width target) ($/ (- target width) stretch))
	((< target width) ($/ (- target width) shrink))))

(defun actual-scales (scale &key (shrink-tolerance -1) (stretch-tolerance 1)
				 (overshrink nil) (overstretch nil))
  "Compute the actual scales for a line, based on required SCALE.
This function returns two values.
- The theoretical scale computed by the algorithm in use. This value depends
  on the algorithm s SHRINK / STRETCH-TOLERANCE (-1 / 1 by default).
- The effective scale, used to pin the line's items. This value further
  depends on the OVERSHRINK / OVERSTRETCH options (nil by default)."
  (let ((theoretical-scale scale) (effective-scale scale))
    (cond (($< scale 0)
	   (setq theoretical-scale ($max theoretical-scale shrink-tolerance))
	   (unless overshrink (setq effective-scale theoretical-scale)))
	  (($> scale 0)
	   (setq theoretical-scale ($min theoretical-scale stretch-tolerance))
	   (unless overstretch (setq effective-scale theoretical-scale))))
    (values theoretical-scale effective-scale)))


;; --------------------
;; Quality measurements
;; --------------------

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
;; HArrays
;; ==========================================================================

;; An harray can be as simple as an array version of the original hlist, but
;; contrary to the hlist, it depends on the final paragraph disposition and
;; typesetting algorithm in use. Most algorithms post-process the hlist (for
;; example by adjusting penalties or adding glues).

;; ------
;; Access
;; ------

(defun haref (harray i start stop &aux (element (aref harray i)))
  "Return HARRAY element at position I, between START and STOP boundaries.
If element is a discretionary, return the appropriate pre/no/post break part."
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the hlist, including
    ;; ligatures / kerning management in the presence of hyphenation points,
    ;; we may end up with harrays beginning or ending with discretionaries (or
    ;; even consecutive discretionaries for that matter). When discretionaries
    ;; begin or end the harray, we must not consider them as post- or
    ;; pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- stop)) (not (= stop (length harray))))
	   (pre-break element))
	  (t (no-break element)))
    element))

(defun flatten-harray (harray start stop)
  "Return a flattened list of HARRAY elements between START and STOP."
  (loop :for i :from start :upto (1- stop)
	:for elt := (haref harray i start stop)
	:if (consp elt) :append elt :else :collect elt))

;; #### FIXME: according to the glossary entry in the CLHS, I think it's
;; permissible for a pair of bounding index designators to be (<array's
;; length>, NIL). This means that we can safely do the 1+ below if BREAK-POINT
;; is the last harray item. However, Lisp implementations seem to diverge in
;; the handling of out-of-bounds errors (maybe not in that case), so this
;; should be checked and perhaps harmonized.
(defun next-break-point
    (harray &optional (break-point *bop*) &aux (start (1+ (idx break-point))))
  "Return the next break point in HARRAY after previous BREAK-POINT.
If BREAK-POINT is NIL (the default), search from the beginning of HARRAY.
If no further break point is found, return a special break point indicating
the end of the paragraph (an EOP instance).
If BREAK-POINT is an EOP one, return NIL."
  (unless (typep break-point 'eop)
    (or (find-if #'break-point-p harray :start start)
	(make-instance 'eop :idx (length harray)))))


;; --------
;; Geometry
;; --------

(defun harray-width (harray start stop)
  "Compute HARRAY's width between START and STOP.
Return five values: the natural, maximum, and minimum width, followed by the
stretch and shrink amounts."
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- stop)
	;; #### FIXME: this works for now, but it is not quite right in the
	;; general case. When ELEMENT is a list (typically the contents of a
	;; discretionary, there could be anything inside, including, e.g.,
	;; glues. See also the long comment above the KERNING function.
	:for element := (haref harray i start stop)
	:do (incf width (width element))
	:when (gluep element)
	  :do (setq stretch ($+ stretch (stretch element))
		    shrink (+ shrink (shrink element)))
	:finally (return (values width ($+ width stretch) (- width shrink)
				 stretch shrink))))

(defun harray-scale (harray start stop target &optional extra)
  "Return the amount of scaling required for HARRAY chunk between START and
STOP to reach TARGET width, possibly with EXTRA stretch.
See `scaling' for more information."
  (multiple-value-bind (width max min stretch shrink)
      (harray-width harray start stop)
    (declare (ignore max min))
    (when extra (setq stretch ($+ stretch extra)))
    (scaling width target stretch shrink)))

;; #### TODO: this is gross but it works for now (we use a single font). 1.2
;; (expressed in ratio to avoid going all floats) is what TeX uses with the
;; Computer Modern fonts. But we should get the appropriate value somewhere
;; (it's up to the font designers, but it's not in the TFM format for
;; example).
(defun baseline-skip (harray)
  "Return HARRAY's baseline skip."
  (* 12/10
     (tfm:design-size (tfm:font (find 'tfm:character-metrics harray
				  :key #'type-of)))))




;; ==========================================================================
;; Boundaries
;; ==========================================================================

(defclass boundary ()
  ((break-point :documentation "This boundary's break point."
		:initarg :break-point :reader break-point))
  (:documentation "Base class for boundaries.
A boundary represents the ending of a line at a certain break point.
Boundaries do not store the position of the beginning of the line. Algorithms
may provide their own subclass in order to store line properties, but those
properties should remain independent from layouts (the LINE hierarchy is here
for that)."))

(defmethod penalty ((boundary boundary))
  "Return BOUNDARY's break point penalty."
  (penalty (break-point boundary)))

(defmethod hyphenated ((boundary boundary))
  "Return BOUNDARY's break point hyphenation status."
  (hyphenation-point-p (break-point boundary)))

(defmethod eol-idx ((boundary boundary))
  "Return BOUNDARY's break-point end-of-line index."
  (eol-idx (break-point boundary)))

(defmethod eopp ((boundary boundary))
  "Return BOUNDARY's break point EOP status."
  (eopp (break-point boundary)))

(defmethod properties strnlcat ((boundary boundary) &key)
  "Return a string advertising BOUNDARY's penalty."
  (format nil "Penalty: ~A." (penalty boundary)))




;; ==========================================================================
;; Lines
;; ==========================================================================

;; -----------
;; Whitespaces
;; -----------

;; #### WARNING: do not confuse whitespaces (pinned glues) with blanks
;; (characters). In fact, newlines are considered blank characters, but they
;; do not produce whitespaces.

;; #### NOTE: glues are currently the only items that cannot be pinned
;; directly, hence the class below. The reason is that the width of a pinned
;; glue is different from the glue's width in general (it depends on the
;; line's scaling).
(defclass whitespace (pinned)
  ((width
    :documentation "The whitespace's width."
    :initarg :width :reader width))
  (:documentation "The WHITESPACE class.
This class represents pinned glues and stores their width after scaling."))

(defun whitespacep (item)
  "Return T if ITEM is a whitespace."
  (typep item 'whitespace))

(defun pin-glue (glue width board x)
  "Pin GLUE of (scaled) WIDTH on BOARD at (X, 0)."
  (make-instance 'whitespace :width width :object glue :board board :x x))


;; -----
;; Lines
;; -----

(defclass line ()
  ((harray
    :documentation "The corresponding harray."
    :initarg :harray :reader harray)
   (bol
    :documentation "This line's beginning break point."
    :initarg :bol :reader bol)
   (boundary
    :documentation "This line's boundary."
    :initarg :boundary :reader boundary)
   (scale
    :documentation "The line'scale, as computed by the algorithm.
It may be different from the boundary's theoretical scale, and from the
effective scale used to pin the items, depending on the algorithm itself,
and on the Overstretch and Overshrink disposition options)."
    :initform 0 :initarg :scale :reader scale)
   (effective-scale
    :documentation "The line's effective scale, used for pinning the items.
It may be different from the boundary's theoretical scale, and from scale
computed by the algorithm in use, depending on the algorithm itself, and on
the Overstretch and Overshrink disposition options)."
    :initarg :effective-scale :reader effective-scale)
   (items
    :documentation "The list of items in the line.
Currently, those are characters, whitespaces, and hyphenation clues.
These items are positioned relatively to the line's origin (which may be
different from the paragraph's origin."
    :reader items))
  (:documentation "The LINE class.
A line represents one step in a layout, that is, a particular path from the
beginning to the end of the paragraph. Algorithms may provide their own
subclass in order to store layout-dependent properties of the line, or
cumulative ones (layout properties up to that particular line). For
layout-independent properties, boundaries should be used instead."))

(defmethod initialize-instance :after ((line line) &key)
  "Initialize the LINE's effective scale if not already done."
  (unless (slot-boundp line 'effective-scale)
    (setf (slot-value line 'effective-scale) (scale line))))

(defun make-line
    (harray bol boundary &rest keys &key scale effective-scale)
  "Make an HARRAY line from BOL to BOUNDARY.
Optionally preset SCALE and EFFECTIVE-SCALE."
  (declare (ignore scale effective-scale))
  (apply #'make-instance 'line
	 :harray harray :bol bol :boundary boundary keys))


(defmethod bol-idx ((line line))
  "Return LINE's BOL index."
  (bol-idx (bol line)))

(Defmethod eol-idx ((line line))
  "Return LINE's EOL index."
  (eol-idx (boundary line)))


;; #### WARNING: the three methods below require the line to be rendered
;; already. Doing it otherwise is possible in theory (because we know the
;; scaling), but would require additional and redundant computation.

(defmethod width ((line line) &aux (item (car (last (items line)))))
  "Return LINE's width."
  (+ (x item) (width item)))

(defmethod height ((line line))
  "Return LINE's height."
  (loop :for item :in (items line) :maximize (height item)))

(defmethod depth ((line line))
  "Return LINE's depth."
  (loop :for item :in (items line) :maximize (depth item)))

(defmethod hyphenated ((line line))
  "Return LINE's hyphenation status."
  (hyphenated (boundary line)))

(defmethod penalty ((line line))
  "Return LINE's penalty."
  (penalty (boundary line)))


;; #### WARNING: this method requires the line to have been rendered already.
;; Doing it otherwise is possible in theory (because we know the scaling), but
;; would require additional and redundant computation.
(defmethod properties strnlcat ((line line) &key)
  "Return a string advertising LINE's properties.
This includes the line's boundary properties, plus "
  (strnlcat
   (properties (boundary line))
   (format nil "Scale: ~A~@[ (effective: ~A)~].~%Width: ~Apt."
     ($float (scale line))
     (when ($/= (scale line) (effective-scale line))
       ($float (effective-scale line)))
     (float (width line)))))


;; ---------
;; Rendering
;; ---------

(defun render-line (line &aux (scale (effective-scale line)))
  "Pin LINE's items and return LINE."
  ;; #### NOTE: infinite scaling means that we do not have any elasticity.
  ;; Leaving things as they are, we would end up doing (* +/-∞ 0) below, which
  ;; is not good. However, the intended value of (* +/-∞ 0) is 0 here (again,
  ;; no elasticity) so we can get the same behavior by resetting SCALE to 0.
  (unless (numberp scale) (setq scale 0))
  (setf (slot-value line 'items)
	(loop :with x := 0 :with w
	      :with harray := (harray line)
	      :for object
		:in (flatten-harray harray (bol-idx line) (eol-idx line))
	      :if (member
		   object '(:explicit-hyphenation-clue :hyphenation-clue))
		:collect (pin-object object line x)
	      :else :if (typep object 'tfm:character-metrics)
		:collect (pin-object object line x)
		:and :do (incf x (width object))
	      :else :if (kernp object)
		:do (incf x (width object))
	      :else :if (gluep object)
		:do (setq w (width object))
		:and :unless (zerop scale)
		  :do (incf w (if (> scale 0)
				  (* scale (stretch object))
				  (* scale (shrink object))))
		  :end
		:and :collect (pin-glue object w line x)
		:and :do (incf x w)))
  line)




;; ==========================================================================
;; Layouts
;; ==========================================================================

(defclass layout ()
  ((breakup
    :documentation "The breakup this layout belongs to."
    :initarg :breakup :reader breakup)
   (pinned-line-class
    :documentation "The class to use when pinning lines."
    :allocation :class :initform 'pinned-line :reader pinned-line-class)
   (lines
    :documentation "This layout's list of lines."
    :initform nil :initarg :lines :reader lines))
  (:documentation "The LAYOUT class.
A layout represents one specific path from the beginning to the end of the
paragraph. Algorithms may provide their own layout subclass in order to store
specific global properties."))


;; ---------
;; Rendering
;; ---------

(defclass pinned-line (line pin)
  ()
  (:documentation "The LINNED-LINE class."))

(defun render-layout (layout &aux (lines (lines layout)))
  "Render LAYOUT's lines and pin them. Return LAYOUT."
  (when lines
    (with-slots (harray disposition width) (breakup layout)
      (loop :with class := (pinned-line-class layout)
	    :with baseline-skip := (baseline-skip harray)
	    :with x := (ecase (disposition-type disposition)
			 ((:flush-left :justified)
			  (lambda (line) (declare (ignore line)) 0))
			 (:centered
			  (lambda (line) (/ (- width (width line)) 2)))
			 (:flush-right
			  (lambda (line) (- width (width line)))))
	    :for y := 0 :then (+ y baseline-skip)
	    :for line :in lines
	    :do (render-line line)
	    :do (change-class line class
		  :board layout :x (funcall x line) :y y))))
  layout)

(defun renderedp (layout)
  "Return T if LAYOUT is rendered."
  (typep (first (lines layout)) (pinned-line-class layout)))


(defun lines-# (layout)
  "Return the number of PINNED-LINES."
  (length (lines layout)))


;; #### WARNING: the three methods below can only work when the layout has
;; been rendered.

;; #### NOTE: there's no WIDTH method for layouts because layout lines may be
;; of different widths. The width of a line is /not/ the width of the
;; paragraph. It's the total physical width occupied by its items.

(defmethod height ((layout layout))
  "Return LAYOUT' height.
This is in fact the height of the first line, since we consider that the
paragraph's baseline is the first line's baseline. Not to be confused with the
height of the whole paragraph."
  (height (first (lines layout))))

(defmethod depth ((layout layout) &aux (last (car (last (lines layout)))))
  "Return LAYOUT's depth.
We consider that the paragraph's baseline is the first line's baseline."
  (+ (y last) (depth last)))


(defmethod properties strnlcat ((layout layout) &key)
  "Return a string advertising LAYOUT's properties.
This includes the layout's number of lines, vertical size, height, and depth."
  (format nil "~A line~:P.~@
	       Vertical size: ~Apt (height: ~Apt, depth: ~Apt)."
    (lines-# layout)
    (float (+ (height layout) (depth layout)))
    (float (height layout))
    (float (depth layout))))




;; ==========================================================================
;; Algorithm Specification
;; ==========================================================================

(defun algorithm-type (algorithm)
  "Return ALGORITHM type.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (car-or-symbol algorithm))

(defun algorithm-options (algorithm)
  "Return ALGORITHM options.
ALGORITHM is either a symbol, or a list of the form (NAME OPTIONS...)."
  (cdr-or-nil algorithm))


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
