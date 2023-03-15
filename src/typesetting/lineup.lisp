;; #### WARNING: there are a number of characteristics in the lineup that
;; affect the way algorithms are implemented. These specificities should be
;; kept in mind because should they change, said algorithms may become buggy.
;; In particular:
;; - the paragraph string is trimmed for spaces. There is a single glue
;;   between each words, and the KP algorithm adds an infinitely stretchable
;;   glue at the end of the paragraph.
;; - the only discretionaries that we have come from the hyphenation step, so
;;   they originally appear only in the middle of words. However, they may
;;   turn out to be anywhere after processing ligatures and kerning.

(in-package :etap)


;; =================
;; General Utilities
;; =================

;; For the interface.

(defparameter *lineup-features*
  '((:kerning t) (:ligatures t) (:hyphenation t))
  "The lineup features, as advertised by the interface.")


;; Lineup elements geometry.

(defgeneric width (object)
  (:documentation "Return OBJECT's width.")
  (:method ((clue (eql :hyphenation-clue)))
    "Return 0 (hyphenation clues don't eat horizontal space)."
    0)
  (:method ((clue (eql :explicit-hyphenation-clue)))
    "Return 0 (hyphenation clues don't eat horizontal space)."
    0)
  ;; #### NOTE: NIL in the lineup can occur in several situations, for
  ;; instance as the (empty) no-break or post-break of a hyphenation point.
  (:method ((null (eql nil)))
    "Return 0 (nothingness doesn't eat horizontal space)."
    0)
  (:method ((list list))
    "Return the sum of the widths of all elements in LIST."
    (reduce #'+ (mapcar #'width list)))
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metric's width."
    (tfm:width character)))

(defgeneric height (object)
  (:documentation "Return OBJECT's height.")
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metrics's height."
    (tfm:height character)))

(defgeneric depth (object)
  (:documentation "Return OBJECT's depth.")
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metrics's depth."
    (tfm:depth character)))



;; ===================
;; Lineup Constituents
;; ===================

;; -----
;; Kerns
;; -----

(defclass kern ()
  ((width :initarg :width :reader width :documentation "The kern's width."))
  (:documentation "The KERN class.
Kerns represent inter-letter horizontal spacing."))

(defun kernp (object)
  "Return T if OBJECT is a kern."
  (typep object 'kern))

(defun make-kern (width)
  "Make a new kern of WIDTH."
  (make-instance 'kern :width width))


;; ------------
;; Break points
;; ------------

;; #### FIXME: there is a confusion around the meaning of infinitely negative
;; penalties (perhaps in TeX as well). Infinitely negative penalties mean both
;; "force the break", and "prefer this one over the others when there are
;; several fit solutions to choose from". But these are two different things.
;; We could want to make a distinction, for example, declare that a break is
;; mandatory without altering the weight computation. In such a case, we could
;; end up choosing another (better) break and keep this one for later, when we
;; pass over it again.

;; Note that if we ever fix this, maybe the whole notion of infinite penalty
;; is going away.

(defgeneric penalty (item)
  (:documentation "Return ITEM's penalty.")
  ;; This methods applies to the last boundary (which has a null item), and
  ;; means to force the break there.
  (:method ((item null))
    "Return -∞."
    -∞))

(defclass break-point ()
  ((penalty :documentation "The penalty for breaking here."
	    :initform 0 :initarg :penalty :accessor penalty))
  (:documentation "The BREAK-POINT class.
This is the base class for all objects at which lines can be broken."))

(defun break-point-p (object)
  "Return T if OBJECT is a break point."
  (typep object 'break-point))


;; Discretionaries

(defclass discretionary (break-point)
  ((pre-break :initform nil :initarg :pre-break :accessor pre-break
	      :documentation "Contents to insert before the break.")
   (post-break :initform nil :initarg :post-break :accessor post-break
	       :documentation "Contents to insert after the break.")
   (no-break :initform nil :initarg :no-break :accessor no-break
	     :documentation "Contents to insert when we don't break."))
  (:documentation "The DISCRETIONARY class.
Discretionaries represent breakable positions with alternative contents,
depending on whether the break occurs or not."))

(defun discretionaryp (object)
  "Return T if OBJECT is a discretionary."
  (typep object 'discretionary))

;; #### NOTE: maybe one day we would need to process kerns and ligatures
;; within user-defined discretionaries' pre/post/no-break values!
(defun make-discretionary (&rest initargs &key pre-break post-break no-break)
  "Make a new discretionary out of PRE-BREAK, POST-BREAK, and NO-BREAK."
  (declare (ignore pre-break post-break no-break))
  (apply #'make-instance 'discretionary initargs))


;; Hyphenation points

;; #### NOTE: we use T as the default here because it allows simple calls to
;; MAKE-HYPHENATION-POINT without arguments (the pre, post, and no-breaks are
;; empty in that case).
(defclass hyphenation-mixin ()
  ((explicitp
    :initform t :initarg :explicit :reader explicitp
    :documentation
    "Whether this hyphenation point comes from an explicit hyphen."))
  (:documentation "The HYPHENATION-MIXIN class.
This is a mixin for hyphenation points."))

(defclass hyphenation-point (discretionary hyphenation-mixin)
  ()
  (:documentation "The HYPHENATION-POINT class.
This class represents hyphenation point discretionaries."))

(defun hyphenation-point-p (object)
  "Return T if OBJECT is a hyphenation point."
  (typep object 'hyphenation-point))

(defun make-hyphenation-point
    (&rest initargs &key pre-break post-break no-break explicit)
  "Make a new EXPLICITP hyphenation point."
  (declare (ignore pre-break post-break no-break explicit))
  (apply #'make-instance 'hyphenation-point initargs))


;; Glues

(defclass glue (break-point)
  ((width :initform 0 :initarg :width :reader width
	  :documentation "The glues's natural width.")
   (shrink :initform 0 :initarg :shrink :reader shrink
	   :documentation "The glue's shrinkability.")
   (stretch :initform 0 :initarg :stretch :reader stretch
	    :documentation "The glue's stretchability."))
  (:documentation "The GLUE class.
Glues represent breakable, elastic space."))

(defmethod initialize-instance :after ((glue glue) &key)
  "Validate GLUE's dimensions.
- WIDTH and SHRINK must be non-negative integers,
- STRETCH must be a non-negative integer, or +∞."
  (assert (and (rationalp (width glue)) (>= (width glue) 0)))
  (assert (and (rationalp (shrink glue)) (>= (shrink glue) 0)))
  (assert (or (and (rationalp (stretch glue)) (>= (stretch glue) 0))
	      (== (stretch glue) +∞))))

(defun gluep (object)
  "Return T if OBJECT is a glue."
  (typep object 'glue))

(defun make-glue (&rest keys &key width shrink stretch penalty)
  "Make a new glue out of WIDTH, SHRINK, STRETCH, and PENALTY."
  (declare (ignore width shrink stretch penalty))
  (apply #'make-instance 'glue keys))

(defun make-interword-glue (blank &aux (font (tfm:font blank)))
  "Make an interword glue, based on BLANK character's font specifications."
  (make-glue :width (tfm:interword-space font)
	     :shrink (tfm:interword-shrink font)
	     :stretch (tfm:interword-stretch font)))



;; ===============
;; Lineup Creation
;; ===============

;; #### WARNING: in the code below, the lineup is still a list.

;; #### NOTE: the procedures handling ligatures and kerning below are aware of
;; discretionaries, but they are really meant for those inserted automatically
;; during the hyphenation process, that is, \discretionary{-}{}{} in the
;; middle of a word. They may be useful in the future, for some more general
;; cases, but combining ligatures, kerning, and discretionaries is impossible
;; to do statically in general. For example, if we end up with different
;; potential ligatures starting from a post-break element and going to both a
;; no-break and a pre-break later on, we can't represent that statically. The
;; only truly general solution is to delay ligature and kerning processing
;; until the lineup is flattened. But then, this means that we also need to do
;; that every time we want to poll the size of various lineup chunks. This
;; could be rather expensive (although I haven't tried it).

;; -------
;; Kerning
;; -------

(defun kerning (elt1 elt2)
  "Return kerning information for lineup elements ELT1 and ELT2, or NIL."
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:kerning elt1 elt2)))

(defgeneric collect-kern (elt1 elt2 remainder)
  (:documentation
   "Collect kerning information for lineup (ELT1 ELT2 . REMAINDER).")
  (:method (elt1 elt2 remainder)
    "Return NIL. This is the default method."
    nil)
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
	    remainder &aux (kerning (tfm:kerning elt1 elt2)))
    "Return a kern for characters ELT1 and ELT2, or nil."
    (when kerning (make-kern kerning)))
  (:method ((elt1 tfm:character-metrics) (elt2 discretionary) remainder)
    "Add kerns to discretionary ELT2 if needed."
    (when (pre-break elt2)
      (let ((kerning (kerning elt1 (car (pre-break elt2)))))
	(when kerning (push (make-kern kerning) (pre-break elt2)))))
    (if (no-break elt2)
      (let ((kerning (kerning elt1 (car (no-break elt2)))))
	(when kerning (push (make-kern kerning) (no-break elt2))))
      (let ((kerning (kerning elt1 (car remainder))))
	(when kerning (setf (no-break elt2) (list (make-kern kerning))))))
    nil)
  (:method ((elt1 discretionary) (elt2 tfm:character-metrics) remainder)
    "Add kerns to discretionary ELT1 if needed."
    (when (no-break elt1)
      (let ((kerning (kerning (car (last (no-break elt1))) elt2)))
	(when kerning (endpush (make-kern kerning) (no-break elt1)))))
    (when (post-break elt1)
      (let ((kerning (kerning (car (last (post-break elt1))) elt2)))
	(when kerning (endpush (make-kern kerning) (post-break elt1)))))
    nil))

(defun process-kerning (lineup)
  "Return an new LINEUP with kerns."
  (loop :for elements :on lineup
	:for elt1 := (car elements)
	:for elt2 := (cadr elements)
	:for remainder := (cddr elements)
	:for kern := (collect-kern elt1 elt2 remainder)
	:collect elt1
	:when kern :collect kern))


;; --------------------
;; Ligatures processing
;; --------------------

(defun ligature (elt1 elt2)
  "Return a ligature for lineup ELT1 and ELT2, or NIL."
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:ligature elt1 elt2)))

(defgeneric next-characters-1 (elt remainder)
  (:documentation
   "Return a list of next characters for lineup (ELT . REMAINDER).
This function looks up in (ELT . REMAINDER) for all characters directly
accessible. There can be several of them, notably if ELT is a discretionary.")
  (:method (elt remainder)
    "Return NIL. This is the default method."
    nil)
  (:method ((elt tfm:character-metrics) remainder)
    "Return (ELT) since ELT is a character."
    (list elt))
  (:method ((elt discretionary) remainder)
    "Lookup next characters in discretionary ELT's pre-break and no-break."
    (append (next-characters (pre-break elt))
	    (next-characters (append (no-break elt) remainder)))))

(defun next-characters (lineup)
  "Return a list of next characters in LINEUP.
This function looks up in LINEUP for all characters directly accessible.
There can be several of them, notably if LINEUP begins with a discretionary."
  (when lineup (next-characters-1 (car lineup) (cdr lineup))))

;; #### NOTE: after processing ligatures, we may end up with adjacent
;; #### discretionaries, but this is normal. For example, in the word
;; #### ef-fi-cient, what we get eventually is
;; #### e\discretionary{f-}{fi}{ffi}\discretionary{-}{}{}cient.
(defgeneric process-ligatures-2 (elt1 elt2 remainder)
  (:documentation "Process ligatures for lineup (ELT1 ELT2 . REMAINDER).
Return a list of two values: a list of done elements that should be appended
to the new lineup, and the unprocessed new remainder.")
  (:method (elt1 elt2 remainder)
    "Return (ELT1) and (ELT2 . REMAINDER). This is the default method."
    (list (list elt1) (cons elt2 remainder)))
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
	    remainder
	    &aux (ligature (tfm:ligature elt1 elt2)) composition)
    "Process ligatures between ELT1 and ELT2 characters."
    (cond (ligature
	   (unless (tfm:delete-after ligature) (push elt2 composition))
	   (push (tfm:composite ligature) composition)
	   (unless (tfm:delete-before ligature) (push elt1 composition))
	   (list (subseq composition 0 (tfm:pass-over ligature))
		 ;; #### NOTE: because of the way TFM ligature programs work,
		 ;; we know that there's at least one thing left in this
		 ;; remainder. Indeed, the pass over cannot exceed the number
		 ;; of retained original characters.
		 (append (nthcdr (tfm:pass-over ligature) composition)
			 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder)))))
  (:method ((elt1 tfm:character-metrics) (elt2 discretionary) remainder
	    &aux (eat-elt1 (some
			    (lambda (character) (tfm:ligature elt1 character))
			    (next-characters (cons elt2 remainder)))))
    "Process ligatures between ELT1 character and ELT2 discretionary."
    (cond (eat-elt1
	   (setf (pre-break elt2)
		 (process-ligatures (cons elt1 (pre-break elt2)))
		 (no-break elt2)
		 (process-ligatures (cons elt1 (no-break elt2))))
	   (list nil (cons elt2 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder)))))
  (:method ((elt1 discretionary) (elt2 tfm:character-metrics) remainder
	    &aux (eat-elt2 (or (ligature (car (last (no-break elt1))) elt2)
			       (ligature (car (last (post-break elt1))) elt2))))
    "Process ligatures between ELT1 character and ELT2 discretionary."
    (cond (eat-elt2
	   (setf (no-break elt1)
		 (process-ligatures (append (no-break elt1) (list elt2)))
		 (post-break elt1)
		 (process-ligatures (append (post-break elt1) (list elt2))))
	   (list nil (cons elt1 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder))))))

(defun process-ligatures-1 (elt remainder)
  "Process ligatures for lineup ELT followed by REMAINDER.
Return a list of two values: a list of done elements that should be appended
to the new lineup, and the unprocessed new remainder."
  (if remainder
    (process-ligatures-2 elt (car remainder) (cdr remainder))
    (list (list elt))))

(defun process-ligatures (lineup)
  "Return a new LINEUP with ligatures."
  (loop :for elts := lineup :then remainder
	:for (done remainder) := (process-ligatures-1 (car elts) (cdr elts))
	:while elts
	:append done))


;; ---------------
;; Word processing
;; ---------------

(defun get-character (char font)
  "Get CHAR in FONT. Replace CHAR by a question mark if not found."
  ;; #### FIXME: no input encoding support yet.
  (or (tfm:get-character (char-code char) font)
      ;; #### WARNING: this one had better be available! Fall back to a null
      ;; character?
      (tfm:get-character (char-code #\?) font)))

(defun hyphen-positions+1 (word)
  "Return WORD (a string)'s explicit hyphen positions + 1."
  (loop :for i :from 1
	:for char :across word
	;; We don't want to collect a final hyphen's position, because if a
	;; word ends with one, there's not point in inserting a discretionary
	;; there. Either the word is followed by a glue, so we will be able to
	;; break, or it's followed by, e.g. punctuation, and we don't want to
	;; break there.
	:when (and (char= char #\-) (< i (length word))) :collect i))

(defun process-word-with-hyphenation (word font hyphenation-points hyphenator)
  "Process WORD (a string) in FONT with HYPHENATION-POINTS.
Return a list of characters from FONT, alternating with discretionaries at
HYPHENATION-POINTS. Use the function HYPHENATOR to create discretionaries."
  (loop :for i :from 0
	:for char :across word
	:for character := (get-character char font)
	:when (member i hyphenation-points) :collect (funcall hyphenator)
	  :collect character))

(defun process-word
    (word font hyphenation-rules &aux hyphenation-points)
  "Process WORD (a string) in FONT, possibly with HYPHENATION-RULES.
Return a list of characters from FONT, possibly alternating with
discretionaries if HYPHENATION-RULES is non-NIL."
  ;; Note that a word with explicit hyphens must not be hyphenated in any
  ;; other way.
  (cond ((and hyphenation-rules
	      (setq hyphenation-points (hyphen-positions+1 word)))
	 (process-word-with-hyphenation
	  word font hyphenation-points #'make-hyphenation-point))
	((and hyphenation-rules
	      (setq hyphenation-points (hyphenate word hyphenation-rules)))
	 (process-word-with-hyphenation
	  word font hyphenation-points
	  (let ((pre-break (list (get-character #\- font))))
	    (lambda ()
	      (make-hyphenation-point :pre-break pre-break :explicit nil)))))
	(t
	 (map 'list (lambda (char) (get-character char font)) word))))


;; --------------
;; Lineup slicing
;; --------------

(defparameter *blanks* '(#\Space #\Tab #\Newline)
  "The list of blank characters.")

(defun blankp (char)
  "Return T if CHAR is a blank character."
  (member char *blanks*))

(defun word-constituent-p (char)
  "Return T if CHAR is a word constituent.
Currently, this means alphabetic or a dash."
  (or (alpha-char-p char) (char= char #\-)))

;; #### NOTE: the hyphenation process below is simple, different from what TeX
;; does and should certainly be improved. For instance, TeX will consider only
;; one word between two glues, so for instance in "... foo.bar ...", bar will
;; never be hyphenated. There are also other rules that prevent hyphenation in
;; some situations, which we do not have right now.
(defun slice-text (text font hyphenation-rules)
  "Slice TEXT (a string) in FONT, possibly with HYPHENATION-RULES.
Return a list of characters from FONT, interword glues, and discretionaries if
HYPHENATION-RULES is non-NIL. STRING is initially trimmed from blanks, and
inner consecutive blanks are replaced with a single interword glue."
  (loop :with string := (string-trim *blanks* text)
	:with length := (length string)
	:with i := 0
	:while (< i length)
	:for char := (aref string i)
	:for character := (get-character char font)
	:if (blankp char)
	  :collect (make-interword-glue character)
	  ;; i cannot be NIL here because we've trimmed any end blanks.
	  :and :do (setq i (position-if-not #'blankp string :start i))
	:else :if (alpha-char-p char)
	  :append (process-word
		   (subseq string i
		     (position-if-not #'word-constituent-p string :start i))
		   font hyphenation-rules)
	  ;; this could happen here on the other hand.
	  :and :do (setq i (or (position-if-not #'word-constituent-p string
				 :start i)
			       length))
	:else
	  :collect character
	  :and :do (incf i)))


;; ------------------
;; Lineup computation
;; ------------------

(defun make-lineup
    (&key (context *context*)
	  (text (if context (text context) *text*))
	  (font (if context (font context) *font*))
	  (hyphenation-rules (if context (hyphenation-rules context)
				 *hyphenation-rules*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
     &aux (lineup (slice-text text font (when hyphenation hyphenation-rules))))
  "Make a new lineup.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, FONT, and HYPHENATION-RULES are defaulted from the
corresponding global variable, and KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES."
  ;; #### NOTE: the order is important below. Kerning must be computed after
  ;; ligature characters have been inserted, and the processing of ligatures
  ;; and kerning may affect the contents of discretionaries, so we must add
  ;; hyphenation clues only after everything else has been done.
  (when ligatures (setq lineup (process-ligatures lineup)))
  (when kerning (setq lineup (process-kerning lineup)))
  (when hyphenation
    (mapc (lambda (element)
	    (when (hyphenation-point-p element)
	      (push (if (explicitp element)
		      :explicit-hyphenation-clue
		      :hyphenation-clue)
		    (no-break element))))
      lineup))
  lineup)



;; ===================
;; Lineup Manipulation
;; ===================

;; #### WARNING: in the code below, the lineup has become an array.

;; ---------
;; Utilities
;; ---------

(defun lineup-aref (lineup i start stop &aux (element (aref lineup i)))
  "Return LINEUP element at position I, between START and STOP boundaries.
If element is a discretionary, return the appropriate pre/no/post break part."
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the lineup,
    ;; including ligatures / kerning management in the presence of hyphenation
    ;; points, we may end up with lineups beginning or ending with
    ;; discretionaries (or even consecutive discretionaries for that matter).
    ;; When discretionaries begin or end the lineup, we must not consider them
    ;; as post- or pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- stop)) (not (= stop (length lineup))))
	   (pre-break element))
	  (t (no-break element)))
    element))


;; -------------
;; Lineup widths
;; -------------

(defun lineup-width (lineup start stop)
  "Compute LINEUP's width between START and STOP.
Return five values: the natural, maximum, and minimum width, followed by the
stretch and shrink amounts."
  ;; #### FIXME: this is ugly. Do we still need this hack ?
  (unless stop (setq stop (length lineup)))
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- stop)
	;; #### FIXME: this works for now, but it is not quite right in the
	;; general case. When ELEMENT is a list (typically the contents of a
	;; discretionary, there could be anything inside, including, e.g.,
	;; glues. See also the long comment above the KERNING function.
	:for element := (lineup-aref lineup i start stop)
	:do (incf width (width element))
	:when (gluep element)
	  :do (setq stretch (++ stretch (stretch element))
		    shrink (+ shrink (shrink element)))
	:finally (return (values width (++ width stretch) (- width shrink)
				 stretch shrink))))

(defun lineup-max-width (lineup start stop)
  "Return LINEUP's width between START and STOP, with maximal stretching."
  (multiple-value-bind (natural max) (lineup-width lineup start stop)
    (declare (ignore natural))
    max))

(defun lineup-min-width (lineup start stop)
  "Return LINEUP's width between START and STOP, with maximal shrinking."
  (multiple-value-bind (natural max min) (lineup-width lineup start stop)
    (declare (ignore natural max))
    min))


;; -------------
;; Lineup scales
;; -------------

(defun scaling (width target stretch shrink)
  "Return the amount of scaling required to reach TARGET from WIDTH.
The amount in question is 0 if WIDTH is equal to TARGET.
Otherwise, it's a possibly infinite stretching (positive) or shrinking
(negative) ratio relative to the elasticity provided by STRETCH and SHRINK."
  (cond ((= width target) 0)
	((< width target) (// (- target width) stretch))
	((< target width) (// (- target width) shrink))))

(defun lineup-scale (lineup start stop target &optional extra)
  "Return the amount of scaling required for LINEUP chunk between START and
STOP to reach TARGET width, possibly with EXTRA stretch.
See `scaling' for more information."
  (multiple-value-bind (width max min stretch shrink)
      (lineup-width lineup start stop)
    (declare (ignore max min))
    (when extra (setq stretch (++ stretch extra)))
    (scaling width target stretch shrink)))


;; -----------------
;; Lineup boundaries
;; -----------------

(defclass boundary ()
  ((item
    :documentation "The lineup item at that boundary."
    :initarg :item :reader item)
   (stop-idx
    :documentation "The lineup index for an end of line at that boundary."
    :initarg :stop-idx :reader stop-idx)
   (start-idx
    :documentation
    "The lineup index for a beginning of line at that boundary."
    :initarg :start-idx :reader start-idx))
  (:default-initargs :allow-other-keys t) ;; allow :lineup
  (:documentation "Base class for lineup boundaries.
A boundary represents a possible break point in the lineup.
The end of the lineup is represented by a special boundary with a null item
and start index (the stop index being the lineup's length).

Algorithms may provide their own boundary sub-class."))

(defun last-boundary-p (boundary)
  "Return T if BOUNDARY is the last one."
  (null (item boundary)))

(defun next-boundary (lineup from &optional (boundary-class 'boundary)
				   &rest keys &key &allow-other-keys
				   &aux (length (length lineup)))
  "Return the next boundary in LINEUP FROM position, or NIL.
The returned object is an instance of BOUNDARY-CLASS (BOUNDARY by default).
This function understands the terminal case where FROM = LINEUP's
length (possibly coming from the end of lineup special boundary), in which
case it signals that there is no more boundary to find by returning NIL."
  (unless (= from length)
    (let* ((idx (position-if #'break-point-p lineup :start (1+ from)))
	   (item (when idx (aref lineup idx)))
	   stop-idx start-idx)
      (etypecase item
	(glue (setq stop-idx idx start-idx (1+ idx)))
	(discretionary (setq stop-idx (1+ idx) start-idx idx))
	(null (setq stop-idx length)))
      (apply #'make-instance boundary-class
	     :item item :stop-idx stop-idx :start-idx start-idx
	     :lineup lineup keys))))
