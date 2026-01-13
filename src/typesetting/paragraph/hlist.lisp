;; Borrowing terminology from TeX, an `hlist' (Horizontal List) is the result
;; of slicing (hashing? but that's not the meaning of the h in hlist ;-)) the
;; input text into a list of items called `helts' (Horizontal Elements).
;; Depending on the requested lineup features and algorithmic options, the
;; possible helts are `fchars' (Font Characters, possibly including
;; ligatures), kerns, glues, and discretionaries (in particular for
;; hyphenation purposes).

;; HLists remain independent from the paragraph width but not from the
;; typesetting algorithm. Algorithms are passed a "raw" hlist in which blank
;; space is represented by a :BLANK keyword. It is each algorithm's
;; responsibility to perform glueing (because it might be done in different
;; ways depending on the algorithm, or the paragraph's disposition). See the
;; function `process-hlist' for more information. Hyphenation, ligaturing, and
;; kerning can be done independently from the algorithm however, and is
;; performed afterwards during lineup initialization.

;; HLists are lists because it makes it easier for algorithms to modified them
;; during their pre-processing stage. Afterwards, an hlist is transformed into
;; an array (called the `harray') which is stored in a lineup object.

(in-package :etap)
(in-readtable :etap)


;; ==========================================================================
;; H-Elements
;; ==========================================================================

;; ---------------
;; Font characters
;; --------------

(defun get-fchar (char &optional (font *font*))
  "Get CHARacter in FONT (*FONT* by default).
Replace CHAR by a question mark if not found."
  ;; #### TODO: no input encoding support yet.
  (or (tfm:get-character (char-code char) font)
      ;; #### FIXME: this one had better be available! Fall back to a null
      ;; character?
      (tfm:get-character (char-code #\?) font)))



;; -----
;; Kerns
;; -----

(defclass kern ()
  ((width :documentation "The kern's width." :initarg :width :reader width))
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

;; #### TODO: there is a confusion around the meaning of infinitely negative
;; penalties (perhaps in TeX as well). Infinitely negative penalties mean both
;; "force the break", and "prefer this one over the others when there are
;; several fit solutions to choose from". But these are two different things.
;; We could want to make a distinction, for example, declare that a break is
;; mandatory without altering the weight computation. In such a case, we could
;; end up choosing another (better) break and keep this one for later, when we
;; pass over it again.

;; Note that if we ever fix this, maybe the whole notion of infinite penalty
;; is going away.

(defabstract break-point ()
  ((idx
    :documentation "This break point's harray index."
    :reader idx))
  (:documentation "The BREAK-POINT abstract class.
This is the base class for all objects at which lines can be broken."))

(defun break-point-p (object)
  "Return T if OBJECT is a break point."
  (typep object 'break-point))

(defgeneric bol-idx (break-point)
  (:documentation
   "Return the harray index for a beginning of line at that break point.")
  (:method (break-point)
    "Return BREAK-POINT's IDX + 1. This is the default method."
    (1+ (idx break-point))))

(defgeneric eol-idx (break-point)
  (:documentation
   "Return the harray index for an end of line at that break point.")
  (:method (break-point)
    "Return BREAK-POINT's IDX. This is the default method."
    (idx break-point)))



;; Penalty mixin

(defgeneric penalty (helt)
  (:documentation "Return HELT's penalty.")
  ;; This methods applies to the last boundary (which has a null helt), and
  ;; means to force the break there.
  (:method ((helt null))
    "Return -∞."
    -∞))

(defabstract penalty-mixin ()
  ((penalty
    :documentation "The penalty associated with that break point."
    :initform 0 :initarg :penalty :accessor penalty)
   (caliber
    :documentation "The penalty's corresponding caliber."
    :initform nil :initarg :caliber :reader caliber))
  (:documentation "The PENALTY-MIXIN class.
This class is a mixin for weighted break point classes."))

(defmethod properties strnlcat ((break-point penalty-mixin) &key)
  "Advertise weighted BREAK-POINT's penalty."
  (format nil "Penalty: ~A." (penalty break-point)))



;; Discretionaries

(defclass discretionary (break-point)
  ((pre-break
    :documentation "Contents to insert before the break."
    :initform nil :initarg :pre-break :accessor pre-break)
   (post-break
    :documentation "Contents to insert after the break."
    :initform nil :initarg :post-break :accessor post-break)
   (no-break
    :documentation "Contents to insert when we don't break."
    :initform nil :initarg :no-break :accessor no-break))
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

;; #### NOTE: there's a method for hard glues (see below), but we have nothing
;; to advertise for hard discretionaries. Since we still need at least one
;; applicable method, here's one that does nothing. Maybe one day we'll want
;; to show something for non-hyphenation discretionaries...
(defmethod properties strnlcat ((break-point discretionary) &key)
  "Advertise nothing."
  nil)

(defmethod bol-idx ((discretionary discretionary))
  "Return DISCRETIONARY's IDX."
  (idx discretionary))

(defmethod eol-idx ((discretionary discretionary))
  "Return DISCRETIONARY's IDX + 1."
  (1+ (idx discretionary)))


(defclass soft-discretionary (discretionary penalty-mixin)
  ()
  (:documentation "The SOFT-DISCRETIONARY class.
This class represents weighted discretionaries."))



;; Hyphenation points

;; #### NOTE: we use T as the default here because it allows simple calls to
;; MAKE-HYPHENATION-POINT without arguments (the pre, post, and no-breaks are
;; empty in that case).
(defabstract hyphenation-mixin ()
  ((explicitp
    :documentation
    "Whether this hyphenation point comes from an explicit hyphen."
    :initform t :initarg :explicit :reader explicitp))
  (:documentation "The HYPHENATION-MIXIN class.
This is a mixin for hyphenation points."))

(defclass hyphenation-point (discretionary hyphenation-mixin)
  ()
  (:documentation "The HYPHENATION-POINT class.
This class represents hyphenation point discretionaries."))

(defun hyphenation-point-p (object)
  "Check whether OBJECT is a hyphenation point.
return :explicit or :implicit if OBJECT is a hyphenation point, or nil."
  (when (typep object 'hyphenation-point)
    (if (explicitp object) :explicit :implicit)))

(defun make-hyphenation-point
    (&rest initargs &key pre-break post-break no-break explicit)
  "Make a new EXPLICITP hyphenation point."
  (declare (ignore pre-break post-break no-break explicit))
  (apply #'make-instance 'hyphenation-point initargs))

(defgeneric hyphenated (object)
  (:documentation "Return OBJECT's hyphenation status.
Possible values are :explicit, :implicit, or nil.")
  (:method (object)
    "Call `hyphenation-point-p' on OBJECT. This is the default method."
    (hyphenation-point-p object)))


(defclass soft-hyphenation-point
    (discretionary hyphenation-mixin penalty-mixin)
  ()
  (:documentation "The SOFT-HYPHENATION-POINT class.
This class represents weighted hyphenation point discretionaries."))



;; Glues

(defclass glue (break-point)
  ((width
    :documentation "The glues's natural width."
    :initform 0 :initarg :width :reader width)
   (shrink
    :documentation "The glue's shrinkability."
    :initform 0 :initarg :shrink :reader shrink)
   (stretch
    :documentation "The glue's stretchability."
    :initform 0 :initarg :stretch :reader stretch))
  (:documentation "The GLUE class.
Glues represent breakable, elastic space."))

(defmethod initialize-instance :after ((glue glue) &key)
  "Validate GLUE's dimensions.
- WIDTH and SHRINK must be non-negative integers,
- STRETCH must be a non-negative integer, or +∞."
  (assert (and (rationalp (width glue)) (>= (width glue) 0)))
  (assert (and (rationalp (shrink glue)) (>= (shrink glue) 0)))
  (assert (or (and (rationalp (stretch glue)) (>= (stretch glue) 0))
	      ($= (stretch glue) +∞))))

(defmethod properties strnlcat ((glue glue) &key)
  "Advertise GLUE's properties."
  (format nil "Glue: ~Apt plus ~Apt minus ~Apt."
    (float (width glue))
    (float (stretch glue))
    (float (shrink glue))))

(defun gluep (object)
  "Return T if OBJECT is a glue."
  (typep object 'glue))


(defclass soft-glue (break-point penlaty-mixin)
  ()
  (:documentation "The SOFT-GLUE class.
This class represents weighted glues."))


(defun make-glue (&rest keys &key hard width shrink stretch penalty caliber)
  "Make a new HARD or soft (the default) glue out of WIDTH, SHRINK, STRETCH.
Soft glues also accept initial PERNALTY and CALIBER."
  (declare (ignore width shrink stretch penalty caliber))
  (apply #'make-instance (if hard 'glue 'soft-glue)
	 (remove-keys keys :hard)))

(defun make-interword-glue (&key hard (font *font*))
  "Make a HARD or soft (the default) interword glue.
Use FONT (*FONT* by default) to set up the spacing."
  (make-glue :hard hard
	     :width (tfm:interword-space font)
	     :shrink (tfm:interword-shrink font)
	     :stretch (tfm:interword-stretch font)))



;; Special break points

(defsingleton bop (break-point)
  ((idx :initform -1)) ;; slot override
  (:documentation "The BOP (Beginning of Paragraph) singleton class."))

(defmethod eol-idx ((bop bop))
  "Return NIL."
  nil)

(defparameter *bop* (make-instance 'bop))


;; #### NOTE: contrary to BOP, there can't be a single EOP instance because
;; the IDX value depends on the corresponding harray.
(defclass eop (break-point)
  ((idx :initarg :idx)) ;; slot override
  (:documentation "The EOP (End of Paragraph) class."))

(defgeneric eopp (object)
  (:documentation "Return T if OBJECT denotes an end of paragraph.")
  (:method (object)
    "Return T if OBJECT is an EOP (End of Paragraph) one.
This is the default method."
    (typep object 'eop)))

(defmethod properties strnlcat ((eop eop) &key)
  "Advertise nothing."
  nil)

(defmethod bol-idx ((eop eop))
  "Return NIL."
  nil)




;; ==========================================================================
;; Features Processing
;; ==========================================================================

;; #### TODO: the hlist construction process is currently done in a mix of
;; pure and impure functional style: every processing step (such as
;; hyphenation, kerning, etc.) returns a new hlist based on the original one,
;; but with original helts (such as discretionaries) potentially modified. At
;; some point in the future, we may want to save up some consing by
;; working exclusively on the original hlist instead.


;; #### NOTE: it's a bit too early to speak of "lineup" here, but since we
;; need the features, let's also define this close.

(defparameter *lineup-features*
  '(:kerning :ligatures :hyphenation)
  "The lineup features, as advertised by the interface.")


;; #### NOTE: the procedures handling ligatures and kerns below are aware of
;; discretionaries, but they are really meant for those inserted automatically
;; during the hyphenation process, that is, \discretionary{-}{}{} in the
;; middle of a word. They may be useful in the future, for some more general
;; cases, but combining ligatures, kerns, and discretionaries is impossible to
;; do statically in general. For example, if we end up with different
;; potential ligatures starting from a post-break element and going to both a
;; no-break and a pre-break later on, we can't represent that statically. The
;; only truly general solution is to delay ligature and kerns processing until
;; the harray is flattened. But then, this means that we also need to do that
;; every time we want to poll the size of various harray chunks. This could be
;; rather expensive (although I haven't tried it).



;; -------
;; Kerning
;; -------

(defvar *kerning* nil "Whether kerning is currently enabled.")

(defun get-kern (helt1 helt2)
  "Return kern for HELT1 and HELT2, or NIL."
  (and (typep helt1 'tfm:character-metrics)
       (typep helt2 'tfm:character-metrics)
       (eq (tfm:font helt1) (tfm:font helt2))
       (tfm:get-kern helt1 helt2)))

(defgeneric collect-kern (helt1 helt2 tail)
  (:documentation "Collect kern for (HELT1 HELT2 . TAIL).")
  (:method (helt1 helt2 tail)
    "Return NIL. This is the default method."
    nil)
  (:method ((helt1 tfm:character-metrics) (helt2 tfm:character-metrics) tail
	    ;; #### NOTE: the first two type checks in GET-KERN will be
	    ;; redundant because of the specialization of this method, but
	    ;; it's not such a big deal for now.
	    &aux (kern (get-kern helt1 helt2)))
    "Return a kern for HELT1 and HELT2 (TFM characters), or nil."
    (when kern (make-kern kern)))
  (:method ((helt1 tfm:character-metrics) (helt2 discretionary) tail)
    "Add kerns to discretionary HELT2 if needed."
    (when (pre-break helt2)
      (let ((kern (get-kern helt1 (first (pre-break helt2)))))
	(when kern (push (make-kern kern) (pre-break helt2)))))
    (if (no-break helt2)
      (let ((kern (get-kern helt1 (first (no-break helt2)))))
	(when kern (push (make-kern kern) (no-break helt2))))
      (let ((kern (get-kern helt1 (first tail))))
	(when kern (setf (no-break helt2) (list (make-kern kern))))))
    nil)
  (:method ((helt1 discretionary) (helt2 tfm:character-metrics) tail)
    "Add kerns to discretionary HELT1 if needed."
    (when (no-break helt1)
      (let ((kern (get-kern (first (last (no-break helt1))) helt2)))
	(when kern (endpush (make-kern kern) (no-break helt1)))))
    (when (post-break helt1)
      (let ((kern (get-kern (first (last (post-break helt1))) helt2)))
	(when kern (endpush (make-kern kern) (post-break helt1)))))
    nil))

(defun kern-hlist (hlist)
  "Return an new HLIST with kerns."
  (loop :for helts :on hlist
	:for helt1 := (first helts)
	:for helt2 := (second helts)
	:for tail := (cddr helts)
	:for kern := (collect-kern helt1 helt2 tail)
	:collect helt1
	:when kern :collect kern))



;; ----------
;; Ligaturing
;; ----------

(defvar *ligaturing* nil "Whether ligaturing is currently enabled.")

(defun get-ligature (helt1 helt2)
  "Return a ligature for HELT1 and HELT2, or NIL."
  (and (typep helt1 'tfm:character-metrics)
       (typep helt2 'tfm:character-metrics)
       (eq (tfm:font helt1) (tfm:font helt2))
       (tfm:get-ligature helt1 helt2)))

(defgeneric next-characters-1 (helt tail)
  (:documentation
   "Return a list of next characters for (HELT . TAIL).
This function looks up in (HELT . TAIL) for all characters directly
accessible. There can be several of them, notably if HELT is a
discretionary.")
  (:method (helt tail)
    "Return NIL. This is the default method."
    nil)
  (:method ((helt tfm:character-metrics) tail)
    "Return (HELT) since HELT is a TFM character."
    (list helt))
  (:method ((helt discretionary) tail)
    "Lookup next characters in discretionary HELT's pre-break and no-break."
    (append (next-characters (pre-break helt))
	    (next-characters (append (no-break helt) tail)))))

(defun next-characters (hlist)
  "Return a list of next characters in HLIST.
This function looks up in HLIST for all characters directly accessible.
There can be several of them, notably if HLIST begins with a discretionary."
  (when hlist (next-characters-1 (first hlist) (rest hlist))))

;; #### NOTE: after processing ligatures, we may end up with adjacent
;; discretionaries, but this is normal. For example, in the word ef-fi-cient,
;; what we get eventually is
;; e\discretionary{f-}{fi}{ffi}\discretionary{-}{}{}cient.
(defgeneric ligature-hlist-2 (helt1 helt2 tail)
  (:documentation "Ligature hlist made of (HELT1 HELT2 . TAIL).
Return a list of two values: a list of done elements that should be appended
to the new hlist, and the unprocessed new tail.")
  (:method (helt1 helt2 tail)
    "Return (HELT1) and (HELT2 . TAIL). This is the default method."
    (list (list helt1) (cons helt2 tail)))
  (:method ((helt1 tfm:character-metrics) (helt2 tfm:character-metrics) tail
	    ;; #### NOTE: the first two type checks in GET-LIGATURE will be
	    ;; redundant because of the specialization of this method, but
	    ;; it's not such a big deal for now.
	    &aux (ligature (get-ligature helt1 helt2)) composition)
    "Process ligatures between HELT1 and HELT2 TFM characters."
    (cond (ligature
	   (unless (tfm:delete-after ligature) (push helt2 composition))
	   (push (tfm:composite ligature) composition)
	   (unless (tfm:delete-before ligature) (push helt1 composition))
	   (list (subseq composition 0 (tfm:pass-over ligature))
		 ;; #### NOTE: because of the way TFM ligature programs work,
		 ;; we know that there's at least one thing left in this
		 ;; tail. Indeed, the pass over cannot exceed the number
		 ;; of retained original characters.
		 (append (nthcdr (tfm:pass-over ligature) composition)
			 tail)))
	  (t
	   (list (list helt1) (cons helt2 tail)))))
  (:method ((helt1 tfm:character-metrics) (helt2 discretionary) tail
	    &aux (eat-helt1
		  (some
		   (lambda (character) (get-ligature helt1 character))
		   (next-characters (cons helt2 tail)))))
    "Process ligatures between HELT1 TFM character and HELT2 discretionary."
    (cond (eat-helt1
	   (setf (pre-break helt2)
		 (ligature-hlist (cons helt1 (pre-break helt2)))
		 (no-break helt2)
		 (ligature-hlist (cons helt1 (no-break helt2))))
	   (list nil (cons helt2 tail)))
	  (t
	   (list (list helt1) (cons helt2 tail)))))
  (:method ((helt1 discretionary) (helt2 tfm:character-metrics) tail
	    &aux (eat-helt2
		  (or (get-ligature (first (last (no-break   helt1))) helt2)
		      (get-ligature (first (last (post-break helt1))) helt2))))
    "Process ligatures between HELT1 discretionary and HELT2 TFM character."
    (cond (eat-helt2
	   (setf (no-break helt1)
		 (ligature-hlist (append (no-break helt1) (list helt2)))
		 (post-break helt1)
		 (ligature-hlist (append (post-break helt1) (list helt2))))
	   (list nil (cons helt1 tail)))
	  (t
	   (list (list helt1) (cons helt2 tail))))))

(defun ligature-hlist-1 (helt tail)
  "Ligature hlist made of (HELT . TAIL).
Return a list of two values: a list of done elements that should be appended
to the new hlist, and the unprocessed new tail."
  (if tail
    (ligature-hlist-2 helt (first tail) (rest tail))
    (list (list helt))))

(defun ligature-hlist (hlist)
  "Return a new HLIST with ligatures."
  (loop :for helts := hlist :then tail
	:for (done tail) := (ligature-hlist-1 (first helts) (rest helts))
	:while helts
	:append done))



;; -----------
;; Hyphenation
;; -----------

;; #### FIXME: this comment is not clear to myself anymore...
;; #### NOTE: the hyphenation process below is simple, different from what TeX
;; does and should certainly be improved. For instance, TeX will consider only
;; one word between two glues, so for instance in "... foo.bar ...", bar will
;; never be hyphenated. There are also other rules that prevent hyphenation in
;; some situations, which we do not have right now.

(defvar *hyphenation* nil "Whether hyphenation is currently enabled.")

(defun explicit-hyphen-positions+1 (helts l)
  "Return explicit hyphen positions + 1 in the first L HELTS (a word)."
  (loop :for i :from 0 :upto (1- l)
	:for helt :in helts
	:when (char= (code-char (tfm:code helt)) #\-) :collect (1+ i)))

(defun hyphenate-word (helts l rules &aux points)
  "Hyphenate the first L HELTS (a word) with hyphenation RULES."
  ;; A word with explicit hyphens must not be hyphenated in any other way.
  (cond ((setq points (explicit-hyphen-positions+1 helts l))
	 (setq points (remove-if (lambda (position)
				   ;; #### WARNING: note the large inequality
				   ;; below. Remember that our positions here
				   ;; are "+1". On the right side, we're
				   ;; subtracting from the length L which
				   ;; already is "last position + 1", so
				   ;; nothing more to do.
				   (or (<= position *lefthyphenmin*)
				       (> position (- l *righthyphenmin*))))
			  points))
	 (if points
	   (loop :for i :from 0 :upto (1- l)
		 :for helt :in helts
		 :when (member i points) :collect (make-hyphenation-point)
		 :collect helt)
	   (subseq helts 0 l)))
	((setq points (get-hyphenation-points helts l rules))
	 (loop :for i :from 0 :upto (1- l)
	       :for helt :in helts
	       :when (member i points)
		 :collect (make-hyphenation-point
			   :pre-break (list (get-fchar #\-
					      (tfm:font (nth (1- i) helts))))
			   :explicit nil)
	       :collect helt))
	(t (subseq helts 0 l))))

(defun word-constituent-p (helt)
  "Return T if HELT is either an alphabetic of a dash font character."
  (when (typep helt 'tfm:character-metrics)
    (let ((char (code-char (tfm:code helt))))
      (or (alpha-char-p char) (char= char #\-)))))

(defun hyphenate-hlist (hlist &aux rules)
  "Return a new hyphenated HLIST."
  (setq rules (get-hyphenation-rules *language*))
  (loop :with l
	:with helts := hlist :while helts
	:if (word-constituent-p (first helts))
	  :do (setq l (or (position-if-not #'word-constituent-p helts)
			  (length helts)))
	  :and :append (hyphenate-word helts l rules)
	  :and :do (setq helts (subseq helts l))
	:else
	  :collect (first helts) :and :do (setq helts (rest helts))))




;; ==========================================================================
;; Glueing
;; ==========================================================================

(defun glue-hlist (hlist &optional hard)
  "Return a new glued hlist. Use HARD or soft (the default) glues."
  (loop :with previous-helt
	:for helt :in hlist
	:if (eq helt :blank)
	  :collect (make-interword-glue
		    :hard hard
		    ;; #### TODO: should look into previous discretionary.
		    :font (or (when (typep previous-helt
					   'tfm:character-metrics)
				(tfm:font previous-helt))
			      *font*))
	:else
	  :do (setq previous-helt helt)
	  :and :collect helt))




;; ==========================================================================
;; Slicing
;; ==========================================================================

(defparameter *blanks* '(#\Space #\Tab #\Newline #\Return)
  "The list of blank characters.")

(defun blankp (char)
  "Return T if CHAR is a blank character."
  (member char *blanks*))

(defun slice  (string)
  "Slice STRING into an hlist.
Words are sliced into *FONT* characters. Consecutive blanks are replaced with
a single :blank keyword."
  (loop :with l := (length string)
	:with i := 0
	:while (< i l)
	:for char := (aref string i)
	:if (blankp char)
	  :collect :blank
	  :and :do (setq i (or (position-if-not #'blankp string :start i) l))
	:else
	  :collect (get-fchar char)
	  :and :do (incf i)))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

;; This is the function that wraps buffer contents.
(defun make-hlist (&rest args &aux hlist)
  "Make an HLIST from ARGS.
ARGS are MAPCAN'ed together after processing. The processing in question
depends on the type of the argument. More specifically:
- lists are used as-is,
- strings are sliced (see `slice').
Finally, consecutive occurrences of :BLANK are replaced with a single one.
Note that the resulting hlist is /not/ stripped from left or right blanks."
  (setq hlist (mapcan (lambda (arg)
			(typecase arg
			  (string (slice arg))
			  (list arg)))
		args))
  (loop :with helts := hlist :while helts
	:for helt := (first helts)
	:if (eq helt :blank)
	  :collect :blank
	  :and :do (while (eq (first helts) :blank) (setq helts (rest helts)))
	:else
	  :collect helt
	  :and :do (setq helts (rest helts))))
