;; An `hlist' (borrowing terminology from TeX) is the result of slicing
;; (hashing? but that's not the meaning of the h in hlist ;-)) the input text
;; into a list of individual items: characters (font-dependent), kerns (if
;; requested), discretionaries for hyphenation points (if requested,
;; language-dependent) and ligatures (if requested), and interword glues.

;; The computation of the hlist is considered a pre-processing stage. No
;; aspects of paragraph formatting are known yet (paragraph disposition,
;; typesetting algorithm, or anything else). Most typesetting algorithms
;; post-process the hlist (for example by adjusting penalties or adding
;; glues).

;; After this is done, the hlist (an actual list) is transformed into an array
;; called the `harray', itself stored into a lineup object.

;; #### WARNING: there are a number of characteristics in the hlist that
;; affect the way algorithms are implemented. These specificities should be
;; kept in mind because should they change, said algorithms may become buggy.
;; In particular:
;; - the paragraph string has leading and trailing spaces removed. There is a
;;   single glue between words (the KP algorithm adds an infinitely
;;   stretchable one at the end),
;; - the only discretionaries that we have come from the hyphenation step, so
;;   they originally appear only in the middle of words. However, they may
;;   turn out to be anywhere after processing ligatures and kerns.

(in-package :etap)


;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *typesetting-features*
  '((:kerning t) (:ligatures t) (:hyphenation t))
  "The typesetting features, as advertised by the interface.")




;; ==========================================================================
;; Geometry
;; ==========================================================================

(defgeneric width (object)
  (:documentation "Return OBJECT's width.")
  (:method (object)
    "Return 0 by default."
    0)
  (:method ((list list))
    "Return the sum of the widths of all elements in LIST."
    (reduce #'+ (mapcar #'width list)))
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metric's width."
    (tfm:width character)))

(defgeneric height (object)
  (:documentation "Return OBJECT's height.")
  (:method (object)
    "Return 0 by default."
    0)
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metrics's height."
    (tfm:height character)))

(defgeneric depth (object)
  (:documentation "Return OBJECT's depth.")
  (:method (object)
    "Return 0 by default."
    0)
  (:method ((character tfm:character-metrics))
    "Return TFM CHARACTER metrics's depth."
    (tfm:depth character)))




;; ==========================================================================
;; HList Items
;; ==========================================================================

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

(defgeneric hyphenated (object)
  (:documentation "Return OBJECT's hyphenation status.
Possible values are nil, :explicit, or :implicit.")
  (:method (object)
    "Return NIL. This is the default method."
    nil)
  (:method ((object hyphenation-mixin))
    "Return hyphenation mixin OBJECT's status (:explicit or :implicit)."
    (if (explicitp object) :explicit :implicit)))


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
	      ($= (stretch glue) +∞))))

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




;; ==========================================================================
;; HList Creation
;; ==========================================================================

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

;; ----------------
;; Kerns processing
;; ----------------

(defun get-kern (elt1 elt2)
  "Return kern for ELT1 and ELT2, or NIL."
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:get-kern elt1 elt2)))

(defgeneric collect-kern (elt1 elt2 remainder)
  (:documentation "Collect kern for (ELT1 ELT2 . REMAINDER).")
  (:method (elt1 elt2 remainder)
    "Return NIL. This is the default method."
    nil)
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
	    remainder &aux (kern (tfm:get-kern elt1 elt2)))
    "Return a kern for characters ELT1 and ELT2, or nil."
    (when kern (make-kern kern)))
  (:method ((elt1 tfm:character-metrics) (elt2 discretionary) remainder)
    "Add kerns to discretionary ELT2 if needed."
    (when (pre-break elt2)
      (let ((kern (get-kern elt1 (car (pre-break elt2)))))
	(when kern (push (make-kern kern) (pre-break elt2)))))
    (if (no-break elt2)
      (let ((kern (get-kern elt1 (car (no-break elt2)))))
	(when kern (push (make-kern kern) (no-break elt2))))
      (let ((kern (get-kern elt1 (car remainder))))
	(when kern (setf (no-break elt2) (list (make-kern kern))))))
    nil)
  (:method ((elt1 discretionary) (elt2 tfm:character-metrics) remainder)
    "Add kerns to discretionary ELT1 if needed."
    (when (no-break elt1)
      (let ((kern (get-kern (car (last (no-break elt1))) elt2)))
	(when kern (endpush (make-kern kern) (no-break elt1)))))
    (when (post-break elt1)
      (let ((kern (get-kern (car (last (post-break elt1))) elt2)))
	(when kern (endpush (make-kern kern) (post-break elt1)))))
    nil))

(defun process-kerns (hlist)
  "Return an new HLIST with kerns."
  (loop :for elements :on hlist
	:for elt1 := (car elements)
	:for elt2 := (cadr elements)
	:for remainder := (cddr elements)
	:for kern := (collect-kern elt1 elt2 remainder)
	:collect elt1
	:when kern :collect kern))


;; --------------------
;; Ligatures processing
;; --------------------

(defun get-ligature (elt1 elt2)
  "Return a ligature for ELT1 and ELT2, or NIL."
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:get-ligature elt1 elt2)))

(defgeneric next-characters-1 (elt remainder)
  (:documentation
   "Return a list of next characters for (ELT . REMAINDER).
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

(defun next-characters (hlist)
  "Return a list of next characters in HLIST.
This function looks up in HLIST for all characters directly accessible.
There can be several of them, notably if HLIST begins with a discretionary."
  (when hlist (next-characters-1 (car hlist) (cdr hlist))))

;; #### NOTE: after processing ligatures, we may end up with adjacent
;; discretionaries, but this is normal. For example, in the word ef-fi-cient,
;; what we get eventually is
;; e\discretionary{f-}{fi}{ffi}\discretionary{-}{}{}cient.
(defgeneric process-ligatures-2 (elt1 elt2 remainder)
  (:documentation "Process ligatures for (ELT1 ELT2 . REMAINDER).
Return a list of two values: a list of done elements that should be appended
to the new hlist, and the unprocessed new remainder.")
  (:method (elt1 elt2 remainder)
    "Return (ELT1) and (ELT2 . REMAINDER). This is the default method."
    (list (list elt1) (cons elt2 remainder)))
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
	    remainder
	    &aux (ligature (tfm:get-ligature elt1 elt2)) composition)
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
	    &aux (eat-elt1
		  (some
		   (lambda (character) (tfm:get-ligature elt1 character))
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
	    &aux (eat-elt2
		  (or (get-ligature (car (last (no-break elt1))) elt2)
		      (get-ligature (car (last (post-break elt1))) elt2))))
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
  "Process ligatures for (ELT . REMAINDER).
Return a list of two values: a list of done elements that should be appended
to the new hlist, and the unprocessed new remainder."
  (if remainder
    (process-ligatures-2 elt (car remainder) (cdr remainder))
    (list (list elt))))

(defun process-ligatures (hlist)
  "Return a new HLIST with ligatures."
  (loop :for elts := hlist :then remainder
	:for (done remainder) := (process-ligatures-1 (car elts) (cdr elts))
	:while elts
	:append done))


;; ---------------
;; Word processing
;; ---------------

(defun get-character (char font)
  "Get CHAR in FONT. Replace CHAR by a question mark if not found."
  ;; #### TODO: no input encoding support yet.
  (or (tfm:get-character (char-code char) font)
      ;; #### FIXME: this one had better be available! Fall back to a null
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


;; ------------
;; Text slicing
;; ------------

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
(defun slice
    (text language font hyphenate
     &aux (hyphenation-rules (when hyphenate (get-hyphenation-rules language))))
  "Slice LANGUAGE TEXT in FONT, possibly HYPHENATE'ing it.
Return a list of FONT characters, interword glues, and discretionaries if
HYPHENATE. TEXT's leading and trailing spaces are removed,, and inner
consecutive blanks are replaced with a single interword glue."
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


;; -----------------
;; HList computation
;; -----------------

(defun %make-hlist (text language font kerning ligatures hyphenation)
  "Make a new hlist for LANGUAGE TEXT in FONT.
When requested, include KERNING, LIGATURES, and HYPHENATION constituents."
  (unless (zerop (length text)) ; works for both NIL and ""
    (let ((hlist (slice text language font hyphenation)))
      ;; #### WARNING: the order is important below. Kerning must be computed
      ;; after ligature characters have been inserted, and the processing of
      ;; ligatures and kerning may affect the contents of discretionaries, so
      ;; we must add hyphenation clues only after everything else has been
      ;; done.
      (when ligatures (setq hlist (process-ligatures hlist)))
      (when kerning (setq hlist (process-kerns hlist)))
      (when hyphenation
	(mapc (lambda (element)
		(when (hyphenation-point-p element)
		  (push (if (explicitp element)
			  :explicit-hyphenation-clue
			  :hyphenation-clue)
			(no-break element))))
	  hlist))
      hlist)))
