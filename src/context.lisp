(in-package :etap)

;; ==========================================================================
;; Contexts
;; ==========================================================================

(defclass context ()
  ((font
    :documentation "The TFM font."
    :initform *font* :initarg :font :accessor font)
   (algorithm
    :documentation "The typesetting algorithm."
    :initform :fixed :initarg :algorithm :accessor algorithm)
   (disposition
    :documentation "The paragraph's disposition."
    :initform :flush-left :initarg :disposition :accessor disposition)
   (features
    :documentation "The features."
    :initform (list) :initarg :features :accessor features)
   (paragraph-width
    :documentation "The requested paragraph width in points."
    :initform *paragraph-width*
    :initarg :paragraph-width
    :accessor paragraph-width)
   (nlstring
    :documentation "The paragraph's natural language string."
    :accessor nlstring))
  (:documentation "The CONTEXT class.
A context object stores the requested parameters for one experiment."))

(defmethod initialize-instance :after
    ((context context) &key (text *text*) (language *language*))
  "Create CONTEXT's nlstring from TEXT in LANGUAGE."
  (setf (nlstring context) (make-nlstring :text text :language language)))

(defun make-context
    (&rest keys
     &key font algorithm disposition features paragraph-width
	  text language)
  (declare (ignore font algorithm disposition features paragraph-width
		   text language))
  "Create a new context object."
  (apply #'make-instance 'context keys))

(defvar *context* (make-context)
  "The global context.")


(defmethod text ((context context))
  "Return CONTEXT's nlstring text."
  (text (nlstring context)))

(defmethod language ((context context))
  "Return CONTEXT's nlstring language."
  (language (nlstring context)))




;; ==========================================================================
;; Entry Points
;; ==========================================================================

;; #### NOTE: this function's interface doesn't have an NLSTRING keyword
;; argument on purpose: it's more convenient to provide access to TEXT and
;; LANGUAGE directly, or rely on CONTEXT for either, or both of these.
(defun make-hlist
    (&key (context *context*)
	  (text (if context (text context) *text*))
	  (language (if context (language context) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation)))
  "Make a new hlist.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, and FONT are defaulted from the corresponding
global variables, and KERNING, LIGATURES, and HYPHENATION are defaulted from
FEATURES."
  (%make-hlist text language font kerning ligatures hyphenation))

;; #### NOTE: this function's interface doesn't have an NLSTRING keyword
;; argument on purpose: it's more convenient to provide access to TEXT and
;; LANGUAGE directly, or rely on CONTEXT for either, or both of these.
(defun make-lineup
    (&key (context *context*)
	  (text (if context (text context) *text*))
	  (language (if context (language context) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (hlist (%make-hlist text language font kerning ligatures
			      hyphenation)))
  "Make a new lineup.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, and FONT are defaulted from the corresponding
global variables, KERNING, LIGATURES, and HYPHENATION are defaulted from
FEATURES, DISPOSITION is defaulted to :flush-left, and ALGORITHM to :fixed."
  (%make-lineup hlist disposition algorithm))

(defun make-breakup
    (&key (context *context*)
	  (text (if context (text context) *text*))
	  (language (if context (language context) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (width (if context (paragraph-width context) *paragraph-width*))
	  ;; #### WARNING: no mutual coherency checks for these three.
	  (hlist (%make-hlist text language font kerning ligatures hyphenation))
	  (lineup (%make-lineup hlist disposition algorithm)))
  "Make a new breakup.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed. Unless provided, HLIST, LINEUP, and BREAKUP are
subsequently computed."
  (%make-breakup lineup disposition width algorithm))

(defun make-paragraph
    (&key (context *context*)
	  (text (if context (text context) *text*))
	  (language (if context (language context) *language*))
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  (width (if context (paragraph-width context) *paragraph-width*))
	  ;; #### WARNING: no mutual coherency checks for these three.
	  (hlist (%make-hlist text language font kerning ligatures hyphenation))
	  (lineup (%make-lineup hlist disposition algorithm))
	  (breakup (%make-breakup lineup disposition width algorithm)))
  "Make a new paragraph.
When provided, CONTEXT is used to default the other parameters.
Otherwise, TEXT, LANGUAGE, FONT, and (paragraph) WIDTH, are defaulted from the
corresponding global variables, KERNING, LIGATURES, and HYPHENATION are
defaulted from FEATURES, DISPOSITION is defaulted to :flush-left, and
ALGORITHM to :fixed. Unless provided, HLIST, LINEUP, and BREAKUP are
subsequently computed."
  (%make-paragraph hlist lineup breakup))
