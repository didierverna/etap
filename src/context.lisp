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

(defun make-hlist
    (&key (context *context*)
	  (nlstring (when context (nlstring context)))
	  (text (if nlstring (text nlstring) *text*) textp)
	  (language (if nlstring (language nlstring) *language*) languagep)
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation)))
  "Make a new hlist.
- CONTEXT defaults to *CONTEXT*.
- NLSTRING, FONT, and FEATURES are defaulted from CONTEXT, or to NIL, *FONT*,
  and NIL respectively.
- TEXT and LANGUAGE are defaulted from NLSTRING, or to *TEXT* and *LANGUAGE*
  respectively.
- FEATURES is defaulted from CONTEXT, or to NIL.
- KERNING, LIGATURES, and HYPHENATION are defaulted from FEATURES.

NLSTRING may be overridden if one of its dependencies is passed explicitly."
  (when (or (null nlstring) textp languagep)
    (setq nlstring (make-nlstring :text text :language language)))
  (%make-hlist nlstring font kerning ligatures hyphenation))

(defun make-lineup
    (&rest keys
     &key (context *context*)
	  (nlstring (when context (nlstring context)) nlstringp)
	  (text (if nlstring (text nlstring) *text*) textp)
	  (language (if nlstring (language nlstring) *language*) languagep)
	  (font (if context (font context) *font*) fontp)
	  (features (when context (features context)) featuresp)
	  (kerning (getf features :kerning) kerningp)
	  (ligatures (getf features :ligatures) ligaturesp)
	  (hyphenation (getf features :hyphenation) hyphenationp)
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
	  hlist)
  "Make a new lineup.
- DISPOSITION and ALGORITHM are defaulted from CONTEXT, or to :FLUSH-LEFT and
  :FIXED respectively.

HLIST may be overridden if one of its dependencies is passed explicitly, or
needs to be overridden itself. See `make-hlist' for the behavior of the other
parameters."
  (declare (ignore text language font kerning ligatures hyphenation))
  (when (or (null hlist)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp)
    (setq hlist (apply #'make-hlist
		  (remove-keys keys :disposition :algorithm))))
  (%make-lineup hlist disposition algorithm))

(defun make-breakup
    (&rest keys
     &key (context *context*)
	  (nlstring (when context (nlstring context)) nlstringp)
	  (text (if nlstring (text nlstring) *text*) textp)
	  (language (if nlstring (language nlstring) *language*) languagep)
	  (font (if context (font context) *font*) fontp)
	  (features (when context (features context)) featuresp)
	  (kerning (getf features :kerning) kerningp)
	  (ligatures (getf features :ligatures) ligaturesp)
	  (hyphenation (getf features :hyphenation) hyphenationp)
	  (disposition (if context (disposition context) :flush-left)
		       dispositionp)
	  (algorithm (if context (algorithm context) :fixed) algorithmp)
	  (hlist nil hlistp)
	  lineup
	  (width (if context (paragraph-width context) *paragraph-width*)))
  "Make a new breakup.
- WIDTH is defaulted from CONTEXT, or to *PARAGRAPH-WIDTH*.

LINEUP may be overridden if one of its dependencies is passed explicitly, or
needs to be overridden itself. See `make-lineup' for the behavior of the other
parameters."
  (declare (ignore text language font kerning ligatures hyphenation hlist))
  (when (or (null lineup)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp
	    hlistp)
    (setq lineup (apply #'make-lineup (remove-keys keys :lineup :width))))
  (%make-breakup lineup disposition width algorithm))

(defun make-paragraph
    (&rest keys
     &key (context *context*)
	  (nlstring (when context (nlstring context)) nlstringp)
	  (text (if nlstring (text nlstring) *text*) textp)
	  (language (if nlstring (language nlstring) *language*) languagep)
	  (font (if context (font context) *font*) fontp)
	  (features (when context (features context)) featuresp)
	  (kerning (getf features :kerning) kerningp)
	  (ligatures (getf features :ligatures) ligaturesp)
	  (hyphenation (getf features :hyphenation) hyphenationp)
	  (disposition (if context (disposition context) :flush-left)
		       dispositionp)
	  (algorithm (if context (algorithm context) :fixed) algorithmp)
	  (hlist nil hlistp)
	  (lineup nil lineupp)
	  (width (if context (paragraph-width context) *paragraph-width*)
		 widthp)
	  breakup)
  "Make a new paragraph.

BREAKUP may be overridden if one of its dependencies is passed explicitly, or
needs to be overridden itself. See `make-breakup' for the behavior of the
other parameters."
  (declare (ignore text language font kerning ligatures hyphenation
		   disposition algorithm width))
  ;; #### FIXME: this is ugly. There's a lot of redundancy in the various data
  ;; structures dependencies, and this is apparent below.
  (when (or (null hlist)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp)
    (setq hlist (apply #'make-hlist
		  (remove-keys keys
		    :disposition :algorithm :hlist :lineup :width :breakup))
	  hlistp t)
    (push hlist keys)
    (push :hlist keys))
  (when (or (null lineup)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp
	    hlistp)
    (setq lineup (apply #'make-lineup
		   (remove-keys keys :lineup :width :breakup))
	  lineupp t)
    (push lineup keys)
    (push :lineup keys))
  (when (or (null breakup)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp
	    hlistp
	    lineupp
	    widthp)
    (setq breakup (apply #'make-breakup (remove-keys keys :breakup))))
  (%make-paragraph hlist lineup breakup))
