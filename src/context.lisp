(in-package :etap)

;; ==========================================================================
;; Contexts
;; ==========================================================================

(defclass context ()
  ((font
    :documentation "The TFM font."
    :initform *font* :initarg :font :accessor font)
   (algorithm
    :documentation "The typesetting algorithm. Defaults to :FIXED.
It is either a keyword naming the algorithm, or a list beginning with it and
followed by a property list of algorithm-dependent options."
    :initform :fixed :initarg :algorithm :accessor algorithm)
   (disposition
    :documentation "The paragraph's disposition. Defaults to :FLUSH-LEFT.
It is either a keyword from `*dispositions*', or a list beginning with it and
followed by a property list of `*disposition-options*'."
    :initform :flush-left :initarg :disposition :accessor disposition)
   (features
    :documentation "The lineup features. Defaults to NIL.
It is a property list of `*lineup-features*'."
    :initform nil :initarg :features :accessor features)
   (paragraph-width
    :documentation "The paragraph width, in points.
Defaults to *PARAGRAPH-WIDTH*."
    :initform *paragraph-width* :initarg :paragraph-width
    :accessor paragraph-width)
   (nlstring
    :documentation "The paragraph's `nlstring'."
    :accessor nlstring))
  (:documentation "The CONTEXT class.
A context object allows to centralize the complete experimental conditions for
paragraph typesetting."))

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

;; #### NOTE: I'm keeping this function around for debugging purposes only.
;; HLists are internal temporary objects which are not supposed to be kept
;; around. The other entry points do not accept hlists arguments anymore.
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
  "Make a new hlist. See `context' for an explanation of the keywords.
- CONTEXT defaults to *CONTEXT*.
- Most other options are defaulted from the context, or to their corresponding
  global variable otherwise, but may be overridden on demand.
- Providing either :text or :language will force recomputing the nlstring.
- Explicit features take precedence over FEATURES."
  (when (or (null nlstring) textp languagep)
    (setq nlstring (make-nlstring :text text :language language)))
  (setq features (list :kerning kerning
		       :ligatures ligatures
		       :hyphenation hyphenation))
  (%make-hlist nlstring font features))

(defun make-lineup
    (&key (context *context*)
	  (nlstring (when context (nlstring context)))
	  (text (if nlstring (text nlstring) *text*) textp)
	  (language (if nlstring (language nlstring) *language*) languagep)
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed)))
  "Make a new lineup. See `context' for an explanation of the keywords.
- CONTEXT defaults to *CONTEXT*.
- Most other options are defaulted from the context, or to their corresponding
  global variable otherwise, but may be overridden on demand.
- Providing either :text or :language will force recomputing the nlstring.
- Explicit features take precedence over FEATURES."
  (when (or (null nlstring) textp languagep)
    (setq nlstring (make-nlstring :text text :language language)))
  (setq features (list :kerning kerning
		       :ligatures ligatures
		       :hyphenation hyphenation))
  (%make-lineup nlstring font features disposition algorithm))

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
	  lineup
	  (width (if context (paragraph-width context) *paragraph-width*)))
  "Make a new breakup. See `context' for an explanation of the keywords.
- CONTEXT defaults to *CONTEXT*.
- Most other options are defaulted from the context, or to their corresponding
  global variable otherwise, but may be overridden on demand.
- Providing any option except for :context, :width, and :lineup will force
  recomputing the lineup (see `make-lineup' for more information."
  (declare (ignore text language font kerning ligatures hyphenation
		   disposition algorithm))
  (when (or (null lineup)
	    nlstringp textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp)
    (setq lineup (apply #'make-lineup (remove-keys keys :lineup :width))))
  (%make-breakup lineup width))

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
