(in-package :etap)

;; ==========================================================================
;; Entry Points
;; ==========================================================================

(defun make-lineup
    (&key (context *context*)
	  (text
	   (if (and context (nlstring context))
	     (text (nlstring context))
	     *text*)
	   textp)
	  (language
	   (if (and context (nlstring context))
	     (language (nlstring context))
	     *language*)
	   languagep)
	  (font (if context (font context) *font*))
	  (features (when context (features context)))
	  (kerning (getf features :kerning))
	  (ligatures (getf features :ligatures))
	  (hyphenation (getf features :hyphenation))
	  (disposition (if context (disposition context) :flush-left))
	  (algorithm (if context (algorithm context) :fixed))
     &aux (nlstring (if (or textp languagep (null context))
		      (make-nlstring :text text :language language)
		      (nlstring context))))
  "Make a new lineup. See `context' for an explanation of the keywords.
- CONTEXT defaults to *CONTEXT*.
- Most other options are defaulted from the context, or to their corresponding
  global variable otherwise, but may be overridden on demand.
- Providing either :text or :language will force recomputing the nlstring.
- Explicit features take precedence over FEATURES."
  (setq features (list :kerning kerning
		       :ligatures ligatures
		       :hyphenation hyphenation))
  (%make-lineup nlstring font features disposition algorithm))

(defun make-breakup
    (&rest keys
     &key (context *context*)
	  (text
	   (if (and context (nlstring context))
	     (text (nlstring context))
	     *text*)
	   textp)
	  (language
	   (if (and context (nlstring context))
	     (language (nlstring context))
	     *language*)
	   languagep)
	  (font (if context (font context) *font*) fontp)
	  (features (when context (features context)) featuresp)
	  (kerning (getf features :kerning) kerningp)
	  (ligatures (getf features :ligatures) ligaturesp)
	  (hyphenation (getf features :hyphenation) hyphenationp)
	  (disposition (if context (disposition context) :flush-left)
		       dispositionp)
	  (algorithm (if context (algorithm context) :fixed) algorithmp)
	  lineup
	  (width (if context (paragraph-width context)
		     (caliber-default *paragraph-width*))))
  "Make a new breakup. See `context' for an explanation of the keywords.
- CONTEXT defaults to *CONTEXT*.
- Most other options are defaulted from the context, or to their corresponding
  global variable otherwise, but may be overridden on demand.
- Providing any option except for :context, :width, and :lineup will force
  recomputing the lineup (see `make-lineup' for more information."
  (declare (ignore text language font kerning ligatures hyphenation
		   disposition algorithm))
  (when (or (null lineup)
	    textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp)
    (setq lineup (apply #'make-lineup (remove-keys keys :lineup :width))))
  (%make-breakup lineup width))
