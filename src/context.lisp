(in-package :etap)

;; =================
;; The Context Class
;; =================

(defclass context ()
  ((font :initform *font*
	 :reader font
	 :documentation "The TFM font description object.")
   (hyphenation-rules :initform *hyphenation-rules*
		      :reader hyphenation-rules
		      :documentation "The hyphenation rules object.")
   (algorithm :initform :fixed :initarg :algorithm
	      :accessor algorithm
	      :documentation "The algorithm to use.")
   (disposition :initform :flush-left :initarg :disposition
		:accessor disposition
		:documentation "The requested disposition.")
   (features :initform (list) :initarg :features
	     :accessor features
	     :documentation "The list of requested features.")
   (paragraph-width :initform 284 ;; 284.52756pt = 10cm
		    :initarg :paragraph-width
		    :accessor paragraph-width
		    :documentation "The requested paragraph width in points.")
   (text :initform *initial-text* :initarg :text
	 :accessor text
	 :documentation "The paragraph's text."))
  (:documentation "The CONTEXT class.
A context object stores the requested parameters for one experiment."))

(defun make-context
    (&rest keys &key algorithm disposition features paragraph-width text)
  (declare (ignore algorithm disposition features paragraph-width text))
  "Create a new context object."
  (apply #'make-instance 'context keys))

(defvar *context* (make-context)
  "The global context.")



;; =================
;; Context Utilities
;; =================

(defun make-context-lineup (&optional (context *context*))
  "Make a new lineup for CONTEXT (the global context by default)."
  (apply #'make-lineup
    (text context)
    (font context)
    (hyphenation-rules context)
    (features context)))

(defun make-context-paragraph
    (&optional (context *context*)
     &aux (width (paragraph-width context))
	  (lineup (make-context-lineup context)))
  "Make a new paragraph for CONTEXT (the global context by default)."
  (make-paragraph width
		  (create-pinned-lines
		   (when lineup
		     (apply #'create-lines
		       lineup
		       width
		       (disposition context)
		       (algorithm-type (algorithm context))
		       (algorithm-options (algorithm context))))
		   width
		   (disposition-type (disposition context)))))
