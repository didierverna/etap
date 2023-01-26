(in-package :etap)

(defclass context ()
  ((font :initform *font* :initarg :font :accessor font
	 :documentation "The TFM font.")
   (hyphenation-rules :initform *hyphenation-rules*
		      :initarg :hyphenation-rules
		      :accessor hyphenation-rules
		      :documentation "The hyphenation rules.")
   (algorithm :initform :fixed :initarg :algorithm :accessor algorithm
	      :documentation "The typesetting algorithm.")
   (disposition :initform :flush-left :initarg :disposition
		:accessor disposition
		:documentation "The paragraph's disposition.")
   (features :initform (list) :initarg :features
	     :accessor features
	     :documentation "The features.")
   (paragraph-width :initform 284 ;; 284.52756pt = 10cm
		    :initarg :paragraph-width
		    :accessor paragraph-width
		    :documentation "The requested paragraph width in points.")
   (text :initform *text* :initarg :text :accessor text
	 :documentation "The paragraph text."))
  (:documentation "The CONTEXT class.
A context object stores the requested parameters for one experiment."))

(defun make-context
    (&rest keys
     &key font hyphenation-rules algorithm disposition features
	  paragraph-width
	  text)
  (declare (ignore font hyphenation-rules algorithm disposition features
		   paragraph-width
		   text))
  "Create a new context object."
  (apply #'make-instance 'context keys))

(defvar *context* (make-context)
  "The global context.")
