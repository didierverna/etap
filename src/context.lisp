(in-package :etap)

(defclass context ()
  ((font :documentation "The TFM font."
	 :initform *font*
	 :initarg :font
	 :accessor font)
   (hyphenation-rules :documentation "The hyphenation rules."
		      :initform *hyphenation-rules*
		      :initarg :hyphenation-rules
		      :accessor hyphenation-rules)
   (algorithm :documentation "The typesetting algorithm."
	      :initform :fixed
	      :initarg :algorithm
	      :accessor algorithm)
   (disposition :documentation "The paragraph's disposition."
		:initform :flush-left
		:initarg :disposition
		:accessor disposition)
   (features :documentation "The features."
	     :initform (list)
	     :initarg :features
	     :accessor features)
   (paragraph-width :documentation "The requested paragraph width in points."
		    :initform 284 ;; 284.52756pt = 10cm
		    :initarg :paragraph-width
		    :accessor paragraph-width)
   (text :documentation "The paragraph text."
	 :initform *text*
	 :initarg :text
	 :accessor text))
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
