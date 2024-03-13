(in-package :etap)

;; #### NOTE: this, along with some :initform values below, are necessary
;; forward declarations.
(defvar *paragraph-width* 284 ;; 284.52756pt = 10cm
  "The default paragraph width.")

(defclass context ()
  ((font :documentation "The TFM font."
	 :initform *font*
	 :initarg :font
	 :accessor font)
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
   (beds :documentation "Whether to record river beds."
	 :initform nil :initarg :beds :accessor beds)
   (paragraph-width :documentation "The requested paragraph width in points."
		    :initform *paragraph-width*
		    :initarg :paragraph-width
		    :accessor paragraph-width)
   (nlstring :documentation "The paragraph's natural language string."
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
