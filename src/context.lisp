(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Contexts
;; ==========================================================================

;; #### NOTE: the paragraph width comes from a caliber, but outside the GUI,
;; we let it be whatever people want (so no calibration here).

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
Defaults to the default value of the *PARAGRAPH-WIDTH* caliber (284pt)."
    :initform (caliber-default *paragraph-width*) :initarg :paragraph-width
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
