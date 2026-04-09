(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Contexts
;; ==========================================================================

;; #### NOTE: the paragraph width comes from a caliber, but outside the GUI,
;; we let it be whatever people want (so no calibration here).

(defclass context ()
  ((buffer
    :documentation "The contents buffer. Defaults to *TEXT*."
    :initform *text* :initarg :buffer :accessor buffer)
   (language
    :documentation "The default language. Initially *LANGUAGE*."
    :initform *language* :initarg :language :accessor language)
   (font
    :documentation "The default font. Initially *FONT*."
    :initform *font* :initarg :font :accessor font)
   (features
    :documentation "The lineup features. Defaults to NIL.
It is a property list of `*lineup-features*'."
    :initform nil :initarg :features :accessor features)
   (disposition
    :documentation "The paragraph's disposition. Defaults to :FLUSH-LEFT.
It is either a keyword from `*dispositions*', or a list beginning with it and
followed by a property list of `*disposition-options*'."
    :initform :flush-left :initarg :disposition :accessor disposition)
   (algorithm
    :documentation "The typesetting algorithm. Defaults to :FIXED.
It is either a keyword naming the algorithm, or a list beginning with it and
followed by a property list of algorithm-dependent options."
    :initform :fixed :initarg :algorithm :accessor algorithm)
   (paragraph-width
    :documentation "The paragraph width, in points.
Defaults to the default value of the *PARAGRAPH-WIDTH* caliber (284pt)."
    :initform (caliber-default *paragraph-width*) :initarg :paragraph-width
    :accessor paragraph-width))
  (:documentation "The CONTEXT class.
A context object allows to centralize the complete experimental conditions for
paragraph typesetting."))

(defun make-context
    (&rest keys
     &key buffer language font features disposition algorithm paragraph-width)
  (declare (ignore buffer language font features disposition algorithm
		   paragraph-width))
  "Create a new context object."
  (apply #'make-instance 'context keys))

(defvar *context* (make-context)
  "The global context.")
