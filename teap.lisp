;;; teap.lisp --- Typesetting Experimental Algorithms Platform

;; ==========
;; Meta Level
;; ==========

(defpackage :teap
  (:use :cl)
  (:export :run))

(in-package :teap)



;; ===
;; GUI
;; ===

(defclass state ()
  ((kerning :initform nil :accessor kerning)
   (ligatures :initform nil :accessor ligatures)
   (hyphenation :initarg nil :accessor hyphenation)
   (layout :initarg :flush-left :accessor layout)
   (text :accessor text)))

;; #### NOTE: we check for the existence of the pane because this function
;; might be called too early in the process. A better implementation would
;; need to wait for the application to be running, and then install the after
;; methods.
(defun redisplay
    (&aux (frame clim:*application-frame*)
	  (pane (clim:find-pane-named frame 'typesetting)))
  (when pane (clim:redisplay-frame-pane frame pane)))

(defun render-paragraph
    (frame pane &aux (*standard-output* pane) (state (state frame)))
  (print (text state)))

(clim:define-application-frame teap ()
  ((state :initform (make-instance 'state) :reader state))
  (:panes
   (processing
    (clim:with-radio-box
	(:type :some-of
	 :value-changed-callback
	 (lambda (pane value &aux (state (state clim:*application-frame*)))
	   (declare (ignore pane))
	   (setf (kerning state) nil
		 (ligatures state) nil
		 (hyphenation state) nil)
	   (when (consp value)
	     (mapc (lambda (gadget)
		     (case (clim:gadget-id gadget)
		       (:kerning (setf (kerning state) t))
		       (:ligatures (setf (ligatures state) t))
		       (:hyphenation (setf (hyphenation state) t))))
	       value))
	   (redisplay)))
      (clim:make-pane 'clim:toggle-button
		      :label "Kerning" :id :kerning)
      (clim:make-pane 'clim:toggle-button
		      :label "Ligatures" :id :ligatures)
      (clim:make-pane 'clim:toggle-button
		      :label "Hyphenation" :id :hyphenation)))
   (layout
    (clim:with-radio-box
	(:value-changed-callback
	 (lambda (pane value)
	   (declare (ignore pane))
	   (setf (layout (state clim:*application-frame*))
		 (clim:gadget-id value))
	   (redisplay)))
      (clim:radio-box-current-selection
       (clim:make-pane 'clim:toggle-button
		       :label "Flush left" :id :flush-left))
      (clim:make-pane 'clim:toggle-button
		      :label "Flush right" :id :flush-right)
      (clim:make-pane 'clim:toggle-button
		      :label "Justified" :id :justified)))
   (source :text-editor
	   :scroll-bars :vertical
	   :nlines 10
	   :ncolumns 60
	   :value "Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
	   :value-changed-callback
	   (lambda (pane value)
	     (declare (ignore pane))
	     (setf (text (state clim:*application-frame*)) value)
	     (redisplay)))
   (typesetting :application :min-width 800 :min-height 300
		:display-function #'render-paragraph))
  (:layouts
   (default
    (clim:vertically ()
      (clim:horizontally ()
	(clim:labelling (:label "Processing") processing)
	(clim:labelling (:label "Layout") layout))
      (clim:labelling (:label "Source text") source)
      (:fill (clim:labelling (:label "Typeset text") typesetting))))))



;; ===========
;; Entry Point
;; ===========

(defmacro run ()
  (clim:run-frame-top-level (clim:make-application-frame 'teap)))

;;; teap.lisp ends here
