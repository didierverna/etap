;;; teap.lisp --- Typesetting Experimental Algorithms Platform

;; ==========
;; Meta Level
;; ==========

(defpackage :teap
  (:add-use-defaults t)
  (:use :capi)
  (:export :run))

(in-package :teap)


(defun typeset (interface &aux (state (state interface)))
  (setf (paragraph state) (text state))
  (gp:invalidate-rectangle (rendering interface)))



;; ===
;; GUI
;; ===

(defconstant +initial-text+
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defclass state ()
  ((kerning :initform nil :accessor kerning)
   (ligatures :initform nil :accessor ligatures)
   (hyphenation :initarg nil :accessor hyphenation)
   (disposition :initarg :flush-left :accessor disposition)
   (text :initform +initial-text+ :accessor text)
   (paragraph :initform +initial-text+ :accessor paragraph)))

(defun set-feature (value interface)
  ;; #### FIXME: this is not satisfactory. Value should be the symbol
  ;; directly.
  (cond ((string= value "Kerning")
	 (setf (kerning (state interface)) t))
	((string= value "Ligatures")
	 (setf (ligatures (state interface)) t))
	((string= value "Hyphenation")
	 (setf (hyphenation (state interface)) t)))
  (typeset interface))

(defun unset-feature (value interface)
  ;; #### FIXME: this is not satisfactory. Value should be the symbol
  ;; directly.
  (cond ((string= value "Kerning")
	 (setf (kerning (state interface)) nil))
	((string= value "Ligatures")
	 (setf (ligatures (state interface)) nil))
	((string= value "Hyphenation")
	 (setf (hyphenation (state interface)) nil)))
  (typeset interface))

(defun set-disposition (value interface)
  (setf (disposition (state interface))
	;; #### FIXME: this is not satisfactory. Value should be the symbol
	;; directly.
	(cond ((string= value "Flush Left") :flush-left)
	      ((string= value "Flush Right") :flush-right)
	      ((string= value "Centered") :centered)
	      ((string= value "Justified") :justified)))
  (typeset interface))

(defun set-text (pane point old-length new-length
		 &aux (interface (element-interface pane)))
  (declare (ignore point old-length new-length))
  (setf (text (state interface)) (editor-pane-text pane))
  (typeset interface))

(defun render (pane x y width height
		    &aux (interface (element-interface pane))
			 (state (state interface))
			 (paragraph (paragraph state)))
  (declare (ignore x y width height))
  (unless (zerop (length paragraph))
    (gp:draw-character pane (aref paragraph 0) 50 50)))

(define-interface teap ()
  ((state :initform (make-instance 'state) :reader state))
  (:panes
   (features check-button-panel
	     :layout-class 'column-layout
	     :title "Features" :title-position :frame
	     :items '("Kerning" "Ligatures" "Hyphenation")
	     :selection-callback 'set-feature
	     :retract-callback 'unset-feature)
   (disposition radio-button-panel
		:layout-class 'column-layout
		:title "Disposition" :title-position :frame
		:items '("Flush Left" "Flush Right" "Centered" "Justified")
		:selection-callback 'set-disposition)
   (source editor-pane
	   :title "Source text" :title-position :frame
	   :text +initial-text+
	   :visible-min-width '(character 80)
	   :visible-min-height '(character 15)
	   :change-callback 'set-text)
   (rendering output-pane
	      :title "Rendered text" :title-position :frame
	      :visible-min-width 800
	      :visible-min-height 300
	      :display-callback 'render
	      :reader rendering))
  (:layouts (main column-layout '(options source rendering))
	    (options row-layout '(features disposition)))
  (:default-initargs :title "Typesetting Experimental Algorithms Platform"))


;; ===========
;; Entry Point
;; ===========

(defmacro run () (display (make-instance 'teap)))

;;; teap.lisp ends here
