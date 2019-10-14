;;; teap.lisp --- Typesetting Experimental Algorithms Platform

(in-package :teap)


(defconstant +initial-text+
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defconstant +font-file+
  #p"/usr/local/texlive/2019/texmf-dist/fonts/tfm/adobe/times/ptmr.tfm")


(defun update (interface &aux (state (state interface)))
  (render state)
  (gp:invalidate-rectangle (paragraph-pane interface)))


(defun set-feature (value interface)
  ;; #### FIXME: this is not satisfactory. Value should be the symbol
  ;; directly.
  (cond ((string= value "Kerning")
	 (setf (kerning (state interface)) t))
	((string= value "Ligatures")
	 (setf (ligatures (state interface)) t))
	((string= value "Hyphenation")
	 (setf (hyphenation (state interface)) t)))
  (update interface))

(defun unset-feature (value interface)
  ;; #### FIXME: this is not satisfactory. Value should be the symbol
  ;; directly.
  (cond ((string= value "Kerning")
	 (setf (kerning (state interface)) nil))
	((string= value "Ligatures")
	 (setf (ligatures (state interface)) nil))
	((string= value "Hyphenation")
	 (setf (hyphenation (state interface)) nil)))
  (update interface))

(defun set-disposition (value interface)
  (setf (disposition (state interface))
	;; #### FIXME: this is not satisfactory. Value should be the symbol
	;; directly.
	(cond ((string= value "Flush Left") :flush-left)
	      ((string= value "Flush Right") :flush-right)
	      ((string= value "Centered") :centered)
	      ((string= value "Justified") :justified)))
  (update interface))

(defun set-text (pane point old-length new-length
		 &aux (interface (top-level-interface pane)))
  (declare (ignore point old-length new-length))
  (setf (text (state interface)) (editor-pane-text pane))
  (update interface))

(defun render-paragraph (pane x y width height
			 &aux (interface (top-level-interface pane))
			      (state (state interface))
			      (paragraph (paragraph state)))
  (declare (ignore x y width height))
  (unless (zerop (length paragraph))
    (gp:with-graphics-scale (pane 3 3)
      (loop :for char-box :across paragraph
	    :do (gp:draw-character pane (code-char (char-box-char char-box))
				   (char-box-x char-box) 10)))))

(define-interface teap ()
  ((state :initform (make-instance 'state :font (tfm:load-font +font-file+))
	  :reader state))
  (:panes
   (disposition-pane radio-button-panel
     :layout-class 'column-layout
     :title "Disposition" :title-position :frame
     :items '("Flush Left" "Flush Right" "Centered" "Justified")
     :selection-callback 'set-disposition)
   (features-pane check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :items '("Kerning" "Ligatures" "Hyphenation")
     :selection-callback 'set-feature
     :retract-callback 'unset-feature)
   (text-pane editor-pane
     :title "Source text" :title-position :frame
     :visible-min-width '(character 80)
     :visible-min-height '(character 15)
     :change-callback 'set-text
     :reader text-pane)
   (paragraph-pane output-pane
     :title "Typeset paragraph" :title-position :frame
     :font (gp:make-font-description :family "Times" :slant :roman
				     :weight :medium :size 10)
     :visible-min-width 800
     :visible-min-height 300
     :display-callback 'render-paragraph
     :reader paragraph-pane))
  (:layouts
   (main-layout column-layout '(options-and-text-layout paragraph-pane))
   (options-and-text-layout row-layout '(options-layout text-pane))
   (options-layout column-layout '(disposition-pane features-pane)))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))

(defmethod interface-display :before ((teap teap))
  (setf (editor-pane-text (text-pane teap)) +initial-text+))



;; ===========
;; Entry Point
;; ===========

(defun run () (display (make-instance 'teap)))

;;; teap.lisp ends here
