;;; etap.lisp --- Typesetting Experimental Algorithms Platform

(in-package :etap)


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
  (gp:invalidate-rectangle (typeset-paragraph interface)))


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

(defun set-paragraph-width (pane value status
			    &aux (interface (top-level-interface pane)))
  (setf (titled-object-title pane)
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  value (/ value 28.452755)))
  (when (eq status :move)
    (setf (paragraph-width (state interface)) value)
    (update interface)))

(defun set-paragraph-zoom (pane value status
			    &aux (interface (top-level-interface pane)))
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Paragraph zoom: ~D%" value))
  (gp:invalidate-rectangle (typeset-paragraph interface)))

(defun |(un)set-clues| (value interface)
  (declare (ignore value))
  (gp:invalidate-rectangle (typeset-paragraph interface)))


(defun render-paragraph (pane x y width height
			 &aux (interface (top-level-interface pane))
			      (state (state interface))
			      (zoom (/ (range-slug-start
					(paragraph-zoom interface))
				       100))
			      (paragraph (paragraph state)))
  (declare (ignore x y width height))
  (unless (zerop (length paragraph))
    (gp:with-graphics-scale (pane zoom zoom)
      (loop :for char-box :across paragraph
	    :do (gp:draw-character pane (code-char (char-box-char char-box))
				   (char-box-x char-box) 10)))))

(define-interface etap ()
  ((state :initform (make-instance 'state :font (tfm:load-font +font-file+))
	  :reader state))
  (:panes
   (disposition radio-button-panel
     :layout-class 'column-layout
     :title "Disposition" :title-position :frame
     :items '("Flush Left" "Flush Right" "Centered" "Justified")
     :selection-callback 'set-disposition)
   (features check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :items '("Kerning" "Ligatures" "Hyphenation")
     :selection-callback 'set-feature
     :retract-callback 'unset-feature)
   (paragraph-width slider
     :title "Paragraph width: 284pt (10cm)"
     :orientation :horizontal
     :start 142 ;; 142.26378pt = 5cm
     :end 569 ;; 569.0551pt = 20cm
     :slug-start 284 ;; 284.52756pt = 10cm
     :tick-frequency 0
     :callback 'set-paragraph-width)
   (paragraph-zoom slider
     :title "Paragraph zoom: 100%"
     :orientation :horizontal
     :start 100
     :end 999
     :slug-start 100
     :tick-frequency 0
     :callback 'set-paragraph-zoom
     :reader paragraph-zoom)
   (clues check-button-panel
     :layout-class 'column-layout
     :title "Clues" :title-position :frame
     :items '("Paragraph box" "Line boxes" "Character boxes" "Baselines")
     :selection-callback '|(un)set-clues|
     :retract-callback '|(un)set-clues|
     :reader clues)
   (source-text editor-pane
     :title "Source text" :title-position :frame
     :visible-min-width '(character 80)
     :visible-max-width '(character 80)
     :visible-min-height '(character 15)
     :change-callback 'set-text
     :reader source-text)
   (typeset-paragraph output-pane
     :title "Typeset paragraph" :title-position :frame
     :font (gp:make-font-description :family "Times" :slant :roman
				     :weight :medium :size 10)
     :visible-min-width 850
     :visible-min-height 350
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'render-paragraph
     :reader typeset-paragraph))
  (:layouts
   (main column-layout '(configuration typeset-paragraph))
   (configuration row-layout '(options-1 options-2 source-text))
   (options-1 column-layout '(disposition features))
   (options-2 column-layout '(paragraph-width paragraph-zoom clues)))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))

(defmethod interface-display :before ((etap etap))
  (setf (editor-pane-text (source-text etap)) +initial-text+))



;; ===========
;; Entry Point
;; ===========

(defun run () (display (make-instance 'etap)))

;;; etap.lisp ends here
