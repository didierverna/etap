(in-package :etap)


(defun keyword-capitalize (keyword)
  (nsubstitute #\Space #\- (string-capitalize keyword)))


(defun update (interface &aux (state (state interface)))
  (setf (paragraph interface)
	(create-paragraph
	 (apply #'lineup
	   (text state) (font state) (hyphenation-rules state) (features state))
	 (paragraph-width state)
	 (disposition state)
	 (algorithm state)))
  (gp:invalidate-rectangle (view interface)))


(defun set-fixed-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (state interface))
	`(:fixed
	  :variant ,(choice-selected-item (fixed-variant interface))
	  ,@(apply #'append (choice-selected-items (fixed-options interface)))))
  (update interface))

(defun set-fit-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (state interface))
	`(:fit
	  :variant ,(choice-selected-item (fit-variant interface))
	  ,@(apply #'append (choice-selected-items (fit-options interface)))))
  (update interface))

(defun set-barnett-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (state interface))
	`(:barnett
	  ,@(apply #'append
	      (choice-selected-items (barnett-options interface)))))
  (update interface))

(defun set-duncan-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (state interface))
	`(:duncan
	  ,@(apply #'append
	      (choice-selected-items (duncan-options interface)))))
  (update interface))

(defun set-knuth-plass-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (state interface))
	`(:knuth-plass
	  ,@(apply #'append
	      (choice-selected-items (knuth-plass-options interface)))))
  (update interface))

(defun set-algorithm (value interface)
  (case (car value)
    (:fixed (set-fixed-algorithm value interface))
    (:fit (set-fit-algorithm value interface))
    (:barnett (set-barnett-algorithm value interface))
    (:duncan (set-duncan-algorithm value interface))
    (:knuth-plass (set-knuth-plass-algorithm value interface))))


(defun set-disposition (value interface)
  (declare (ignore value))
  (setf (disposition (state interface))
	`(,(choice-selected-item (disposition interface))
	  ,@(apply #'append
	      (choice-selected-items (disposition-options interface)))))
  (update interface))


(defun set-features (value interface)
  (declare (ignore value))
  (setf (features (state interface))
	(apply #'append (choice-selected-items (features interface))))
  (update interface))


(defun set-text (pane point old-length new-length
		 &aux (interface (top-level-interface pane)))
  (declare (ignore point old-length new-length))
  (setf (text (state interface)) (editor-pane-text pane))
  (update interface))


(defun set-paragraph-width
    (pane value status &aux (interface (top-level-interface pane)))
  (declare (ignore status))
  (setf (titled-object-title pane)
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  value (/ value 28.452755)))
  (setf (paragraph-width (state interface)) value)
  (update interface))


(defun set-zoom (pane value status)
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Paragraph zoom: ~D%" value))
  (gp:invalidate-rectangle (view (top-level-interface pane))))


(defun |(un)set-clues| (value interface)
  (declare (ignore value))
  (gp:invalidate-rectangle (view interface)))


(defun render-paragraph
    (pane x y width height
     &aux (interface (top-level-interface pane))
	  (state (state interface))
	  (paragraph (paragraph interface))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (clues (choice-selected-items (clues interface))))
  (declare (ignore x y width height))
  (when (pinned-lines paragraph)
    (set-horizontal-scroll-parameters pane
      :max-range (+ (* (width paragraph) zoom) 40))
    (set-vertical-scroll-parameters pane
      :max-range (+ (* (+ (height paragraph) (depth paragraph)) zoom) 40))
    (gp:with-graphics-translation (pane 20 20)
      (gp:with-graphics-scale (pane zoom zoom)
	(when (member :paragraph-box clues)
	  (gp:draw-rectangle pane
	      0 0 (width paragraph) (+ (height paragraph) (depth paragraph))
	    :foreground :red))
	(loop :with par-y := (height (first (pinned-lines paragraph)))
	      :for pinned-line :in (pinned-lines paragraph)
	      :for x := (x pinned-line)
	      :for y := (+ par-y (y pinned-line))
	      :when (member :line-boxes clues)
		:do (gp:draw-rectangle pane
			x
			(- y (height pinned-line))
			(width pinned-line)
			(+ (height pinned-line) (depth pinned-line))
		      :foreground :blue)
	      :when (member :over/underfull-boxes clues)
		:if (> (width pinned-line) (width paragraph))
		  :do (gp:draw-rectangle pane
			  (+ x (width pinned-line) 5)
			  (- y (height pinned-line))
			  5
			  (+ (height pinned-line) (depth pinned-line))
			:foreground :orange :filled t)
		:else :if (and (eq (car (disposition state)) :justified)
			       (< (width pinned-line) (width paragraph)))
			:do (gp:draw-rectangle pane
				(+ x (width pinned-line) 5)
				(- y (height pinned-line))
				5
				(+ (height pinned-line) (depth pinned-line))
			      :foreground :orange)
	      :when (member :baselines clues)
		:do (gp:draw-line pane x y (+ x (width pinned-line)) y
		      :foreground :purple)
	      :when (or (member :characters clues)
			(member :character-boxes clues))
		:do (mapc (lambda (object)
			    (cond ((pinned-character-p object)
				   (when (member :character-boxes clues)
				     (gp:draw-rectangle pane
					 (+ x (x object))
					 (- y (height object))
					 (width object)
					 (+ (height object)
					    (depth object))))
				   (when (member :characters clues)
				     (gp:draw-character pane
					 (cadr
					  (assoc
					   (elt +lm-ec-encoding+
						(tfm:code
						 (character-metrics object)))
					   +glyph-list+))
					 (+ x (x object))
					 y)))
				  ((pinned-hyphenation-clue-p object)
				   (when (member :hyphenation-points clues)
				     (gp:draw-polygon pane
				       (list (+ x (x object)) y
					     (+ x (x object) -3) (+ y 5)
					     (+ x (x object) +3) (+ y 5))
				       :filled t
				       :foreground :orange)))))
		      (pinned-characters  (line pinned-line))))))))


(defun show-help (interface pane type key)
  (declare (ignore interface pane))
  (case type
    (:tooltip
     (typecase key
       (symbol
	(case key
	  (:fixed-variant-underfull "Always prefer underfull lines.")
	  (:fixed-variant-best
	   "Prefer lines closer to the paragraph
width, whether underfull or overfull.")
	  (:fixed-variant-overfull "Always prefer overfull lines.")
	  (:fixed-option-avoid-hyphens "Avoid hyphenating words when possible.")
	  (:fixed-option-prefer-overfull-lines
	   "For the Best variant, when the underfull and overfull
lines are equally distant from the paragraph width,
choose the overfull rather than the underfull one.")
	  (:fit-variant-first "Prefer lines with fewer words (more stretch).")
	  (:fit-variant-best "Minimize scaling.")
	  (:fit-variant-last "Prefer lines with more words (more shrink).")
	  (:fit-option-avoid-hyphens "Avoid hyphenating words when possible.")
	  (:fit-option-relax
	   "For the First and Last variants, in ragged dispositions,
de-stretch or de-shrink lines afterwards.")
	  (:fit-option-prefer-shrink
	   "For the Best variant, in Justified disposition,
prefer shrinking over stretching when the
amount of scaling is the same.")
	  (:fit-option-prefer-overfull-lines
"For the Best variant, in Justified disposition,
when there is no perfect fit and the underfull and overfull
lines are equally distant from the paragraph width,
choose the overfull rather than the underfull one.")
	  (:disposition-option-sloppy
	   "In Justified disposition, stretch or shrink as needed,
ignoring the font's inter-word spacing boundaries.")))))))

(define-interface etap ()
  ((state :initform (make-state) :reader state)
   (paragraph :accessor paragraph))
  (:panes
   (algorithms tab-layout
     :title "Algorithms"
     :visible-max-width nil
     :combine-child-constraints t
     :items '((:fixed fixed-settings)
	      (:fit fit-settings)
	      (:barnett barnett-settings)
	      (:duncan duncan-settings)
	      (:knuth-plass knuth-plass-settings))
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :visible-child-function 'second
     :selection-callback 'set-algorithm
     :reader algorithms)
   (fixed-variant radio-button-panel
     :layout-class 'column-layout
     :title "Variant" :title-position :frame
     :items '(:underfull :best :overfull)
     :help-keys '(:fixed-variant-underfull :fixed-variant-best
		  :fixed-variant-overfull)
     :print-function 'keyword-capitalize
     :selection-callback 'set-fixed-algorithm
     :reader fixed-variant)
   (fixed-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '((:avoid-hyphens t) (:prefer-overfull-lines t))
     :help-keys '(:fixed-option-avoid-hyphens
		  :fixed-option-prefer-overfull-lines)
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-fixed-algorithm
     :retract-callback 'set-fixed-algorithm
     :reader fixed-options)
   (fit-variant radio-button-panel
     :layout-class 'column-layout
     :title "Variant" :title-position :frame
     :items '(:first :best :last)
     :help-keys '(:fit-variant-first :fit-variant-best :fit-variant-last)
     :print-function 'keyword-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-variant)
   (fit-options check-button-panel
     :layout-class 'grid-layout
     :layout-args '(:orientation :column)
     :title "Options" :title-position :frame
     :items '((:avoid-hyphens t) (:relax t)
	      (:prefer-shrink t) (:prefer-overfull-lines t))
     :help-keys '(:fit-option-avoid-hyphens :fit-option-relax
		  :fit-option-prefer-shrink :fit-option-prefer-overfull-lines)
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-fit-algorithm
     :retract-callback 'set-fit-algorithm
     :reader fit-options)
   (barnett-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '()
     :help-keys '()
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-barnett-algorithm
     :retract-callback 'set-barnett-algorithm
     :reader barnett-options)
   (duncan-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '()
     :help-keys '()
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-duncan-algorithm
     :retract-callback 'set-duncan-algorithm
     :reader duncan-options)
   (knuth-plass-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '()
     :help-keys '()
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-knuth-plass-algorithm
     :retract-callback 'set-knuth-plass-algorithm
     :reader knuth-plass-options)
   (disposition radio-button-panel
     :layout-class 'column-layout
     :title "Disposition" :title-position :frame
     :visible-max-width nil
     :items '(:flush-left :centered :flush-right :justified)
     :print-function 'keyword-capitalize
     :selection-callback 'set-disposition
     :reader disposition)
   (disposition-options check-button-panel
     :layout-class 'column-layout
     :title "Disposition Options" :title-position :frame
     :visible-max-width nil
     :items '((:sloppy t))
     :help-keys '(:disposition-option-sloppy)
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-disposition
     :retract-callback 'set-disposition
     :reader disposition-options)
   (features check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :visible-max-width nil
     :items '((:kerning t) (:ligatures t) (:hyphenation t))
     :print-function (lambda (item) (keyword-capitalize (car item)))
     :selection-callback 'set-features
     :retract-callback 'set-features
     :reader features)
   (paragraph-width slider
     :title "Paragraph width: 284pt (10cm)"
     :orientation :horizontal
     :start 142 ;; 142.26378pt = 5cm
     :end 569 ;; 569.0551pt = 20cm
     :tick-frequency 0
     :callback 'set-paragraph-width
     :reader paragraph-width)
   (zoom slider
     :title "Paragraph zoom: 100%"
     :orientation :horizontal
     :start 100
     :end 999
     :slug-start 100
     :tick-frequency 0
     :callback 'set-zoom
     :reader zoom)
   (clues check-button-panel
     :layout-class 'column-layout
     :title "Characters and Clues" :title-position :frame
     :visible-max-width nil
     :items '(:characters :hyphenation-points
	      :paragraph-box :line-boxes :character-boxes :baselines
	      :over/underfull-boxes)
     :selected-items '(:characters)
     :print-function 'keyword-capitalize
     :selection-callback '|(un)set-clues|
     :retract-callback '|(un)set-clues|
     :reader clues)
   (text editor-pane
     :title "Source text" :title-position :frame
     :visible-min-width '(character 80)
     ;;:visible-max-width '(character 80)
     :visible-min-height '(character 15)
     :visible-max-height '(character 30)
     :change-callback 'set-text
     :reader text)
   (view output-pane
     :title "Typeset paragraph" :title-position :frame
     :font (gp:make-font-description :family "Latin Modern Roman"
	     :weight :normal :slant :roman :size 10)
     :visible-min-width 850
     :visible-min-height 350
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'render-paragraph
     :reader view))
  (:layouts
   (main column-layout '(settings view))
   (settings row-layout '(configuration text))
   (configuration column-layout '(algorithms options)
     :visible-max-width t)
   (fixed-settings row-layout '(fixed-variant fixed-options))
   (fit-settings row-layout '(fit-variant fit-options))
   (barnett-settings row-layout '(barnett-options))
   (duncan-settings row-layout '(duncan-options))
   (knuth-plass-settings row-layout '(knuth-plass-options))
   (options row-layout '(options-1 options-2))
   (options-1 column-layout '(disposition disposition-options features)
     :visible-min-width 150
     :visible-max-width 150)
   (options-2 column-layout '(paragraph-width zoom clues)
     :visible-min-width 250
     :visible-max-width 250))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))

(defmethod interface-display :before ((etap etap) &aux (state (state etap)))
  (let ((algorithm (algorithm state)))
    (case (car algorithm)
      (:fixed
       (setf (choice-selection (algorithms etap)) 0)
       (setf (choice-selected-item (fixed-variant etap))
	     (or (cadr (member :variant algorithm)) :underfull))
       (setf (choice-selection (fixed-options etap))
	     (let ((selection (list)))
	       (when (cadr (member :avoid-hyphens algorithm))
		 (push 0 selection))
	       (when (cadr (member :prefer-overfull-lines algorithm))
		 (push 1 selection))
	       selection)))
      (:fit
       (setf (choice-selection (algorithms etap)) 1)
       (setf (choice-selected-item (fit-variant etap))
	     (or (cadr (member :variant algorithm)) :first))
       (setf (choice-selection (fit-options etap))
	     (let ((selection (list)))
	       (when (cadr (member :relax algorithm))
		 (push 0 selection))
	       (when (cadr (member :sloppy algorithm))
		 (push 1 selection))
	       (when (cadr (member :avoid-hyphens algorithm))
		 (push 2 selection))
	       (when (cadr (member :prefer-shrink algorithm))
		 (push 3 selection))
	       selection)))
      (:barnett
       (setf (choice-selection (algorithms etap)) 2)
       (setf (choice-selection (barnett-options etap))
	     (let ((selection (list)))
	       (when (cadr (member :sloppy algorithm))
		 (push 0 selection))
	       selection)))
      (:duncan
       (setf (choice-selection (algorithms etap)) 3)
       (setf (choice-selection (duncan-options etap))
	     (let ((selection (list)))
	       (when (cadr (member :sloppy algorithm))
		 (push 0 selection))
	       selection)))
      (:knuth-plass
       (setf (choice-selection (algorithms etap)) 4)
       (setf (choice-selection (knuth-plass-options etap))
	     (let ((selection (list)))
	       (when (cadr (member :sloppy algorithm))
		 (push 0 selection))
	       selection)))))
  (setf (choice-selected-item (disposition etap)) (car (disposition state)))
  (let ((options (cdr (disposition state))))
    (setf (choice-selection (disposition-options etap))
	  (let ((selection (list)))
	    (when (cadr (member :sloppy options)) (push 0 selection))
	    selection)))
  (let ((features (features state)))
    (setf (choice-selection (features etap))
	  (let ((selection (list)))
	    (when (cadr (member :kerning features)) (push 0 selection))
	    (when (cadr (member :ligatures features)) (push 1 selection))
	    (when (cadr (member :hyphenation features)) (push 2 selection))
	    selection)))
  (setf (range-slug-start (paragraph-width etap)) (paragraph-width state))
  (setf (editor-pane-text (text etap)) (text state)))



;; ===========
;; Entry Point
;; ===========

(defun run ()
  (display (make-instance 'etap :help-callback 'show-help)))
