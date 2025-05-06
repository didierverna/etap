(in-package :etap)

;; =========
;; Utilities
;; =========

(defmacro define-gui-caliber (name min default max)
  "Define a NAMEd GUI caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber gui ,name ,min ,default ,max))

(define-gui-caliber zoom 100 100 999)


(defmacro calibrate-gui (name)
  "Calibrate NAMEd GUI variable."
  `(calibrate gui ,name :earmuffs nil))


(defun remake-rivers
    (interface
     &aux (rivers-interface (rivers-interface interface))
	  (layout (layout interface)))
  "Remake INTERFACE's rivers."
  (setf (rivers interface)
	(when (and (button-selected
		    (rivers-detection (rivers-interface interface)))
		   (not (zerop layout)))
	  (detect-rivers
	   (get-layout (1- layout) (breakup (paragraph interface)))
	   (range-slug-start (rivers-angle rivers-interface))))))

(defun remake-paragraph (interface)
  "Remake INTERFACE's paragaph."
  (let* ((paragraph (make-paragraph :context (context interface)))
	 (layouts-# (layouts-# (breakup paragraph))))
    (setf (paragraph interface) paragraph)
    (setf (layout interface) (if (zerop layouts-#) 0 1))
    (setf (titled-object-title (view interface))
	  (format nil "Layout ~D/~D" (layout interface) layouts-#))))

(defun update (interface)
  "Update INTERFACE.
This remakes INTERFACE's paragraph and everything that depends on it,
and invalidates the view."
  (remake-paragraph interface)
  (remake-rivers interface)
  (gp:invalidate-rectangle (view interface)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun title-capitalize (title)
    "Capitalize TITLE and substitute dashes with spaces."
    (nsubstitute #\Space #\- (string-capitalize title))))


;; -------
;; Sliders
;; -------

(defclass etap-slider (slider)
  ((property-name :documentation "This slider's property name."
		  :reader property-name)
   (context-updater :documentation "This slider's context updater function."
		    :reader context-updater))
  (:default-initargs
   :title "Dummy" ;; #### FIXME: without this, the sliders don't appear.
   :tick-frequency 0
   :orientation :horizontal
   :visible-min-width 220
   :callback 'etap-slider-callback)
  (:documentation "The ETAP Slider Class."))

(defclass etap-dimen-slider (etap-slider)
  ()
  (:documentation "The ETAP Dimension Slider Class."))


(defgeneric etap-slider-title (slider)
  (:documentation "Compute ETAP SlIDER's title based on its current value.")
  (:method ((slider etap-slider))
    "Advertise ETAP SLIDER's value in its title. This is the default method."
    (concatenate 'string
      (property-name slider) ": "
      (write-to-string (range-slug-start slider))))
  (:method ((slider etap-dimen-slider) &aux (value (range-slug-start slider)))
    "Advertise ETAP dimension SLIDER's value in its title, in pt and cm."
    (concatenate 'string
      (property-name slider) ": "
      (write-to-string value)
      "pt ("
      (write-to-string (float (/ value 28.452755)))
      "cm)")))

(defun update-etap-slider-title (slider)
  "Update ETAP SLIDER's title."
  (setf (titled-object-title slider) (etap-slider-title slider)))


(defmethod initialize-instance :after
    ((slider etap-slider) &key algorithm property)
  "Post-initialize SLIDER."
  (setf (slot-value slider 'property-name)
	(title-capitalize property))
  (setf (slot-value slider 'context-updater)
	(symbol-function (intern (concatenate 'string
				   (symbol-name algorithm)
				   "-UPDATE-CONTEXT")
				 :etap)))
  (let ((caliber (symbol-value (intern (concatenate 'string
					 "*"
					 (symbol-name algorithm)
					 "-"
					 (symbol-name property)
					 "*")
				       :etap))))
    (setf (range-start slider)      (caliber-min caliber)
	  (range-end slider)        (caliber-max caliber)
	  (range-slug-start slider) (caliber-default caliber))
    (update-etap-slider-title slider)))


;; #### NOTE: apparently, slider callbacks signature cannot be changed.
(defun etap-slider-callback
    (slider value status &aux (interface (top-level-interface slider)))
  "Handle SLIDER's value change."
  (declare (ignore value status))
  (update-etap-slider-title slider)
  (funcall (context-updater slider) nil interface)
  (update interface))


(defmacro slider-setting (algorithm property interface)
  "Return (:PROPERTY (RANGE-SLUG-START (ALGORITHM-PROPERTY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name property)))))
    `(list ,property (range-slug-start (,accessor ,interface)))))


;; -------------------
;; Radio Button Panels
;; -------------------

(defclass etap-radio-button-panel (radio-button-panel)
  ((context-updater
    :documentation "This radio button panel's context updater function."
    :reader context-updater))
  (:default-initargs
   :title "Dummy" ;; #### FIXME: without this, the panels don't appear.
   :title-position :frame
   :layout-class 'column-layout
   :visible-max-height nil
   :print-function 'title-capitalize
   :callback-type '(:element :interface)
   :selection-callback 'etap-radio-button-panel-selection-callback)
  (:documentation "The ETAP Radio Button Panel Class."))

(defmethod initialize-instance :after
    ((panel etap-radio-button-panel) &key algorithm property plural)
  "Post-initialize ETAP radio button PANEL."
  (setf (slot-value panel 'context-updater)
	(symbol-function (intern (concatenate 'string
				   (symbol-name algorithm)
				   "-UPDATE-CONTEXT")
				 :etap)))
  (setf (collection-items panel)
	(symbol-value (intern (concatenate 'string
				"*"
				(symbol-name algorithm)
				"-"
				(symbol-name property)
				(ecase plural
				  (:ies "IES")
				  (:es "ES")
				  ((:s nil) "S"))
				"*")
			      :etap)))
  (setf (titled-object-title panel) (title-capitalize property)))


(defun etap-radio-button-panel-selection-callback (panel interface)
  "Handle ETAP radio button PANEL's value change."
  (funcall (context-updater panel) nil interface)
  (update interface))


(defmacro radio-setting (algorithm property interface)
  "Return (:PROPERTY (CHOICE-SELECTED-ITEM (ALGORITHM-PROPERTY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name property)))))
    `(list ,property (choice-selected-item (,accessor ,interface)))))



;; -------------------
;; Check Button Panels
;; -------------------

(defclass etap-check-button-panel (check-button-panel)
  ((context-updater
    :documentation "This check button panel's context updater function."
    :reader context-updater))
  (:default-initargs
   :title "Dummy" ;; #### FIXME: without this, the panels don't appear.
   :title-position :frame
   :layout-class 'column-layout
   :visible-max-height nil
   :print-function (lambda (item) (title-capitalize (car item)))
   :callback-type '(:element :interface)
   :selection-callback 'etap-check-button-panel-callback
   :retract-callback   'etap-check-button-panel-callback)
  (:documentation "The ETAP Radio Button Panel Class."))

(defmethod initialize-instance :after
    ((panel etap-check-button-panel) &key algorithm properties)
  "Post-initialize ETAP check button PANEL."
  (setf (slot-value panel 'context-updater)
	(symbol-function (intern (concatenate 'string
				   (symbol-name algorithm)
				   "-UPDATE-CONTEXT")
				 :etap)))
  (setf (collection-items panel)
	(symbol-value (intern (concatenate 'string
				"*"
				(symbol-name algorithm)
				"-"
				(symbol-name properties)
				"*")
			      :etap)))
  (setf (titled-object-title panel) (title-capitalize properties)))


(defun etap-check-button-panel-callback (panel interface)
  "Handle ETAP check button PANEL's value change."
  (funcall (context-updater panel) nil interface)
  (update interface))



;; =================
;; Interface Actions
;; =================

;; ----------
;; Algorithms
;; ----------

;; Fixed
(defun fixed-update-context (value interface)
  "Set the current algorithm to Fixed in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :fixed
	      (apply #'append
		(radio-setting fixed :fallback interface)
		(slider-setting fixed :width-offset interface)
		(choice-selected-items (fixed-options interface))))))

;; Fit
(defun fit-update-context (value interface)
  "Set the current algorithm to Fit in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :fit
	      (apply #'append
		(radio-setting fit :variant interface)
		(radio-setting fit :fallback interface)
		(radio-setting fit :discriminating-function interface)
		(slider-setting fit :line-penalty interface)
		(slider-setting fit :hyphen-penalty interface)
		(slider-setting fit :explicit-hyphen-penalty interface)
		(slider-setting fit :width-offset interface)
		(choice-selected-items (fit-options interface))))))

;; Barnett
(defun barnett-update-context (value interface)
  "Set the current algorithm to Barnett in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface)) '(:barnett)))

;; Duncan
(defun duncan-update-context (value interface)
  "Set the current algorithm to Duncan in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :duncan
	      (radio-setting duncan :discriminating-function interface))))

;; Knuth-Plass
(defun kp-update-context (value interface)
  "Set the current algorithm to Knuth-Plass in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :knuth-plass
	      (append
	       (radio-setting kp :variant interface)
	       (slider-setting kp :line-penalty interface)
	       (slider-setting kp :hyphen-penalty interface)
	       (slider-setting kp :explicit-hyphen-penalty interface)
	       (slider-setting kp :adjacent-demerits interface)
	       (slider-setting kp :double-hyphen-demerits interface)
	       (slider-setting kp :final-hyphen-demerits interface)
	       (slider-setting kp :pre-tolerance interface)
	       (slider-setting kp :tolerance interface)
	       (slider-setting kp :emergency-stretch interface)
	       (slider-setting kp :looseness interface)))))

;; KPX
(defun kpx-update-context (value interface)
  "Set the current algorithm to KPX in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :kpx
	      (append
	       (radio-setting kpx :variant interface)
	       (radio-setting kpx :fitness interface)
	       (slider-setting kpx :line-penalty interface)
	       (slider-setting kpx :hyphen-penalty interface)
	       (slider-setting kpx :explicit-hyphen-penalty interface)
	       (slider-setting kpx :adjacent-demerits interface)
	       (slider-setting kpx :double-hyphen-demerits interface)
	       (slider-setting kpx :final-hyphen-demerits interface)
	       (slider-setting kpx :similar-demerits interface)
	       (slider-setting kpx :pre-tolerance interface)
	       (slider-setting kpx :tolerance interface)
	       (slider-setting kpx :emergency-stretch interface)
	       (slider-setting kpx :looseness interface)))))


(defun set-algorithm (value interface)
  "Select algorithm specified by VALUE in INTERFACE."
  (case (car value)
    (:fixed (fixed-update-context value interface))
    (:fit (fit-update-context value interface))
    (:barnett (barnett-update-context value interface))
    (:duncan (duncan-update-context value interface))
    (:knuth-plass (kp-update-context value interface))
    (:kpx (kpx-update-context value interface)))
  (update interface))


;; Other actions

(defun set-disposition (value interface)
  "Set the current disposition in INTERFACE's context."
  (declare (ignore value))
  (setf (disposition (context interface))
	`(,(choice-selected-item (disposition interface))
	  ,@(apply #'append
	      (choice-selected-items (disposition-options-panel interface)))))
  (update interface))

(defun set-features (value interface)
  "Set the current features in INTERFACE's context."
  (declare (ignore value))
  (setf (features (context interface))
	(apply #'append (choice-selected-items (features interface))))
  (update interface))

(defun set-text (pane point old-length new-length
		 &aux (interface (top-level-interface pane)))
  "Set editor PANE's current text in PANE's context."
  (declare (ignore point old-length new-length))
  (setf (text (nlstring (context interface))) (editor-pane-text pane))
  (update interface))

(defun set-paragraph-width
    (pane value status &aux (interface (top-level-interface pane)))
  "Set the current paragraph width to VALUE in PANE's context."
  (declare (ignore status))
  (setf (titled-object-title pane)
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  value (/ value 28.452755)))
  (setf (paragraph-width (context interface)) value)
  (update interface))

(defun set-zoom (pane value status)
  "Set PANE's zooming to to VALUE."
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Paragraph zoom: ~3D%" value))
  (gp:invalidate-rectangle (view (top-level-interface pane))))

(defun set-clues (value interface)
  "Invalidate INTERFACE's view after a change to the clues."
  (declare (ignore value))
  (gp:invalidate-rectangle (view interface)))

(defun render-view
    (pane x y width height
     &aux (interface (top-level-interface pane))
	  (paragraph (paragraph interface))
	  (layout-# (layout interface))
	  (layout (unless (zerop layout-#)
		    (get-layout (1- layout-#) (breakup paragraph))))
	  (par-y (height layout))
	  (par-h+d (+ par-y (depth layout)))
	  (rivers (rivers interface))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (clues (choice-selected-items (clues interface))))
  "Render PANE's view, including paragraph, clues, etc."
  (declare (ignore x y width height))
  (set-horizontal-scroll-parameters pane
    :max-range (+ (* (width paragraph) zoom) 40))
  (set-vertical-scroll-parameters pane
    :max-range (+ (* par-h+d zoom) 40))
  (gp:with-graphics-translation (pane 20 20)
    (gp:with-graphics-scale (pane zoom zoom)
      (when (member :paragraph-box clues)
	(gp:draw-rectangle pane 0 0 (width paragraph) par-h+d
	  :foreground :red
	  :scale-thickness nil))
      (when layout
	(loop :for rest :on (lines layout)
	      :for line := (car rest)
	      :for x := (x line)
	      :for y := (+ par-y (y line))
	      :when (member :line-boxes clues)
		:do (gp:draw-rectangle pane
			x
			(- y (height line))
			(width line)
			(+ (height line) (depth line))
		      :foreground :blue
		      :scale-thickness nil)
	      :when (member :over/underfull-boxes clues)
		:if (> (width line) (width paragraph))
		  :do (gp:draw-rectangle pane
			  (+ x (width line) 5)
			  (- y (height line))
			  5
			  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr rest) ;; not the last one
			       (eq (disposition-type (disposition paragraph))
				   :justified)
			       (< (width line) (width paragraph)))
		  :do (gp:draw-rectangle pane
			  (+ (width paragraph) 5)
			  (- y (height line))
			  5
			  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if ($< (effective-scale line) (scale line))
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(- y (height line))
				(+ (width paragraph) 11)
				(- y (height line))
				(+ (width paragraph) 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled t :closed t)
		:else :if ($< (scale line) -1)
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(- y (height line))
				(+ (width paragraph) 11)
				(- y (height line))
				(+ (width paragraph) 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled nil :closed t)
		:else :if ($> (effective-scale line) (scale line))
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(+ y (depth line))
				(+ (width paragraph) 11)
				(+ y (depth line))
				(+ (width paragraph) 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($> (scale line) 1)
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(+ y (depth line))
				(+ (width paragraph) 11)
				(+ y (depth line))
				(+ (width paragraph) 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled nil :closed t)
	      :when (member :baselines clues)
		:do (gp:draw-line pane x y (+ x (width line)) y
		      :foreground :purple
		      :scale-thickness nil)
	      :when (or (member :characters clues)
			(member :character-boxes clues))
		:do (mapc (lambda (item)
			    (cond ((typep (object item)
					  'tfm:character-metrics)
				   (when (member :character-boxes clues)
				     (gp:draw-rectangle pane
					 (+ x (x item))
					 (- y (height item))
					 (width item)
					 (+ (height item)
					    (depth item))
				       :scale-thickness nil))
				   (when (member :characters clues)
				     (gp:draw-character pane
					 (aref *lm-ec*
					       (tfm:code (object item)))
					 (+ x (x item))
					 y)))
				  ((member (object item)
					   '(:explicit-hyphenation-clue
					     :hyphenation-clue))
				   (when (member :hyphenation-points clues)
				     (gp:draw-polygon pane
				       (list (+ x (x item)) y
					     (+ x (x item) -3) (+ y 5)
					     (+ x (x item) +3) (+ y 5))
				       :filled t
				       :foreground (if (eq (object item)
							   :hyphenation-clue)
						     :orange
						     :blue))))
				  ((whitespacep item)
				   (when (member :river-beds clues)
				     (gp:draw-circle
				      pane
				      (+ x (x item) (/ (width item) 2)) y 1
				      :filled t :foreground :red)))))
		      (items line)))
	;; #### FIXME: see PIN-LINE comment about the beds boards.
	(when (and (button-selected
		    (rivers-detection (rivers-interface interface)))
		   rivers)
	  (maphash (lambda (source arms)
		     (mapc (lambda (arm &aux (mouth (mouth arm)))
			     (gp:draw-line pane
				 (+ (x (board source))
				    (x source)
				    (/ (width source) 2))
				 (+ par-y (y (board source)) (y source))
				 (+ (x (board mouth))
				    (x mouth)
				    (/ (width mouth) 2))
				 (+ par-y (y (board mouth)) (y mouth))
			       :foreground :red :scale-thickness nil))
		       arms))
		   rivers))))))


;; Layouts

(defun next-layout
    (op interface
     &aux (layouts-# (layouts-# (breakup (paragraph interface))))
	  (layout (layout interface)))
  "Select the next OP layout."
  (unless (zerop layouts-#)
    (setq layout (1+ (mod (1- (funcall op layout)) layouts-#)))
    (setf (layout interface) layout)
    (when (button-selected (rivers-detection (rivers-interface interface)))
      (remake-rivers interface))
    (setf (titled-object-title (view interface))
	  (format nil "Layout ~D/~D" layout layouts-#))
    (gp:invalidate-rectangle (view interface))))


;; Tooltips

(defparameter *interface-tooltips*
  '(:layout-1 "Display previous layout."
    :layout+1 "Display next layout."))

(defparameter *tooltips*
  `(,@*interface-tooltips*
    ,@*fixed-tooltips*
    ,@*fit-tooltips*
    ,@*duncan-tooltips*
    ,@*kp-tooltips*
    ,@*kpx-tooltips*
    ,@*disposition-options-tooltips*)
  "The GUI's tooltips.")

(defun show-help (interface pane type key)
  "The GUI's help callback."
  (declare (ignore interface pane))
  (case type
    (:tooltip
     (typecase key
       (symbol (cadr (member key *tooltips*)))))))

;; Properties
(defun display-properties
    (pane x y
     &aux (interface (top-level-interface pane))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (paragraph (paragraph interface))
	  (layout-# (1- (layout interface)))
	  (layout (when (<= 0 layout-#)
		    (get-layout layout-# (breakup paragraph)))))
  "Display the properties of the paragraph, or the line clicked on."
  (when (member :properties-tooltips (choice-selected-items (clues interface)))
    (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
    (decf y (height layout))
    (if (and (<= x 0) (<= y (depth layout)))
      (display-tooltip pane
	:text (properties paragraph
		:layout-# (when (<= 0 layout-#) layout-#)))
      (let ((line (when (and layout (>= x 0) (<= x (width paragraph)))
		    (find-if (lambda (line)
			       (and (>= y (- (y line) (height line)))
				    (<= y (+ (y line) (depth line)))))
			     (lines layout)))))
	(if line
	  (display-tooltip pane :text (properties line))
	  (display-tooltip pane))))))

;; Rivers detection
(defun set-rivers-detection
    (value interface
     &aux (detectionp (button-selected value))
	  (main-interface (main-interface interface)))
  "Toggle rivers detection."
  (when (and detectionp (null (rivers main-interface)))
    (remake-rivers main-interface))
  (setf (simple-pane-enabled (rivers-angle interface)) detectionp)
  (gp:invalidate-rectangle (view main-interface)))

(defun set-rivers-angle
    (pane value status
     &aux (main-interface (main-interface (top-level-interface pane))))
  "Set the rivers detection angle threshold to VALUE in PANE's context."
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Angle: ~D°" value))
  (remake-rivers main-interface)
  (gp:invalidate-rectangle (view main-interface)))

(define-interface rivers-detection ()
  ((main-interface :reader main-interface))
  (:panes
   (rivers-detection check-button
     :text "Detect rivers"
     :selection-callback 'set-rivers-detection
     :retract-callback 'set-rivers-detection
     :callback-type :item-interface
     :reader rivers-detection)
   (rivers-angle slider
     :title "Angle: 0°"
     :orientation :horizontal
     :visible-min-width 250
     :visible-max-width 250
     :start 0
     :end 45
     :slug-start 0
     :tick-frequency 0
     :enabled nil
     :callback 'set-rivers-angle
     :reader rivers-angle))
  (:layouts
   (main column-layout
     '(rivers-detection rivers-angle)))
  (:default-initargs
   :title "Rivers Detection"
   :window-styles '(:always-on-top t :toolbox t)))

;; Menus
(defun tools-menu-callback (data interface)
  "Display the rivers interface." ;; Currently what the only button does.
  (declare (ignore data))
  (display (rivers-interface interface) :owner interface))

(defun text-menu-callback
    (data interface &aux (context (context interface)))
  "Reset the source text." ;; Currently what the only button does.
  (declare (ignore data))
  (setf (nlstring context) (make-nlstring :text *text* :language *language*))
  ;; #### NOTE: the language menu's selection is updated on pop-up.
  (setf (editor-pane-text (text interface)) (text context))
  (update interface))

(defun language-menu-callback (data interface)
  "Change the current text's language."
  (setf (language (nlstring (context interface))) data)
  (update interface))

(defun language-menu-popup-callback (component)
  "Update the language popup to the current language."
  (setf (choice-selection component)
	(position
	 (language (context (element-interface-for-callback component)))
	 *languages*
	 :key #'car)))

;; Interface
(define-interface etap ()
  ((context :initform *context* :initarg :context :reader context)
   (paragraph :accessor paragraph)
   (layout :initform 0 :accessor layout)
   (rivers :documentation "The paragraph's detected rivers."
	   :initform nil
	   :accessor rivers)
   (rivers-interface :initform (make-instance 'rivers-detection)
		     :reader rivers-interface))
  (:menus
   (tools-menu "Tools" (:rivers-detection)
     :print-function 'title-capitalize
     :callback 'tools-menu-callback)
   (text-menu nil ;; Ignore popup menu's title
    (:reset)
    :print-function 'title-capitalize
    :callback 'text-menu-callback)
   (language-menu nil ;; Ignore popup menu's title
     nil)) ;; The items will be created dynamically in INTERFACE-DISPLAY.
  (:menu-bar tools-menu)
  (:panes
   (algorithms tab-layout
     :title "Algorithms"
     :visible-max-width nil
     :combine-child-constraints t
     :items '((:fixed fixed-settings)
	      (:fit fit-settings)
	      (:barnett barnett-settings)
	      (:duncan duncan-settings)
	      (:knuth-plass kp-settings)
	      (:kpx kpx-settings))
     :print-function (lambda (item) (title-capitalize (car item)))
     :visible-child-function 'second
     :selection-callback 'set-algorithm
     :reader algorithms)
   (fixed-fallback etap-radio-button-panel
     :algorithm 'fixed
     :property 'fallback
     :help-keys *fixed-fallbacks-help-keys*
     :reader fixed-fallback)
   (fixed-options etap-check-button-panel
     :algorithm 'fixed
     :properties 'options
     :help-keys *fixed-options-help-keys*
     :reader fixed-options)
   (fixed-width-offset etap-dimen-slider
     :algorithm 'fixed
     :property 'width-offset
     :reader fixed-width-offset)
   (fit-variant etap-radio-button-panel
     :algorithm 'fit
     :property 'variant
     :help-keys *fit-variants-help-keys*
     :reader fit-variant)
   (fit-fallback etap-radio-button-panel
     :algorithm 'fit
     :property 'fallback
     :help-keys *fit-fallbacks-help-keys*
     :reader fit-fallback)
   (fit-discriminating-function etap-radio-button-panel
     :algorithm 'fit
     :property 'discriminating-function
     :help-keys *fit-discriminating-functions-help-keys*
     :reader fit-discriminating-function)
   (fit-options etap-check-button-panel
     :algorithm 'fit
     :properties 'options
     :help-keys *fit-options-help-keys*
     :reader fit-options)
   (fit-line-penalty etap-slider
     :algorithm 'fit
     :property 'line-penalty
     :reader fit-line-penalty)
   (fit-hyphen-penalty etap-slider
     :algorithm 'fit
     :property 'hyphen-penalty
     :reader fit-hyphen-penalty)
   (fit-explicit-hyphen-penalty etap-slider
     :algorithm 'fit
     :property 'explicit-hyphen-penalty
     :reader fit-explicit-hyphen-penalty)
   (fit-width-offset etap-dimen-slider
     :algorithm 'fit
     :property 'width-offset
     :reader fit-width-offset)
   (duncan-discriminating-function etap-radio-button-panel
     :algorithm 'duncan
     :property 'discriminating-function
     :help-keys *duncan-discriminating-functions-help-keys*
     :reader duncan-discriminating-function)
   (kp-variant etap-radio-button-panel
     :algorithm 'kp
     :property 'variant
     :help-keys *kp-variants-help-keys*
     :reader kp-variant)
   (kp-line-penalty etap-slider
     :algorithm 'kp
     :property 'line-penalty
     :reader kp-line-penalty)
   (kp-hyphen-penalty etap-slider
     :algorithm 'kp
     :property 'hyphen-penalty
     :reader kp-hyphen-penalty)
   (kp-explicit-hyphen-penalty etap-slider
     :algorithm 'kp
     :property 'explicit-hyphen-penalty
     :reader kp-explicit-hyphen-penalty)
   (kp-adjacent-demerits etap-slider
     :algorithm 'kp
     :property 'adjacent-demerits
     :reader kp-adjacent-demerits)
   (kp-double-hyphen-demerits etap-slider
     :algorithm 'kp
     :property 'double-hyphen-demerits
     :reader kp-double-hyphen-demerits)
   (kp-final-hyphen-demerits etap-slider
     :algorithm 'kp
     :property 'final-hyphen-demerits
     :reader kp-final-hyphen-demerits)
   (kp-pre-tolerance etap-slider
     :algorithm 'kp
     :property 'pre-tolerance
     :reader kp-pre-tolerance)
   (kp-tolerance etap-slider
     :algorithm 'kp
     :property 'tolerance
     :reader kp-tolerance)
   (kp-emergency-stretch etap-dimen-slider
     :algorithm 'kp
     :property 'emergency-stretch
     :reader kp-emergency-stretch)
   (kp-looseness etap-slider
     :algorithm 'kp
     :property 'looseness
     :reader kp-looseness)
   (kpx-variant etap-radio-button-panel
     :algorithm 'kpx
     :property 'variant
     :help-keys *kpx-variants-help-keys*
     :reader kpx-variant)
   (kpx-fitness etap-radio-button-panel
     :algorithm 'kpx
     :property 'fitness
     :plural :es
     :help-keys *kpx-fitnesses-help-keys*
     :reader kpx-fitness)
   (kpx-line-penalty etap-slider
     :algorithm 'kpx
     :property 'line-penalty
     :reader kpx-line-penalty)
   (kpx-hyphen-penalty etap-slider
     :algorithm 'kpx
     :property 'hyphen-penalty
     :reader kpx-hyphen-penalty)
   (kpx-explicit-hyphen-penalty etap-slider
     :algorithm 'kpx
     :property 'explicit-hyphen-penalty
     :reader kpx-explicit-hyphen-penalty)
   (kpx-adjacent-demerits etap-slider
     :algorithm 'kpx
     :property 'adjacent-demerits
     :reader kpx-adjacent-demerits)
   (kpx-double-hyphen-demerits etap-slider
     :algorithm 'kpx
     :property 'double-hyphen-demerits
     :reader kpx-double-hyphen-demerits)
   (kpx-final-hyphen-demerits etap-slider
     :algorithm 'kpx
     :property 'final-hyphen-demerits
     :reader kpx-final-hyphen-demerits)
   (kpx-similar-demerits etap-slider
     :algorithm 'kpx
     :property 'similar-demerits
     :reader kpx-similar-demerits)
   (kpx-pre-tolerance etap-slider
     :algorithm 'kpx
     :property 'pre-tolerance
     :reader kpx-pre-tolerance)
   (kpx-tolerance etap-slider
     :algorithm 'kpx
     :property 'tolerance
     :reader kpx-tolerance)
   (kpx-emergency-stretch etap-dimen-slider
     :algorithm 'kpx
     :property 'emergency-stretch
     :reader kpx-emergency-stretch)
   (kpx-looseness etap-slider
     :algorithm 'kpx
     :property 'looseness
     :reader kpx-looseness)
   (disposition radio-button-panel
     :layout-class 'column-layout
     :title "Disposition" :title-position :frame
     :visible-max-width nil
     :items *dispositions*
     :print-function 'title-capitalize
     :selection-callback 'set-disposition
     :reader disposition)
   (disposition-options check-button-panel
     :layout-class 'column-layout
     :title "Disposition Options" :title-position :frame
     :visible-max-width nil
     :items *disposition-options*
     :help-keys *disposition-options-help-keys*
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-disposition
     :retract-callback 'set-disposition
     :reader disposition-options-panel)
   (features check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :visible-max-width nil
     :visible-max-height nil
     :items *typesetting-features*
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-features
     :retract-callback 'set-features
     :reader features)
   (paragraph-width slider
     :title "Paragraph width: XXXpt (XXcm)"
     :orientation :horizontal
     :start *paragraph-min-width*
     :end *paragraph-max-width*
     :tick-frequency 0
     :callback 'set-paragraph-width
     :reader paragraph-width)
   (zoom slider
     :title "Paragraph zoom: XXX%"
     :orientation :horizontal
     :start (caliber-min *gui-zoom*)
     :end (caliber-max *gui-zoom*)
     :tick-frequency 0
     :callback 'set-zoom
     :reader zoom)
   (layout-1 push-button
     :text "<"
     :data #'1-
     :callback 'next-layout
     :help-key :layout-1)
   (layout+1 push-button
     :text ">"
     :data #'1+
     :callback 'next-layout
     :help-key :layout+1)
   (clues check-button-panel
     :layout-class 'column-layout
     :title "Characters and Clues" :title-position :frame
     :visible-max-width nil
     :visible-max-height nil
     :items '(:characters :hyphenation-points
	      :paragraph-box :line-boxes :character-boxes :baselines
	      :over/underfull-boxes :overshrunk/stretched-boxes
	      :properties-tooltips :river-beds)
     :print-function 'title-capitalize
     :selection-callback 'set-clues
     :retract-callback 'set-clues
     :reader clues)
   (text-button popup-menu-button :text "Source text" :menu text-menu)
   (language-button popup-menu-button :text "Language" :menu language-menu)
   (text editor-pane
     :visible-min-width '(character 80)
     ;;:visible-max-width '(character 80)
     :visible-min-height '(character 10)
     :visible-max-height '(character 30)
     :change-callback 'set-text
     :reader text)
   (view output-pane
     :title "Layout" :title-position :frame
     :font (gp:make-font-description :family "Latin Modern Roman"
	     :weight :normal :slant :roman :size 10)
     :visible-min-height 300
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'render-view
     :reader view
     ;; :input-model '(((:button-1 :press) display-properties))))
     :input-model '((:motion display-properties))))
  (:layouts
   (main column-layout '(settings view))
   (settings row-layout '(settings-1 settings-2))
   (settings-1 column-layout '(options paragraph-width zoom layouts-ctrl)
     :reader settings-1)
   (layouts-ctrl row-layout '(layout-1 layout+1))
   (options row-layout '(options-1 options-2))
   (options-1 column-layout '(disposition disposition-options features))
   (options-2 column-layout '(clues))
   (settings-2 column-layout '(algorithms text-options text)
     :reader settings-2)
   (text-options row-layout '(text-button language-button))
   (fixed-settings row-layout '(fixed-fallback fixed-options fixed-parameters))
   (fixed-parameters column-layout
     '(fixed-width-offset)
     :title "Other Parameters"
     :title-position :frame
     :visible-max-height nil)
   (fit-settings row-layout
     '(fit-variant fit-fallback fit-discriminating-function fit-options
       fit-parameters))
   (fit-parameters column-layout
     '(fit-line-penalty fit-hyphen-penalty fit-explicit-hyphen-penalty
       fit-width-offset)
     :title "Other Parameters"
     :title-position :frame
     :visible-max-height nil)
   (barnett-settings row-layout '())
   (duncan-settings row-layout '(duncan-discriminating-function))
   (kp-settings row-layout '(kp-variant kp-sliders))
   (kp-sliders grid-layout
     '(kp-line-penalty            kp-adjacent-demerits      kp-pre-tolerance
       kp-hyphen-penalty          kp-double-hyphen-demerits kp-tolerance
       kp-explicit-hyphen-penalty kp-final-hyphen-demerits  kp-emergency-stretch
       nil                        nil                       kp-looseness)
     :columns 3)
   (kpx-settings row-layout '(kpx-variant kpx-fitness kpx-sliders))
   (kpx-sliders grid-layout
     '(kpx-line-penalty            kpx-adjacent-demerits      kpx-pre-tolerance
       kpx-hyphen-penalty          kpx-double-hyphen-demerits kpx-tolerance
       kpx-explicit-hyphen-penalty kpx-final-hyphen-demerits  kpx-emergency-stretch
       nil                         kpx-similar-demerits       kpx-looseness)
     :columns 3))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))

(defmethod initialize-instance :after ((etap etap) &key zoom clues)
  "Adjust some creation-time GUI options.
This currently includes the initial ZOOMing factor and CLUES."
  (setf (titled-object-title (zoom etap))
	(format nil "Paragraph zoom: ~3D%" zoom))
  (setf (range-slug-start (zoom etap)) zoom)
  (setf (choice-selected-items (clues etap)) clues))


;; Interface display

(defun collect-options-indices (options choices)
  "Collect each CHOICES option's index in OPTIONS."
  (loop :for option :in choices
	:for i :from 0
	:when (cadr (member (car option) options))
	  :collect i))

(defun set-choice-selection (pane options choices)
  "Set PANE's choice selection from CHOICES in OPTIONS."
  (setf (choice-selection pane) (collect-options-indices options choices)))

(defmethod interface-display :before
    ((etap etap) &aux (context (context etap)))
  "Prepare ETAP GUI for display."
  (setf (slot-value (rivers-interface etap) 'main-interface) etap)
  ;; #### NOTE: this menu's selection is updated on pop-up.
  (setf (menu-items (slot-value etap 'language-menu))
	(list (make-instance 'menu-component
		:items (mapcar #'car *languages*)
		:interaction :single-selection
		:print-function 'title-capitalize
		:callback 'language-menu-callback
		:popup-callback 'language-menu-popup-callback)))
  (let ((algorithm (algorithm-type (algorithm context)))
	(options (algorithm-options (algorithm context))))
    (macrolet
	((set-variant (prefix)
	   (let ((accessor (intern (concatenate 'string
				     (string prefix) "-VARIANT")))
		 (choices (intern (concatenate 'string
				    "*" (string prefix) "-VARIANTS*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member :variant options)) (car ,choices)))))
	 (set-fallback (prefix)
	   (let ((accessor (intern (concatenate 'string
				     (string prefix) "-FALLBACK")))
		 (choices (intern (concatenate 'string
				    "*" (string prefix) "-FALLBACKS*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member :fallback options)) (car ,choices)))))
	 (set-options (prefix)
	   (let ((accessor (intern (concatenate 'string
				     (string prefix) "-OPTIONS")))
		 (choices (intern (concatenate 'string
				    "*" (string prefix) "-OPTIONS*"))))
	     `(set-choice-selection (,accessor etap) options ,choices)))
	 (set-slider (prefix key)
	   (let ((accessor (intern (concatenate 'string
				     (string prefix) "-" (string key))))
		 (caliber (intern (concatenate 'string
				    "*" (string prefix) "-" (string key) "*"))))
	     `(progn
		(setf (range-slug-start (,accessor etap))
		      (or (cadr (member ,key options))
			  (caliber-default ,caliber)))
		(setf (titled-object-title (,accessor etap))
		      (format nil "~A: ~D"
			,(title-capitalize key)
			(range-slug-start (,accessor etap)))))))
	 (set-sliders (prefix &rest sliders)
	   `(progn ,@(mapcar (lambda (slider) `(set-slider ,prefix ,slider))
		       sliders)))
	 (set-choice (prefix key)
	   (let ((accessor
		   (intern (concatenate 'string
			     (string prefix) "-" (string key))))
		 (choices
		   (intern (concatenate 'string
			     "*" (string prefix) "-" (string key) "S*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member ,key options)) (car ,choices))))))
      (case algorithm
	(:fixed
	 (setf (choice-selection (algorithms etap)) 0)
	 (set-fallback fixed)
	 (set-options fixed)
	 ;; SET-SLIDER doesn't handle more informative titles than just
	 ;; displaying the values.
	 (setf (range-slug-start (fixed-width-offset etap))
	       (or (cadr (member :width-offset options))
		   (caliber-default *fixed-width-offset*)))
	 (setf (titled-object-title (fixed-width-offset etap))
	       (format nil "Width Offset: ~Dpt (~Fcm)"
		 (range-slug-start (fixed-width-offset etap))
		 (/ (range-slug-start (fixed-width-offset etap)) 28.452755))))
	(:fit
	 (setf (choice-selection (algorithms etap)) 1)
	 (set-variant fit)
	 (set-fallback fit)
	 (set-options fit)
	 (set-choice fit :discriminating-function)
	 (set-sliders fit :line-penalty
	   :hyphen-penalty :explicit-hyphen-penalty)
	 ;; SET-SLIDER doesn't handle more informative titles than just
	 ;; displaying the values.
	 (setf (range-slug-start (fit-width-offset etap))
	       (or (cadr (member :width-offset options))
		   (caliber-default *fit-width-offset*)))
	 (setf (titled-object-title (fit-width-offset etap))
	       (format nil "Width Offset: ~Dpt (~Fcm)"
		 (range-slug-start (fit-width-offset etap))
		 (/ (range-slug-start (fit-width-offset etap)) 28.452755))))
	(:barnett
	 (setf (choice-selection (algorithms etap)) 2))
	(:duncan
	 (setf (choice-selection (algorithms etap)) 3)
	 (set-choice duncan :discriminating-function))
	(:knuth-plass
	 (setf (choice-selection (algorithms etap)) 4)
	 (set-variant kp)
	 (set-sliders kp
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty
	   :adjacent-demerits :double-hyphen-demerits :final-hyphen-demerits
	   :pre-tolerance :tolerance :emergency-stretch :looseness))
	(:kpx
	 (setf (choice-selection (algorithms etap)) 5)
	 (set-variant kpx)
	 (setf (choice-selected-item (kpx-fitness etap))
	       (or (cadr (member :fitness options)) (car *kpx-fitnesses*)))
	 (set-sliders kpx
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty
	   :adjacent-demerits :double-hyphen-demerits :final-hyphen-demerits
	   :similar-demerits
	   :pre-tolerance :tolerance :emergency-stretch :looseness)))))
  (setf (choice-selected-item (disposition etap))
	(disposition-type (disposition context)))
  (set-choice-selection (disposition-options-panel etap)
			(disposition-options (disposition context))
			*disposition-options*)
  (set-choice-selection (features etap)
			(features context)
			*typesetting-features*)
  (setf (range-slug-start (paragraph-width etap)) (paragraph-width context))
  (setf (titled-object-title (paragraph-width etap))
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  (paragraph-width context) (/ (paragraph-width context) 28.452755)))
  (setf (editor-pane-text (text etap)) (text context))
  (let ((size
	  (multiple-value-list (simple-pane-visible-size (settings-1 etap)))))
    (set-hint-table (settings-1 etap)
      `(:visible-min-width ,(car size) :visible-max-width t
	:visible-min-height ,(cadr size) :visible-max-height t)))
  (let ((size
	  (multiple-value-list (simple-pane-visible-size (settings-2 etap)))))
    (set-hint-table (settings-2 etap)
      `(:visible-min-height ,(cadr size) :visible-max-height t))))



;; ===========
;; Entry Point
;; ===========

(defun run (&key (context *context*) zoom (clues :characters))
  "Run ETAP's GUI for CONTEXT (the global context by default).
Optionally provide initial ZOOMing and CLUES (characters by default)."
  (calibrate-gui zoom)
  (unless (listp clues) (setq clues (list clues)))
  (display (make-instance 'etap
	     :context context
	     :zoom zoom
	     :clues clues
	     :help-callback 'show-help
	     :destroy-callback (lambda (interface)
				 (destroy (rivers-interface interface))))))
