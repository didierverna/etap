(in-package :etap)

;; ==========================================================================
;; Utilities
;; ==========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun title-capitalize (title)
    "Capitalize TITLE and substitute dashes with spaces."
    (nsubstitute #\Space #\- (string-capitalize title))))



;; --------
;; Calibers
;; --------

(defmacro define-gui-caliber (name min default max)
  "Define a NAMEd GUI caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber gui ,name ,min ,default ,max))

(define-gui-caliber zoom 100 100 999)


(defmacro calibrate-gui (name)
  "Calibrate NAMEd GUI variable."
  `(calibrate gui ,name :earmuffs nil))



;; --------------------------------------
;; CLIM-less object under mouse detection
;; --------------------------------------

(defun vector-product (p1 p2 p3)
  "Return the vector product of P1P2 - P1P3.
Each point is of the form (X . Y)."
  (let ((x1 (car p1)) (y1 (cdr p1))
	(x2 (car p2)) (y2 (cdr p2))
	(x3 (car p3)) (y3 (cdr p3)))
    (- (* (- x1 x3) (- y2 y3)) (* (- x2 x3) (- y1 y3)))))

(defun triangle-under-p (p a b c)
  "Return T if P is within the ABC triangle."
  (let ((vp1 (vector-product p a b))
	(vp2 (vector-product p b c))
	(vp3 (vector-product p c a)))
    (not (or (and (< vp1 0) (>= vp2 0) (>= vp3 0))
	     (and (< vp2 0) (>= vp1 0) (>= vp3 0))
	     (and (< vp3 0) (>= vp1 0) (>= vp2 0))
	     (and (> vp1 0) (<= vp2 0) (<= vp3 0))
	     (and (> vp2 0) (<= vp1 0) (<= vp3 0))
	     (and (> vp3 0) (<= vp1 0) (<= vp2 0))))))

(defun hyphenation-point-under (x y lines &aux (p (cons x y)))
  "Return the hyphenation point from LINES which is under (X, Y), or nil.
Technically, (X, Y) is not over the hyphenation point, but over the
corresponding hyphenation clue."
  (let ((line (find-if (lambda (line)
			 (and (>= y (y line)) (<= y (+ (y line) 5))))
		       lines)))
    (when line
      (let* ((x (x line))
	     (y (y line))
	     (pinned (find-if (lambda (item)
				(and (discretionary-clue-p (object item))
				     (hyphenation-point-p
				      (discretionary (object item)))
				     (triangle-under-p
				      p
				      (cons (+ x (x item)) y)
				      (cons (+ x (x item) -3) (+ y 5))
				      (cons (+ x (x item) +3) (+ y 5)))))
			      (items line))))
	(when pinned (discretionary (object pinned)))))))

(defun line-under (y lines)
  "Return the line from LINES which is under Y coordinate, or NIL."
  (find-if (lambda (line)
	     (and (>= y (- (y line) (height line)))
		  (<= y (+ (y line) (depth line)))))
	   lines))



;; ------------------------
;; Panes hierarchy enabling
;; ------------------------

;; #### NOTE: there is no mechanism to globally enable or disable an
;; interface or a layout's components, so we need to do it by hand.

(defgeneric enable-interface (interface &optional enabled)
  (:documentation "Set INTERFACE's enabled status to ENABLED (T by default)."))

(defun enable-pane (pane &optional (enabled t))
  "Set PANE and its descendants'enabled status to ENABLED (T by default)."
  (map-pane-descendant-children
   pane (lambda (child) (setf (simple-pane-enabled child) enabled))
   :test (lambda (child) (typep child 'simple-pane))
   :visible t)
  (when (typep pane 'simple-pane) (setf (simple-pane-enabled pane) enabled)))



;; -------------------------
;; Various utility protocols
;; -------------------------

(defgeneric river-detection-p (interface)
  (:documentation "Return T if river detection is enabled in INTERFACE."))




;; ==========================================================================
;; Updaters
;; ==========================================================================

;; #### FIXME: see comment in rivers.lisp
(defun remake-rivers (interface &aux (layout (layout interface)))
  "Remake INTERFACE's rivers."
  (setf (rivers interface)
	(when (and (river-detection-p interface) (not (zerop layout)))
	  (detect-rivers
	   (get-layout (1- layout) (breakup interface))
	   (range-slug-start
	    (angle-slider (river-detection-panel interface)))))))

(defun remake-breakup (interface &rest args)
  "Remake INTERFACE's breakup. ARGS are passed along to MAKE-BREAKUP."
  (let* ((breakup (apply #'make-breakup :context (context interface) args))
	 (layouts-# (layouts-# breakup)))
    (setf (breakup interface) breakup)
    (setf (layout interface) (if (zerop layouts-#) 0 1))
    (setf (titled-object-title (view interface))
	  (format nil "Layout ~D/~D" (layout interface) layouts-#))))

(defun update (interface &rest args)
  "Update INTERFACE.
This remakes INTERFACE's breakup, everything that depends on it,
and invalidates the view.
ARGS are passed along to MAKE-BREAKUP."
  (apply #'remake-breakup interface args)
  (remake-rivers interface)
  (gp:invalidate-rectangle (view interface)))

(defun update-from-lineup (interface)
  "Update INTERFACE sarting from the current lineup.
See `update' for more information."
  (update interface :lineup (lineup (breakup interface))))




;; ==========================================================================
;; River Detection Interface
;; ==========================================================================

(defun river-detection-activation-switch-callback
    (switch interface
     &aux (main-interface (main-interface interface)))
  "Function called when the river detection activation SWITCH is toggled."
  (remake-rivers main-interface)
  (setf (simple-pane-enabled (angle-slider interface))
	(button-selected switch))
  (gp:invalidate-rectangle (view main-interface)))

;; #### WARNING: moving the slider with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that I can safely ignore :MOVE callbacks which means saving two
;; calls out of 3! I will need to check this again when I introduce focus and
;; keyboard control though.
(defun river-detection-angle-slider-callback
    (slider value gesture
     &aux (main-interface (main-interface (top-level-interface slider))))
  "Function called when the river detection angle slider is moved."
  (when (eq gesture :drag)
    (setf (titled-object-title slider) (format nil "Angle: ~D°" value))
    (remake-rivers main-interface)
    (gp:invalidate-rectangle (view main-interface))))

(define-interface river-detection-panel ()
  ((main-interface :reader main-interface))
  (:panes
   (activation-switch check-button
     :text "Detect rivers"
     :selection-callback 'river-detection-activation-switch-callback
     :retract-callback 'river-detection-activation-switch-callback
     :callback-type '(:element :interface)
     :reader activation-switch)
   (angle-slider slider
     :title "Angle: 0°"
     :orientation :horizontal
     :visible-min-width 250
     :visible-max-width 250
     :start 0
     :end 45
     :slug-start 0
     :tick-frequency 0
     :enabled nil
     :callback 'river-detection-angle-slider-callback
     :reader angle-slider))
  (:layouts
   (main column-layout
     '(activation-switch angle-slider)))
  (:default-initargs
   :title "River Detection"
   :window-styles '(:always-on-top t :toolbox t)))

(defmethod river-detection-p ((interface river-detection-panel))
  "Return T if river detection is enabled in INTERFACE."
  (button-selected (activation-switch interface)))




;; ==========================================================================
;; Algorithmic GUI Components
;; ==========================================================================

(defclass agc ()
  ((algorithm
    :documentation "This component's algorithm."
    :initarg :algorithm :reader algorithm))
  (:default-initargs :title "Dummy")
  (:documentation "The Algorithmic GUI Component class.
This class serves as a mixin for GUI components representing algorithmic
settings."))

;; #### WARNING: CAPI sliders impose a specific signature on callbacks (3
;; mandatory arguments: pane, value, and gesture) so we need to conform to
;; that if we want to use a single generic function for all kinds of GUI
;; components.
(defgeneric agc-callback (component arg2 arg3)
  (:documentation "The algorithmic GUI components callback.")
  (:method ((component agc) arg2 arg3
	    &aux (interface (top-level-interface component)))
    "Update algorithmic settings in COMPONENT's interface context."
    (select-algorithm (algorithm component) interface))
  (:method :after ((component agc) arg2 arg3
		   &aux (interface (top-level-interface component)))
    "Update COMPONENT's interface."
    (update interface)))



;; -----------
;; AGC Sliders
;; -----------

;; #### NOTE: even though the caliber's min and max values could be retrieved
;; through the slider's range start and end, it's still useful to keep the
;; caliber around because of its default value and behavior wrt infinity.
(defclass agc-slider (agc slider)
  ((property-name
    :documentation "This slider's property name."
    :reader property-name)
   (caliber
    :documentation "This slider's corresponding caliber."
    :reader caliber))
  (:default-initargs
   :tick-frequency 0
   :orientation :horizontal
   :visible-min-width 220
   :callback 'agc-callback)
  (:documentation "The AGC Slider Class."))


(defgeneric agc-slider-title (slider)
  (:documentation "Compute AGC SlIDER's title based on its current value.")
  (:method ((slider agc-slider))
    "Advertise AGC SLIDER's value in its title. This is the default method."
    (format nil "~A: ~A"
      (property-name slider)
      (calibrated-value (range-slug-start slider) (caliber slider)))))

(defun update-agc-slider-title (slider)
  "Update AGC SLIDER's title to its current value."
  (setf (titled-object-title slider) (agc-slider-title slider)))


(defmethod initialize-instance :after ((slider agc-slider) &key property)
  "Post-initialize AGC SLIDER."
  (setf (slot-value slider 'property-name) (title-capitalize property))
  (let ((caliber (symbol-value (intern (concatenate 'string
					 "*"
					 (symbol-name (algorithm slider))
					 "-"
					 (symbol-name property)
					 "*")
				       :etap))))
    (setf (slot-value slider 'caliber) caliber)
    (setf (range-start slider)      (caliber-min caliber)
	  (range-end slider)        (caliber-max caliber)
	  (range-slug-start slider) (caliber-default caliber))
    (update-agc-slider-title slider)))


;; #### WARNING: moving the slider with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that I can safely ignore :MOVE callbacks which means saving two
;; calls out of 3! I will need to check this again when I introduce focus and
;; keyboard control though.
(defmethod agc-callback :around ((slider agc-slider) value gesture)
  (when (eq gesture :drag) (call-next-method)))

(defmethod agc-callback :before ((slider agc-slider) value gesture)
  "Update AGC SLIDER's title."
  (declare (ignore value gesture))
  (update-agc-slider-title slider))


(defmacro agc-slider-setting (algorithm property interface)
  "Return (:PROPERTY (RANGE-SLUG-START (ALGORITHM-PROPERTY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name property)))))
    `(list ,property (range-slug-start (,accessor ,interface)))))


;; AGC Dimension sliders

(defclass agc-dimen-slider (agc-slider)
  ()
  (:documentation "The AGC Dimension Slider Class."))

(defmethod agc-slider-title
    ((slider agc-dimen-slider)
     &aux (value (calibrated-value (range-slug-start slider) (caliber slider))))
  "Advertise AGC dimension SLIDER's value in its title, in pt and cm."
  (format nil "~A: ~A~@[pt (~Acm)~]"
    (property-name slider)
    value
    (when (numberp value) (float (/ value 28.452755)))))



;; -----------------
;; AGC Button Panels
;; -----------------

(defclass agc-button-panel (agc)
  ()
  (:default-initargs
   :title-position :frame
   :layout-class 'column-layout
   :visible-max-height nil
   :print-function 'title-capitalize
   :callback-type '(:element :data :interface) ; see comment atop AGC
   :selection-callback 'agc-callback)
  (:documentation "The Algorithm GUI Component Button Panel class.
This is the mixin class for AGC radio and check button panels."))


;; Radio Button Panels

(defclass agc-radio-button-panel (agc-button-panel radio-button-panel)
  ()
  (:documentation "The AGC Radio Button Panel Class."))

(defmethod initialize-instance :after
    ((panel agc-radio-button-panel) &key property plural)
  "Post-initialize AGC radio button PANEL."
  (setf (collection-items panel)
	(symbol-value (intern (concatenate 'string
				"*"
				(symbol-name (algorithm panel))
				"-"
				(symbol-name property)
				(ecase plural
				  (:ies "IES")
				  (:es "ES")
				  ((:s nil) "S"))
				"*")
			      :etap)))
  (setf (titled-object-title panel) (title-capitalize property)))


(defmacro agc-radio-setting (algorithm property interface)
  "Return (:PROPERTY (CHOICE-SELECTED-ITEM (ALGORITHM-PROPERTY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name property)))))
    `(list ,property (choice-selected-item (,accessor ,interface)))))


;; Check Button Panels

(defclass agc-check-button-panel (agc-button-panel check-button-panel)
  ()
  (:default-initargs :retract-callback 'agc-callback)
  (:documentation "The AGC Radio Button Panel Class."))

(defmethod initialize-instance :after
    ((panel agc-check-button-panel) &key properties)
  "Post-initialize ETAP check button PANEL."
  (setf (collection-items panel)
	(symbol-value (intern (concatenate 'string
				"*"
				(symbol-name (algorithm panel))
				"-"
				(symbol-name properties)
				"*")
			      :etap)))
  (setf (titled-object-title panel) (title-capitalize properties)))

(defun selection-plist (selected all)
  "Return a property list for ALL items.
The values are T if the item is SELECTED, or NIL otherwise."
  (loop :for item :across all ; collection items are in a vector
	:collect item
	:if (member item selected)
	  :collect t
	:else
	  :collect nil))

(defun choice-selection-plist (choice)
  "Return CHOICE's selection property list.
See `selection-plist' for more information."
  (selection-plist (choice-selected-items choice) (collection-items choice)))

(defmacro agc-check-settings (algorithm properties interface)
  "Return (CHOICE-SELECTION-PLIST (ALGORITHM-PROPERTIES INTERFACE))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name properties)))))
    `(choice-selection-plist (,accessor ,interface))))




;; ==========================================================================
;; Interface Actions
;; ==========================================================================

;; -------------------
;; Algorithm Selection
;; -------------------

(defgeneric select-algorithm (algorithm interface)
  (:documentation "Select ALGORITHM in INTERFACE's context.")
  (:method ((algorithm (eql :fixed)) interface)
    "Select the Fixed algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :fixed
		(append
		  (agc-radio-setting fixed :fallback interface)
		  (agc-slider-setting fixed :width-offset interface)
		  (agc-check-settings fixed options interface)))))
  (:method ((algorithm (eql :fit)) interface)
    "Select the Fit algorithm  in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :fit
		(append
		  (agc-radio-setting fit :variant interface)
		  (agc-radio-setting fit :fallback interface)
		  (agc-radio-setting fit :discriminating-function interface)
		  (agc-slider-setting fit :line-penalty interface)
		  (agc-slider-setting fit :hyphen-penalty interface)
		  (agc-slider-setting fit :explicit-hyphen-penalty interface)
		  (agc-slider-setting fit :width-offset interface)
		  (agc-check-settings fit options interface)))))
  (:method ((algorithm (eql :barnett)) interface)
    "Select the Barnett algorithm in INTERFACE's context."
    (setf (algorithm (context interface)) '(:barnett)))
  (:method ((algorithm (eql :duncan)) interface)
    "Select the Duncan algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :duncan
		(agc-radio-setting duncan :discriminating-function interface))))
  (:method ((algorithm (eql :kp)) interface)
    "Select the Knuth-Plass algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :knuth-plass
		(append
		 (agc-radio-setting kp :variant interface)
		 (agc-slider-setting kp :line-penalty interface)
		 (agc-slider-setting kp :hyphen-penalty interface)
		 (agc-slider-setting kp :explicit-hyphen-penalty interface)
		 (agc-slider-setting kp :adjacent-demerits interface)
		 (agc-slider-setting kp :double-hyphen-demerits interface)
		 (agc-slider-setting kp :final-hyphen-demerits interface)
		 (agc-slider-setting kp :pre-tolerance interface)
		 (agc-slider-setting kp :tolerance interface)
		 (agc-slider-setting kp :emergency-stretch interface)
		 (agc-slider-setting kp :looseness interface)))))
  (:method ((algorithm (eql :kpx)) interface)
    "Select the KPX algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :kpx
		(append
		 (agc-radio-setting kpx :variant interface)
		 (agc-radio-setting kpx :fitness interface)
		 (agc-slider-setting kpx :line-penalty interface)
		 (agc-slider-setting kpx :hyphen-penalty interface)
		 (agc-slider-setting kpx :explicit-hyphen-penalty interface)
		 (agc-slider-setting kpx :adjacent-demerits interface)
		 (agc-slider-setting kpx :double-hyphen-demerits interface)
		 (agc-slider-setting kpx :final-hyphen-demerits interface)
		 (agc-slider-setting kpx :similar-demerits interface)
		 (agc-slider-setting kpx :pre-tolerance interface)
		 (agc-slider-setting kpx :tolerance interface)
		 (agc-slider-setting kpx :emergency-stretch interface)
		 (agc-slider-setting kpx :looseness interface))))))


;; ---------
;; Callbacks
;; ---------

(defun set-algorithm (value interface)
  "If INTERFACE is enabled, set algorithm specified by VALUE.
Otherwise, do nothing."
  (if (enabled interface)
    (let ((algorithm (car value)))
      ;; #### WARNING: hack alert. The Knuth-Plass prefix is :kp throughout,
      ;; except that it's :knuth-plass in contexts, and also in the interface
      ;; algorithm selection pane where the title needs to be human readable.
      ;; Hence the title conversion below.
      (when (eq algorithm :knuth-plass) (setq algorithm :kp))
      (select-algorithm algorithm interface)
      (update interface))
    ;; #### NOTE: in fact, we're not really doing nothing here. The call to
    ;; (SETF CHOICE-SELECTION) below allows the correct tab layout button to
    ;; remain highlighted. Beware however that doing it triggers this very
    ;; same callback again, so we need to take care of avoiding an infinite
    ;; callback loop by changing the choice selection only when necessary.
    (let* ((algorithms-tab-layout (algorithms interface))
	   (old-selection
	     (position (algorithm-type (algorithm (context interface)))
		       (collection-items algorithms-tab-layout)
		       :key #'car))
	   (new-selection (choice-selection algorithms-tab-layout)))
      (unless (eq old-selection new-selection)
	(setf (choice-selection algorithms-tab-layout) old-selection)))))

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
  (update-from-lineup interface))

(defun set-zoom (pane value status)
  "Set PANE's zooming to to VALUE."
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Paragraph zoom: ~3D%" value))
  (gp:invalidate-rectangle (view (top-level-interface pane))))

(defun set-clues (value interface)
  "Invalidate INTERFACE's view after a change to the clues."
  (declare (ignore value))
  (gp:invalidate-rectangle (view interface)))


;; -------------------
;; Paragraph Rendering
;; -------------------

;; #### FIXME: the bounds are hard-wired, but should really depend on the
;; defined caliber.
(defun penalty-hue (penalty)
  "Return PENALTY's HUE in HSV model.
Colors are interpolated for penalties ranging from  -∞ (blue),
through 0 (green), and finally to +∞ (red)."
  (cond ((eq penalty +∞) (setq penalty 10000))
	((eq penalty -∞) (setq penalty -10000)))
  (- 4s0 (* 4s0 (/ (+ penalty 10000s0) 20000s0))))

(defun render-view
    (pane x y width height
     &aux (interface (top-level-interface pane))
	  (breakup (breakup interface))
	  (par-width (paragraph-width breakup))
	  (layout-# (layout interface))
	  (layout (unless (zerop layout-#) (get-layout (1- layout-#) breakup)))
	  (par-y (height layout))
	  (par-h+d (+ par-y (depth layout)))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (clues (choice-selected-items (clues interface))))
  "Render PANE's view."
  (declare (ignore x y width height))
  (set-horizontal-scroll-parameters pane :max-range (+ (* par-width zoom) 40))
  (set-vertical-scroll-parameters pane :max-range (+ (* par-h+d zoom) 40))
  (gp:with-graphics-translation (pane 20 20)
    (gp:with-graphics-scale (pane zoom zoom)
      (when (member :paragraph-box clues)
	(gp:draw-rectangle pane 0 0 par-width par-h+d
	  :foreground :red
	  :scale-thickness nil))
      (when layout
	(loop :for full-x := (+ (loop :for line :in (lines layout)
				      :maximize (+ (x line) (width line)))
				5)
	      :for rest :on (lines layout)
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
		:if (> (width line) par-width)
		  :do (gp:draw-rectangle pane
			  full-x  (- y (height line))
			  5  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr rest) ;; not the last one
			       (eq (disposition-type (disposition breakup))
				   :justified)
			       (< (width line) par-width))
		  :do (gp:draw-rectangle pane
			  full-x (- y (height line))
			  5 (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if ($< (esar line) (asar line))
		  :do (gp:draw-polygon pane
			  (list (+ par-width 5)
				(- y (height line))
				(+ par-width 11)
				(- y (height line))
				(+ par-width 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled t :closed t)
		:else :if ($< (asar line) -1)
		  :do (gp:draw-polygon pane
			  (list (+ par-width 5)
				(- y (height line))
				(+ par-width 11)
				(- y (height line))
				(+ par-width 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled nil :closed t)
		:else :if ($> (esar line) (asar line))
		  :do (gp:draw-polygon pane
			  (list (+ par-width 5)
				(+ y (depth line))
				(+ par-width 11)
				(+ y (depth line))
				(+ par-width 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($> (asar line) 1)
		  :do (gp:draw-polygon pane
			  (list (+ par-width 5)
				(+ y (depth line))
				(+ par-width 11)
				(+ y (depth line))
				(+ par-width 8)
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
				  ((and (discretionary-clue-p (object item))
					(hyphenation-point-p
					 (discretionary (object item)))
					(member :hyphenation-points clues))
				   (gp:draw-polygon pane
				     (list (+ x (x item)) y
					   (+ x (x item) -3) (+ y 5)
					   (+ x (x item) +3) (+ y 5)
					   (+ x (x item)) y)
				     :filled
				     (not (explicitp
					   (discretionary (object item))))
				     :foreground
				     (color:make-hsv
				      (penalty-hue
				       (penalty(discretionary (object item))))
				      1s0 .7s0)))))
		      (items line)))
	(when (and (member :rivers clues) (rivers interface))
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
		   (rivers interface)))))))


;; -------
;; Layouts
;; -------

(defun next-layout
    (op interface
     &aux (layouts-# (layouts-# (breakup interface)))
	  (layout (layout interface)))
  "Select the next OP layout."
  (unless (zerop layouts-#)
    (setq layout (1+ (mod (1- (funcall op layout)) layouts-#)))
    (setf (layout interface) layout)
    (when (river-detection-p interface) (remake-rivers interface))
    (setf (titled-object-title (view interface))
	  (format nil "Layout ~D/~D" layout layouts-#))
    (gp:invalidate-rectangle (view interface))))


;; --------
;; Tooltips
;; --------

(defparameter *interface-tooltips*
  '(:layout--1 "Display previous layout."
    :layout-+1 "Display next layout."))

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


;; ---------------
;; Motion Callback
;; ---------------

(defun motion-callback
    (pane x y
     &aux (interface (top-level-interface pane))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (breakup (breakup interface))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout interface)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Display the properties of the paragraph, or the line clicked on."
  (when (member :properties-tooltips (choice-selected-items (clues interface)))
    (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
    ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
    ;; returning 0, but this is borderline.
    (decf y (height layout))
    (if (or (and (<= x 0) (<= y (depth layout)))
	    (and (<= y (- (height layout))) (<= x par-width)))
      (display-tooltip pane :text (properties breakup :layout-# layout-#))
      (when layout
	(let (object)
	  (if (setq object
		    (or (and (member
			      :hyphenation-points
			      (choice-selected-items (clues interface)))
			     ;; #### NOTE: the +3 and (+ ... 5) are for
			     ;; hyphenation clues occurring at the end of the
			     ;; lines, or in the last line.
			     (>= x 0)
			     (<= x (+ par-width 3))
			     (>= y 0) ; no need to look above the 1st line
			     (<= y (+ (y (car (last (lines layout)))) 5))
			     (hyphenation-point-under x y (lines layout)))
			(and (>= x 0) (<= x par-width)
			     (>= y (- (height layout))) (<= y (depth layout))
			     (line-under y (lines layout)))))
	    (display-tooltip pane :text (properties object))
	    (display-tooltip pane)))))))


;; ------------------
;; Penalty Adjustment
;; ------------------

;; #### WARNING: the global variables defining each algorithm's
;; parametrization are calibrated by the algorithms entry points, because
;; those entry points can be called programmatically. On the other hand, the
;; penalty sliders and reset buttons below affect an already existing lineup
;; and are accessible only from the GUI. Hence, the returned values need to be
;; calibrated (potentially to infinity) here.

(defun penalty-slider-callback
    (pane value status
     &aux (interface (top-level-interface pane))
	  (main-interface (main-interface interface))
	  (hyphenation-point (hyphenation-point interface)))
  "Set PANE's corresponding break point penalty."
  (declare (ignore status))
  (setq value
	(calibrated-value (range-slug-start pane) (caliber hyphenation-point)))
  (setf (title-pane-text (title interface)) (princ-to-string value))
  (setf (penalty hyphenation-point) value)
  (update-from-lineup main-interface))

(defun reset-buttons-callback
  (data pane
   &aux (interface (top-level-interface pane))
	(penalty-slider (penalty-slider interface))
	(hyphenation-point (hyphenation-point interface))
	(caliber (caliber hyphenation-point))
	(value (ecase data
		 (:reset-to-original
		  (original-value interface))
		 (:reset-to-global
		  (or
		   (getf (cdr (algorithm (context (main-interface interface))))
			 (caliber-property caliber))
		   (caliber-default caliber)))
		 (:reset-to-default
		  (caliber-default caliber)))))
  (setf (range-slug-start penalty-slider) value)
  (penalty-slider-callback penalty-slider value nil))

(define-interface penalty-adjustment ()
  ((original-value :reader original-value)
   (hyphenation-point :initarg :hyphenation-point :reader hyphenation-point)
   (main-interface :initarg :main-interface :reader main-interface))
  (:panes
   (title title-pane
     :reader title)
   (penalty-slider slider
     :orientation :vertical
     :visible-min-height 220
     :tick-frequency 0
     :callback 'penalty-slider-callback
     :reader penalty-slider)
   (reset-buttons push-button-panel
     :items '(:reset-to-original :reset-to-global :reset-to-default)
     :print-function 'title-capitalize
     :layout-class 'column-layout
     :selection-callback 'reset-buttons-callback))
  (:layouts
   (main column-layout '(title row))
   (row row-layout '(penalty-slider reset-buttons)))
  (:default-initargs :title "Penalty Adjustment"))

(defmethod initialize-instance :after
    ((interface penalty-adjustment)
     &key
     &aux (hyphenation-point (hyphenation-point interface)))
  "Memorize the original penalty value."
  (setf (slot-value interface 'original-value)
	(decalibrated-value
	 (penalty hyphenation-point) (caliber hyphenation-point))))

(defmethod interface-display :before ((interface penalty-adjustment))
  "Prepare the penalty adjustment INTERFACE for display."
  (let* ((slider (penalty-slider interface))
	 (hyphenation-point (hyphenation-point interface))
	 (caliber (caliber hyphenation-point)))
    (setf (range-start slider) (caliber-min caliber)
	  (range-end slider)   (caliber-max caliber))
    (setf (range-slug-start slider) (original-value interface))
    (setf (title-pane-text (title interface))
	  (princ-to-string (penalty hyphenation-point)))))

(defun penalty-adjustment-dialog-destroy-callback (dialog)
  "Possibly re-enable the main interface if DIALOG was the last one."
  (let ((interface (main-interface dialog)))
    (setf (penalty-adjustment-dialogs interface)
	  (remove dialog (penalty-adjustment-dialogs interface)))
    (unless (penalty-adjustment-dialogs interface)
      (enable-interface interface))))

(defun make-penalty-adjustment (hyphenation-point interface)
  "Display a penalty adjustment dialog for HYPHENATION-POINT.
If one already exists, activate it and give it the focus.
Otherwise, create the dialog first.

INTERFACE is the main ETAP window."
  (let ((dialog (find hyphenation-point (penalty-adjustment-dialogs interface)
		  :key #'hyphenation-point)))
    (if dialog
      (activate-pane dialog)
      (multiple-value-bind (x y) (top-level-interface-geometry interface)
	(setq dialog (make-instance 'penalty-adjustment
		       :hyphenation-point hyphenation-point
		       :main-interface interface
		       :destroy-callback
		       'penalty-adjustment-dialog-destroy-callback))
	(set-top-level-interface-geometry dialog :x (+ x 200) :y (+ y 200))
	(push dialog (penalty-adjustment-dialogs interface))
	(display dialog
		 :owner interface
		 :window-styles '(:toolbox t
				  :never-iconic t
				  :always-on-top t
				  :can-full-screen nil)))))
  (when (enabled interface) (enable-interface interface nil)))


;; ------------------
;; Post Menu Callback
;; ------------------

;; #### TODO: when this gets enriched, we will eventually end up with the same
;; logic as in MOTION-CALLBACK in order to figure out what's under the mouse,
;; and we already wish we used CLIM...
(defun post-menu-callback
    (pane x y
     &aux (interface (top-level-interface pane))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (breakup (breakup interface))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout interface)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
  ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
  ;; returning 0, but this is borderline.
  (decf y (height layout))
  (when layout
    (let ((object (and (member :hyphenation-points
			       (choice-selected-items (clues interface)))
		       ;; #### NOTE: the +3 and (+ ... 5) are for hyphenation
		       ;; clues occurring at the end of the lines, or in the
		       ;; last line.
		       (>= x 0)
		       (<= x (+ par-width 3))
		       (>= y 0) ; no need to look above the 1st line
		       (<= y (+ (y (car (last (lines layout)))) 5))
		       (hyphenation-point-under x y (lines layout)))))
      (when object
	(make-penalty-adjustment object interface)))))


;; -----
;; Menus
;; -----

(defun etap-menu-callback (data interface)
  "ETAP menu callback."
  (ecase data
    (:reset-paragraph
     (update interface))
    (:river-detection
     (display (river-detection-panel interface) :owner interface))))

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




;; ==========================================================================
;; Interface
;; ==========================================================================

(defun algorithms-tab-layout-visible-child-function (item interface)
  "Return the appropriate algorithm pane to display.
This is either ITEM's second element if INTERFACE is enabled,
or the current algorithm's one otherwise."
  (unless (enabled interface)
    (setq item (find (algorithm-type (algorithm (context interface)))
		   (collection-items (algorithms interface))
		 :key #'car)))
  (second item))

(define-interface etap ()
  ((context :initform *context* :initarg :context :reader context)
   (breakup :accessor breakup)
   (layout :initform 0 :accessor layout)
   (enabled :initform t :accessor enabled)
   (penalty-adjustment-dialogs
    :initform nil
    :accessor penalty-adjustment-dialogs)
   (rivers
    :documentation "The paragraph's detected rivers."
    :initform nil
    :accessor rivers)
   (river-detection-panel
    :initform (make-instance 'river-detection-panel)
    :reader river-detection-panel))
  (:menus
   (etap-menu "ETAP" (:reset-paragraph :river-detection)
     :print-function 'title-capitalize
     :callback 'etap-menu-callback)
   (text-menu nil ;; Ignore popup menu's title
    (:reset)
    :print-function 'title-capitalize
    :callback 'text-menu-callback)
   (language-menu nil ;; Ignore popup menu's title
     nil)) ;; The items will be created dynamically in INTERFACE-DISPLAY.
  (:menu-bar etap-menu)
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
     ;; #### NOTE: :VISIBLE-CHILD-FUNCTION is set in the INITIALIZE-INSTANCE
     ;; :after method.
     :selection-callback 'set-algorithm
     :reader algorithms)
   (fixed-fallback agc-radio-button-panel
     :algorithm :fixed
     :property :fallback
     :help-keys *fixed-fallbacks-help-keys*
     :reader fixed-fallback)
   (fixed-options agc-check-button-panel
     :algorithm :fixed
     :properties :options
     :help-keys *fixed-options-help-keys*
     :reader fixed-options)
   (fixed-width-offset agc-dimen-slider
     :algorithm :fixed
     :property :width-offset
     :reader fixed-width-offset)
   (fit-variant agc-radio-button-panel
     :algorithm :fit
     :property :variant
     :help-keys *fit-variants-help-keys*
     :reader fit-variant)
   (fit-fallback agc-radio-button-panel
     :algorithm :fit
     :property :fallback
     :help-keys *fit-fallbacks-help-keys*
     :reader fit-fallback)
   (fit-discriminating-function agc-radio-button-panel
     :algorithm :fit
     :property :discriminating-function
     :help-keys *fit-discriminating-functions-help-keys*
     :reader fit-discriminating-function)
   (fit-options agc-check-button-panel
     :algorithm :fit
     :properties :options
     :help-keys *fit-options-help-keys*
     :reader fit-options)
   (fit-line-penalty agc-slider
     :algorithm :fit
     :property :line-penalty
     :reader fit-line-penalty)
   (fit-hyphen-penalty agc-slider
     :algorithm :fit
     :property :hyphen-penalty
     :reader fit-hyphen-penalty)
   (fit-explicit-hyphen-penalty agc-slider
     :algorithm :fit
     :property :explicit-hyphen-penalty
     :reader fit-explicit-hyphen-penalty)
   (fit-width-offset agc-dimen-slider
     :algorithm :fit
     :property :width-offset
     :reader fit-width-offset)
   (duncan-discriminating-function agc-radio-button-panel
     :algorithm :duncan
     :property :discriminating-function
     :help-keys *duncan-discriminating-functions-help-keys*
     :reader duncan-discriminating-function)
   (kp-variant agc-radio-button-panel
     :algorithm :kp
     :property :variant
     :help-keys *kp-variants-help-keys*
     :reader kp-variant)
   (kp-line-penalty agc-slider
     :algorithm :kp
     :property :line-penalty
     :reader kp-line-penalty)
   (kp-hyphen-penalty agc-slider
     :algorithm :kp
     :property :hyphen-penalty
     :reader kp-hyphen-penalty)
   (kp-explicit-hyphen-penalty agc-slider
     :algorithm :kp
     :property :explicit-hyphen-penalty
     :reader kp-explicit-hyphen-penalty)
   (kp-adjacent-demerits agc-slider
     :algorithm :kp
     :property :adjacent-demerits
     :reader kp-adjacent-demerits)
   (kp-double-hyphen-demerits agc-slider
     :algorithm :kp
     :property :double-hyphen-demerits
     :reader kp-double-hyphen-demerits)
   (kp-final-hyphen-demerits agc-slider
     :algorithm :kp
     :property :final-hyphen-demerits
     :reader kp-final-hyphen-demerits)
   (kp-pre-tolerance agc-slider
     :algorithm :kp
     :property :pre-tolerance
     :reader kp-pre-tolerance)
   (kp-tolerance agc-slider
     :algorithm :kp
     :property :tolerance
     :reader kp-tolerance)
   (kp-emergency-stretch agc-dimen-slider
     :algorithm :kp
     :property :emergency-stretch
     :reader kp-emergency-stretch)
   (kp-looseness agc-slider
     :algorithm :kp
     :property :looseness
     :reader kp-looseness)
   (kpx-variant agc-radio-button-panel
     :algorithm :kpx
     :property :variant
     :help-keys *kpx-variants-help-keys*
     :reader kpx-variant)
   (kpx-fitness agc-radio-button-panel
     :algorithm :kpx
     :property :fitness
     :plural :es
     :help-keys *kpx-fitnesses-help-keys*
     :reader kpx-fitness)
   (kpx-line-penalty agc-slider
     :algorithm :kpx
     :property :line-penalty
     :reader kpx-line-penalty)
   (kpx-hyphen-penalty agc-slider
     :algorithm :kpx
     :property :hyphen-penalty
     :reader kpx-hyphen-penalty)
   (kpx-explicit-hyphen-penalty agc-slider
     :algorithm :kpx
     :property :explicit-hyphen-penalty
     :reader kpx-explicit-hyphen-penalty)
   (kpx-adjacent-demerits agc-slider
     :algorithm :kpx
     :property :adjacent-demerits
     :reader kpx-adjacent-demerits)
   (kpx-double-hyphen-demerits agc-slider
     :algorithm :kpx
     :property :double-hyphen-demerits
     :reader kpx-double-hyphen-demerits)
   (kpx-final-hyphen-demerits agc-slider
     :algorithm :kpx
     :property :final-hyphen-demerits
     :reader kpx-final-hyphen-demerits)
   (kpx-similar-demerits agc-slider
     :algorithm :kpx
     :property :similar-demerits
     :reader kpx-similar-demerits)
   (kpx-pre-tolerance agc-slider
     :algorithm :kpx
     :property :pre-tolerance
     :reader kpx-pre-tolerance)
   (kpx-tolerance agc-slider
     :algorithm :kpx
     :property :tolerance
     :reader kpx-tolerance)
   (kpx-emergency-stretch agc-dimen-slider
     :algorithm :kpx
     :property :emergency-stretch
     :reader kpx-emergency-stretch)
   (kpx-looseness agc-slider
     :algorithm :kpx
     :property :looseness
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
     :print-function 'title-capitalize
     :selection-callback 'set-disposition
     :retract-callback 'set-disposition
     :reader disposition-options-panel)
   (features check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :visible-max-width nil
     :visible-max-height nil
     :items *lineup-features*
     :print-function 'title-capitalize
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
   (layout--1 push-button
     :text "<"
     :data #'1-
     :callback 'next-layout
     :help-key :layout--1
     :reader layout--1)
   (layout-+1 push-button
     :text ">"
     :data #'1+
     :callback 'next-layout
     :help-key :layout-+1
     :reader layout-+1)
   (clues check-button-panel
     :layout-class 'column-layout
     :title "Characters and Clues" :title-position :frame
     :visible-max-width nil
     :visible-max-height nil
     :items '(:characters :hyphenation-points
	      :over/underfull-boxes :overshrunk/stretched-boxes
	      :rivers
	      :paragraph-box :line-boxes :character-boxes :baselines
	      :properties-tooltips)
     :print-function 'title-capitalize
     :selection-callback 'set-clues
     :retract-callback 'set-clues
     :reader clues)
   (text-button popup-menu-button
     :text "Source text" :menu text-menu :reader text-button)
   (language-button popup-menu-button
     :text "Language" :menu language-menu :reader language-button)
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
     :input-model '((:motion motion-callback)
		    (:post-menu post-menu-callback))))
  (:layouts
   (main column-layout '(settings view))
   (settings row-layout '(settings-1 settings-2))
   (settings-1 column-layout '(options paragraph-width zoom layouts-ctrl)
     :reader settings-1)
   (layouts-ctrl row-layout '(layout--1 layout-+1)
     :reader layouts-ctrl)
   (options row-layout '(options-1 options-2))
   (options-1 column-layout '(disposition disposition-options features)
     :reader options-1)
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
  (setf (slot-value (river-detection-panel etap) 'main-interface) etap)
  ;; #### NOTE: this menu's selection is updated on pop-up.
  (setf (menu-items (slot-value etap 'language-menu))
	(list (make-instance 'menu-component
		:items (mapcar #'car *languages*)
		:interaction :single-selection
		:print-function 'title-capitalize
		:callback 'language-menu-callback
		:popup-callback 'language-menu-popup-callback)))
  (setf (titled-object-title (zoom etap))
	(format nil "Paragraph zoom: ~3D%" zoom))
  (setf (range-slug-start (zoom etap)) zoom)
  (setf (choice-selected-items (clues etap)) clues)
  (setf (tab-layout-visible-child-function (algorithms etap))
	(lambda (item)
	  (algorithms-tab-layout-visible-child-function item etap))))

(defmethod enable-interface ((interface etap) &optional (enabled t))
  "Change ETAP INTERFACE's enabled status.
The zooming and clues controls are always enabled.
The only interface controls which are subject to enabling / disabling are
those which may affect the typesetting."
  (setf (simple-pane-enabled (paragraph-width interface)) enabled)
  (enable-pane (layouts-ctrl interface) enabled)
  (enable-pane (options-1 interface) enabled)
  (enable-pane (settings-2 interface) enabled)
  (setf (enabled interface) enabled))

(defmethod river-detection-p ((interface etap))
  "Return T if river detection is enabled in INTERFACE."
  (river-detection-p (river-detection-panel interface)))


;; Interface display

(defun selected-items (options choices)
  "Collect the CHOICES being true in OPTIONS."
  (loop :for option :across choices ; collection items are in a vector
	:when (getf options option)
	  :collect option))

(defun set-choice-selection (pane options)
  "Set PANE's choice selection to the choices being true in OPTIONS."
  (setf (choice-selected-items pane)
	(selected-items options (collection-items pane))))

(defun update-interface (interface &aux (context (context interface)))
  "Update INTERFACE after a context change."
  (let ((algorithm (algorithm-type (algorithm context)))
	(options (algorithm-options (algorithm context))))
    (macrolet
	((set-variant (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-VARIANT"))))
	     `(setf (choice-selected-item (,accessor interface))
		    (or (cadr (member :variant options))
			(svref (collection-items (,accessor interface)) 0)))))
	 (set-fallback (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-FALLBACK"))))
	     `(setf (choice-selected-item (,accessor interface))
		    (or (cadr (member :fallback options))
			(svref (collection-items (,accessor interface)) 0)))))
	 (set-options (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-OPTIONS"))))
	     `(set-choice-selection (,accessor interface) options)))
	 (set-slider (alg prop)
	   (let* ((accessor (intern (concatenate 'string
				      (symbol-name alg)
				      "-"
				      (symbol-name prop))))
		  (the-slider (gensym "SLIDER")))
	     `(let ((,the-slider (,accessor interface)))
		(setf (range-slug-start ,the-slider)
		      (or (cadr (member ,prop options))
			  (caliber-default (caliber ,the-slider))))
		(update-agc-slider-title ,the-slider))))
	 (set-sliders (alg &rest sliders)
	   `(progn ,@(mapcar (lambda (slider) `(set-slider ,alg ,slider))
		       sliders)))
	 (set-choice (alg prop)
	   (let ((accessor
		   (intern (concatenate 'string
			     (symbol-name alg)
			     "-"
			     (symbol-name prop)))))
	     `(setf (choice-selected-item (,accessor interface))
		    (or (cadr (member ,prop options))
			(svref (collection-items (,accessor interface)) 0))))))
      (case algorithm
	(:fixed
	 (setf (choice-selection (algorithms interface)) 0)
	 (set-fallback fixed)
	 (set-options fixed)
	 (set-slider fixed :width-offset))
	(:fit
	 (setf (choice-selection (algorithms interface)) 1)
	 (set-variant fit)
	 (set-fallback fit)
	 (set-options fit)
	 (set-choice fit :discriminating-function)
	 (set-sliders fit
	   :width-offset
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty))
	(:barnett
	 (setf (choice-selection (algorithms interface)) 2))
	(:duncan
	 (setf (choice-selection (algorithms interface)) 3)
	 (set-choice duncan :discriminating-function))
	(:knuth-plass
	 (setf (choice-selection (algorithms interface)) 4)
	 (set-variant kp)
	 (set-sliders kp
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty
	   :adjacent-demerits :double-hyphen-demerits :final-hyphen-demerits
	   :pre-tolerance :tolerance :emergency-stretch :looseness))
	(:kpx
	 (setf (choice-selection (algorithms interface)) 5)
	 (set-variant kpx)
	 (setf (choice-selected-item (kpx-fitness interface))
	       (or (cadr (member :fitness options)) (car *kpx-fitnesses*)))
	 (set-sliders kpx
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty
	   :adjacent-demerits :double-hyphen-demerits :final-hyphen-demerits
	   :similar-demerits
	   :pre-tolerance :tolerance :emergency-stretch :looseness)))))
  (setf (choice-selected-item (disposition interface))
	(disposition-type (disposition context)))
  (set-choice-selection (disposition-options-panel interface)
			(disposition-options (disposition context)))
  (set-choice-selection (features interface) (features context))
  (setf (range-slug-start (paragraph-width interface))
	(paragraph-width context))
  (setf (titled-object-title (paragraph-width interface))
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  (paragraph-width context) (/ (paragraph-width context) 28.452755)))
  (setf (editor-pane-text (text interface)) (text context))
  (values))

;; #### NOTE: I'm not sure, but I suppose that twiddling with the geometry is
;; better done here than in an INITIALIZE-INSTANCE :after method.
(defmethod interface-display :before ((etap etap))
  "Finalize ETAP GUI's display settings.
This currently involves fixating the geometry of option panes so that resizing
the interface is done sensibly."
  (let ((size (multiple-value-list
	       (simple-pane-visible-size (settings-1 etap)))))
    (set-hint-table (settings-1 etap)
      `(:visible-min-width ,(car size) :visible-max-width t
	:visible-min-height ,(cadr size) :visible-max-height t)))
  (let ((size (multiple-value-list
	       (simple-pane-visible-size (settings-2 etap)))))
    (set-hint-table (settings-2 etap)
      `(:visible-min-height ,(cadr size) :visible-max-height t)))
  (update-interface etap))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

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
	     :destroy-callback
	     (lambda (interface)
	       (destroy (river-detection-panel interface))
	       (mapc #'destroy (penalty-adjustment-dialogs interface))))))
