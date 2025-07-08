(in-package :etap)

;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### NOTE: SIMPLE-PANE-ENABLED doesn't do what I would expect on interfaces
;; or layouts (namely, to recursively enable or disable its components), so we
;; need to do it by hand..
(defgeneric enable-interface (interface &optional enabled)
  (:documentation "Set INTERFACE's enabled status to ENABLED (T by default)."))


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


(defun calibrated-value (value caliber)
  "Depending on CALIBER, potentially convert VALUE to +∞/-∞."
  (cond ((and (= value (caliber-min caliber))
	      (member (caliber-infinity caliber) '(t :min)))
	 -∞)
	((and (= value (caliber-max caliber))
	      (member (caliber-infinity caliber) '(t :max)))
	 +∞)
	(t value)))

(defun uncalibrated-value (value caliber)
  "Depending on CALIBER, potentially convert an infinity VALUE to min/max."
  (cond ((eq value +∞) (caliber-max caliber))
	((eq value -∞) (caliber-min caliber))
	(t value)))


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


;; --------
;; Updaters
;; --------

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

(defun remake-paragraph (interface &rest args)
  "Remake INTERFACE's paragraph. ARGS are passed along to MAKE-PARAGRAPH."
  (let* ((paragraph
	   (apply #'make-paragraph :context (context interface) args))
	 (layouts-# (layouts-# (breakup paragraph))))
    (setf (paragraph interface) paragraph)
    (setf (layout interface) (if (zerop layouts-#) 0 1))
    (setf (titled-object-title (view interface))
	  (format nil "Layout ~D/~D" (layout interface) layouts-#))))

(defun update (interface &rest args)
  "Update INTERFACE.
This remakes INTERFACE's paragraph, everything that depends on it,
and invalidates the view.
ARGS are passed along to MAKE-PARAGRAPH."
  (apply #'remake-paragraph interface args)
  (remake-rivers interface)
  (gp:invalidate-rectangle (view interface)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun title-capitalize (title)
    "Capitalize TITLE and substitute dashes with spaces."
    (nsubstitute #\Space #\- (string-capitalize title))))




;; ==========================================================================
;; GUI Constituents
;; ==========================================================================

;; ------------------------
;; Algorithm GUI Components
;; ------------------------

(defclass agc ()
  ((algorithm
    :documentation "This slider's algorithm."
    :initarg :algorithm :reader algorithm))
  (:default-initargs :title "Dummy")
  (:documentation "The Algorithm GUI Component class.
This class serves as a mixin for GUI components representing algorithmic
settings."))

;; #### WARNING: CAPI sliders impose a specific signature on callbacks (3
;; mandatory arguments) so we need to conform to that if we want to use a
;; single generic function for all kinds of GUI components.
(defgeneric agc-callback (component arg2 arg3)
  (:documentation "The callback for all algorithm GUI components.")
  (:method ((component agc) arg2 arg3
	    &aux (interface (top-level-interface component)))
    "Update algorithmic settings in COMPONENT's interface context."
    (select-algorithm (algorithm component) interface))
  (:method :after ((component agc) arg2 arg3
		   &aux (interface (top-level-interface component)))
    "Update COMPONENT's interface."
    (update interface)))


;; -------
;; Sliders
;; -------

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
  "Update AGC SLIDER's title."
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


(defmethod agc-callback :before ((slider agc-slider) how where)
  "Update AGC SLIDER's title."
  (declare (ignore how where))
  (update-agc-slider-title slider))


(defmacro agc-slider-setting (algorithm property interface)
  "Return (:PROPERTY (RANGE-SLUG-START (ALGORITHM-PROPERTY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (symbol-name algorithm)
			    "-"
			    (symbol-name property)))))
    `(list ,property (range-slug-start (,accessor ,interface)))))


;; Dimension sliders

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


;; -------------------
;; AGC Button Panels
;; -------------------

(defclass agc-button-panel (agc)
  ()
  (:default-initargs
   :title-position :frame
   :layout-class 'column-layout
   :visible-max-height nil
   :callback-type '(:element :data :interface) ; see comment atop AGC
   :selection-callback 'agc-callback)
  (:documentation "The Algorithm GUI Component Button Panel class.
This is the mixin class for AGC radio and check button panels."))


;; Radio Button Panels

(defclass agc-radio-button-panel (agc-button-panel radio-button-panel)
  ()
  (:default-initargs
   :print-function 'title-capitalize)
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
  (:default-initargs
   :print-function (lambda (item) (title-capitalize (car item)))
   :retract-callback   'agc-callback)
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
		(apply #'append
		  (agc-radio-setting fixed :fallback interface)
		  (agc-slider-setting fixed :width-offset interface)
		  (choice-selected-items (fixed-options interface))))))
  (:method ((algorithm (eql :fit)) interface)
    "Select the Fit algorithm  in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :fit
		(apply #'append
		  (agc-radio-setting fit :variant interface)
		  (agc-radio-setting fit :fallback interface)
		  (agc-radio-setting fit :discriminating-function interface)
		  (agc-slider-setting fit :line-penalty interface)
		  (agc-slider-setting fit :hyphen-penalty interface)
		  (agc-slider-setting fit :explicit-hyphen-penalty interface)
		  (agc-slider-setting fit :width-offset interface)
		  (choice-selected-items (fit-options interface))))))
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

;; #### NOTE: this callback does nothing when the interface is virtually
;; disabled.
(defun set-algorithm (value interface)
  "Set algorithm specified by VALUE in INTERFACE."
  (if (enabled interface)
    (let ((algorithm (car value)))
      ;; #### WARNING: hack alert. The Knuth-Plass prefix is :kp throughout,
      ;; except that it's :knuth-plass in contexts, and also in the interface
      ;; algorithm selection pane where the title needs to be human readable.
      ;; Hence the title conversion below.
      (when (eq algorithm :knuth-plass) (setq algorithm :kp))
      (select-algorithm algorithm interface)
      (update interface))
    (let* ((algorithms-tab-layout (algorithms interface))
	   (old-selection
	     (position (algorithm-type (algorithm (context interface)))
		       (collection-items algorithms-tab-layout)
		       :key #'car))
	   (new-selection (choice-selection algorithms-tab-layout)))
      ;; Blindly restoring the old selection would probably lead to an
      ;; infinite loop in the callback, so better be clever here...
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
		:if (> (width line) (width paragraph))
		  :do (gp:draw-rectangle pane
			  full-x  (- y (height line))
			  5  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr rest) ;; not the last one
			       (eq (disposition-type (disposition paragraph))
				   :justified)
			       (< (width line) (width paragraph)))
		  :do (gp:draw-rectangle pane
			  full-x (- y (height line))
			  5 (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if ($< (esar line) (asar line))
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(- y (height line))
				(+ (width paragraph) 11)
				(- y (height line))
				(+ (width paragraph) 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled t :closed t)
		:else :if ($< (asar line) -1)
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(- y (height line))
				(+ (width paragraph) 11)
				(- y (height line))
				(+ (width paragraph) 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled nil :closed t)
		:else :if ($> (esar line) (asar line))
		  :do (gp:draw-polygon pane
			  (list (+ (width paragraph) 5)
				(+ y (depth line))
				(+ (width paragraph) 11)
				(+ y (depth line))
				(+ (width paragraph) 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($> (asar line) 1)
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
				      1s0 .7s0)))
				  ((whitespacep item)
				   (when (member :river-beds clues)
				     (gp:draw-circle
				      pane
				      (+ x (x item) (/ (width item) 2)) y 1
				      :filled t :foreground :red)))))
		      (items line)))
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


;; -------
;; Layouts
;; -------

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
	  (paragraph (paragraph interface))
	  (layout-# (let ((i (1- (layout interface)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# (breakup paragraph)))))
  "Display the properties of the paragraph, or the line clicked on."
  (when (member :properties-tooltips (choice-selected-items (clues interface)))
    (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
    ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
    ;; returning 0, but this is borderline.
    (decf y (height layout))
    (if (or (and (<= x 0) (<= y (depth layout)))
	    (and (<= y (- (height layout))) (<= x (width paragraph))))
      (display-tooltip pane :text (properties paragraph :layout-# layout-#))
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
			     (<= x (+ (width paragraph) 3))
			     (>= y 0) ; no need to look above the 1st line
			     (<= y (+ (y (car (last (lines layout)))) 5))
			     (hyphenation-point-under x y (lines layout)))
			(and (>= x 0) (<= x (width paragraph))
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
	  (hyphenation-point (hyphenation-point interface))
	  (context (context main-interface)))
  "Set PANE's corresponding break point penalty."
  (declare (ignore status))
  (setq value
	(calibrated-value (range-slug-start pane) (caliber hyphenation-point)))
  (setf (title-pane-text (title interface)) (princ-to-string value))
  (setf (penalty hyphenation-point) value)
  (let ((lineup (lineup (paragraph main-interface))))
    (update main-interface
	    :hlist (hlist (paragraph main-interface))
	    :lineup lineup
	    :breakup (%make-breakup lineup
				    (disposition context)
				    (paragraph-width context)
				    (algorithm context)))))

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

;; #### FIXME: since this interface is not currently used as a dialog, it
;; remains on the screen, which is wrong if anything is changed underneath
;; (algorithm, text, etc.)
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
	(uncalibrated-value
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


(defun make-penalty-adjustment (hyphenation-point interface)
  "Create and display a penalty adjustment dialog for HYPHENATION-POINT.
INTERFACE is the main ETAP window."
  (let ((dialog (make-instance 'penalty-adjustment
		  :hyphenation-point hyphenation-point
		  :main-interface interface
		  :destroy-callback (lambda (dialog)
				      (declare (ignore dialog))
				      (enable-interface interface)))))
    (multiple-value-bind (x y) (top-level-interface-geometry interface)
      (set-top-level-interface-geometry dialog :x (+ x 200) :y (+ y 200)))
    (display dialog
      :owner interface
      :window-styles '(:toolbox t
		       :never-iconic t
		       :always-on-top t
		       :can-full-screen nil))
    (enable-interface interface nil)))


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
	  (paragraph (paragraph interface))
	  (layout-# (let ((i (1- (layout interface)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# (breakup paragraph)))))
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
		       (<= x (+ (width paragraph) 3))
		       (>= y 0) ; no need to look above the 1st line
		       (<= y (+ (y (car (last (lines layout)))) 5))
		       (hyphenation-point-under x y (lines layout)))))
      (when object
	(make-penalty-adjustment object interface)))))


;; ----------------
;; Rivers detection
;; ----------------

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


;; -----
;; Menus
;; -----

(defun etap-menu-callback (data interface)
  "ETAP menu callback."
  (ecase data
    (:reset-paragraph
     (update interface))
    (:rivers-detection
     (display (rivers-interface interface) :owner interface))))

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

;; #### NOTE: as there's apparently no way to disable a tab layout, we need to
;; emulate this be simply doing nothing on clicks.
(defun algorithms-tab-layout-visible-child-function (item interface)
  "Return the appropriate algorithm pane.
This is either ITEM's second element if the interface is enabled,
or the current algorithm's one otherwise."
  (if (enabled interface)
    (second item)
    (second (find (algorithm-type (algorithm (context interface)))
		(collection-items (algorithms interface))
	      :key #'car))))

(define-interface etap ()
  ((context :initform *context* :initarg :context :reader context)
   (paragraph :accessor paragraph)
   (layout :initform 0 :accessor layout)
   (enabled :initform t :accessor enabled)
   (rivers :documentation "The paragraph's detected rivers."
	   :initform nil
	   :accessor rivers)
   (rivers-interface :initform (make-instance 'rivers-detection)
		     :reader rivers-interface))
  (:menus
   (etap-menu "ETAP" (:reset-paragraph :rivers-detection)
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
	      :paragraph-box :line-boxes :character-boxes :baselines
	      :over/underfull-boxes :overshrunk/stretched-boxes
	      :properties-tooltips :river-beds)
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
   (layouts-ctrl row-layout '(layout--1 layout-+1))
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
  (setf (choice-selected-items (clues etap)) clues)
  (setf (tab-layout-visible-child-function (algorithms etap))
	(lambda (item)
	  (algorithms-tab-layout-visible-child-function item etap))))

(defmethod enable-interface ((interface etap) &optional (enabled t))
  "Change ETAP INTERFACE's enabled status."
  (setf (enabled interface) enabled)
  (setf (simple-pane-enabled (disposition interface)) enabled
	(simple-pane-enabled (disposition-options-panel interface)) enabled
	(simple-pane-enabled (features interface)) enabled
	(simple-pane-enabled (paragraph-width interface)) enabled
	(simple-pane-enabled (layout--1 interface)) enabled
	(simple-pane-enabled (layout-+1 interface)) enabled
	(simple-pane-enabled (clues interface)) enabled
	(simple-pane-enabled (text-button interface)) enabled
	(simple-pane-enabled (language-button interface)) enabled
	(simple-pane-enabled (text interface)) enabled))


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
	((set-variant (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-VARIANT")))
		 (choices (intern (concatenate 'string
				    "*" (symbol-name alg) "-VARIANTS*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member :variant options)) (car ,choices)))))
	 (set-fallback (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-FALLBACK")))
		 (choices (intern (concatenate 'string
				    "*" (symbol-name alg) "-FALLBACKS*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member :fallback options)) (car ,choices)))))
	 (set-options (alg)
	   (let ((accessor (intern (concatenate 'string
				     (symbol-name alg) "-OPTIONS")))
		 (choices (intern (concatenate 'string
				    "*" (symbol-name alg) "-OPTIONS*"))))
	     `(set-choice-selection (,accessor etap) options ,choices)))
	 (set-slider (alg prop)
	   (let* ((accessor (intern (concatenate 'string
				      (symbol-name alg)
				      "-"
				      (symbol-name prop))))
		  (caliber (intern (concatenate 'string
				     "*"
				     (symbol-name alg)
				     "-"
				     (symbol-name prop)
				     "*")))
		  (the-slider (gensym "SLIDER")))
	     `(let ((,the-slider (,accessor etap)))
		(setf (range-slug-start ,the-slider)
		      (or (cadr (member ,prop options))
			  (caliber-default ,caliber)))
		(update-agc-slider-title ,the-slider))))
	 (set-sliders (alg &rest sliders)
	   `(progn ,@(mapcar (lambda (slider) `(set-slider ,alg ,slider))
		       sliders)))
	 (set-choice (alg prop)
	   (let ((accessor
		   (intern (concatenate 'string
			     (symbol-name alg)
			     "-"
			     (symbol-name prop))))
		 (choices
		   (intern (concatenate 'string
			     "*"
			     (symbol-name alg)
			     "-"
			     (symbol-name prop)
			     "S*"))))
	     `(setf (choice-selected-item (,accessor etap))
		    (or (cadr (member ,prop options)) (car ,choices))))))
      (case algorithm
	(:fixed
	 (setf (choice-selection (algorithms etap)) 0)
	 (set-fallback fixed)
	 (set-options fixed)
	 (set-slider fixed :width-offset))
	(:fit
	 (setf (choice-selection (algorithms etap)) 1)
	 (set-variant fit)
	 (set-fallback fit)
	 (set-options fit)
	 (set-choice fit :discriminating-function)
	 (set-sliders fit
	   :width-offset
	   :line-penalty :hyphen-penalty :explicit-hyphen-penalty))
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
	     :destroy-callback (lambda (interface)
				 (destroy (rivers-interface interface))))))
