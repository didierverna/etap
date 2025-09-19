(in-package :etap)

(defparameter *clues*
  '(:characters :hyphenation-points
    :over/underfull-boxes :overshrunk/stretched-boxes
    :rivers
    :paragraph-box :line-boxes :character-boxes :baselines
    :properties-tooltips)
  "The visual clues available for conditional display.")




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

(define-gui-caliber zoom 10 100 500)


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
;; Widgets
;; ==========================================================================

;; Our specific widgets currently include radio boxes, check boxes, and
;; cursors. They are all associated with a property name (used in the title),
;; and they provide a two-way translation mechanism to and from property
;; lists.

;; #### WARNING: dynamically setting a widget title in the initialization
;; after method doesn't make the title appear (nore the widget's frame),
;; unless a dummy title is specified below.
(defclass widget ()
  ((property :documentation "This widget's property (a keyword)."
	     :initarg :property :reader property))
  (:default-initargs :title "Dummy")
  (:documentation "The Widget class.
This class is a mixin class for ETAP widgets."))

(defgeneric widget-state (widget)
  (:documentation "Return a property list representing WIDGET's state."))

(defgeneric (setf widget-state) (plist widget)
  (:documentation "Set WIDGET's state based on PLIST."))



;; ------------
;; Button Boxes
;; ------------

(defclass button-box (widget)
  ()
  (:default-initargs
   :title-position :frame
   :layout-class 'column-layout
   :visible-max-height nil
   :print-function 'title-capitalize)
  (:documentation "The Button Box class.
This is the base class for radio and check boxes."))

(defmethod initialize-instance :after ((box button-box) &key)
  "Set button BOX's title to <Property>."
  (setf (titled-object-title box) (title-capitalize (property box))))



;; Radio Boxes

(defclass radio-box (button-box radio-button-panel)
  ()
  (:documentation "The Radio Box class."))

(defmethod widget-state ((box radio-box))
  "Return a property list representing radio BOX's state.
This is a list of the form (<property> <selected item>)."
  (list (property box) (choice-selected-item box)))

;; #### NOTE: the reason we have two methods below is because we have two ways
;; of using radio boxes.
;; 1. The value can be in the middle of a plist, for example, in an algorithm
;;    specification: (... :fallback :anyfull ...). There, the method on lists
;;    applies.
;; 2. The value can also be specific, for example in the case of a
;;    disposition which is extracted by calling DISPOSITION-TYPE.

(defmethod (setf widget-state) ((plist list) (box radio-box))
  "Set radio BOX's state based on PLIST.
More specifically, BOX's selection is set to the value of BOX's property
in PLIST if found (the value must be one of BOX's items). Otherwise, the
first item in BOX is selected."
  (setf (choice-selected-item box)
	(or (getf plist (property box)) (svref (collection-items box) 0))))

(defmethod (setf widget-state) ((item symbol) (box radio-box))
  "Set radio BOX's selected item to ITEM (must be one of BOX's items)."
  (setf (choice-selected-item box) item))



;; Check Boxes

(defclass check-box (button-box check-button-panel)
  ()
  (:documentation "The Check Box class."))

(defmethod widget-state ((box check-box))
  "Return a property list representing check BOX's state.
This is a list of the form (<property> <item> <state> ...).
Note that the list is exhaustive: all BOX items are present, with their state
being T or NIL."
  (cons (property box)
	(loop :with selection := (choice-selected-items box)
	      :for item :across (collection-items box) ; a vector
	      :collect item
	      :if (member item selection)
		:collect t
	      :else
		:collect nil)))

(defmethod (setf widget-state) (plist (box check-box))
  "Set check BOX's state based on PLIST.
More specifically, every BOX item found to be true in PLIST is selected. The
rest is deselected (i.e., no items are left in their previous state)."
  (setf (choice-selected-items box)
	(loop :for item :across (collection-items box) ; a vector
	      :when (getf plist item)
		:collect item)))



;; Cursors

(defclass cursor (widget slider)
  ((caliber
    :documentation "This cursor's corresponding caliber."
    :initarg :caliber :reader caliber))
  (:default-initargs
   :tick-frequency 0
   :orientation :horizontal
   :visible-min-width 220)
  (:documentation "The Cursor class."))


(defgeneric cursor-title (cursor)
  (:documentation "Compute CURSOR's title based on its current value.")
  (:method ((cursor cursor))
    "Return a string of the form \"<Property>: <calibrated value>\".
This is the default method."
    (format nil "~A: ~A"
      (title-capitalize (property cursor))
      (calibrated-cursor-value cursor))))

(defun update-cursor-title (cursor)
  "Update CURSOR's title with its current value."
  (setf (titled-object-title cursor) (cursor-title cursor)))

;; #### NOTE: it's important to initialize the value and title because
;; otherwise, only the initial algorithm's widgets would have those settings
;; properly set.
(defmethod initialize-instance :after
    ((cursor cursor) &key &aux (caliber (caliber cursor)))
  "Initialize CURSOR based on its caliber.
This means setting its range start and end, default value, and title."
  (setf (range-start cursor) (caliber-min caliber)
	(range-end cursor)   (caliber-max caliber))
  (setf (widget-state cursor) nil))


(defun calibrated-cursor-value (cursor)
  "Return the calibrated current CURSOR value."
  (calibrated-value (range-slug-start cursor) (caliber cursor)))


(defmethod widget-state ((cursor cursor))
  "Return a property list representing CURSOR's state.
This is a list of the form (<property> <calibrated value>)."
  (list (property cursor) (calibrated-cursor-value cursor)))

(defmethod (setf widget-state)
    (plist (cursor cursor) &aux (caliber (caliber cursor)))
  "Set CURSOR's state based on PLIST.
More specifically, CURSOR's value is set to the decalibrated value of CURSOR's
property in PLIST if found. Otherwise, the default value of CURSOR's caliber
is used. CURSOR's title is updated accordingly."
  (setf (range-slug-start cursor)
	(decalibrated-value (or (getf plist (property cursor))
				(caliber-default caliber))
			    caliber))
  (update-cursor-title cursor))



;; Percentage cursors

(defclass %-cursor (cursor)
  ()
  (:documentation "The Percentage Cursor class."))

;; #### TODO: ~3D in the format string below will not like +/-∞ if one day we
;; use percentage cursors with infinity handling calibers. This just doesn't
;; happen for now.
(defmethod cursor-title ((cursor %-cursor))
  "Return a string of the form \"<Property>: <calibrated value>%\".
The calibrated value is displayed with 3 digits."
  (format nil "~A: ~3D%"
    (title-capitalize (property cursor))
    (calibrated-cursor-value cursor)))



;; Dimension (pt) cursors

(defclass pt-cursor (cursor)
  ()
  (:documentation "The Dimension (pt) Cursor class."))

(defmethod cursor-title
    ((cursor pt-cursor) &aux (value (calibrated-cursor-value cursor)))
  "Return a string of the form \"<Property>: <calibrated value>pt (<in>cm)\".
The cm equivalent part is not displayed if the value is +/-∞."
  (format nil "~A: ~A~@[pt (~Acm)~]"
    (title-capitalize (property cursor))
    value
    (when (numberp value) (float (/ value 28.452755)))))




;; ==========================================================================
;; Interface Actions
;; ==========================================================================

;; -------------------
;; Algorithm Selection
;; -------------------

(defgeneric %select-algorithm (algorithm interface)
  (:documentation "Select ALGORITHM in INTERFACE's context.")
  (:method ((algorithm (eql :fixed)) interface)
    "Select the Fixed algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :fixed
		(append
		 (widget-state (fixed-fallback interface))
		 (cdr (widget-state (fixed-options interface)))
		 (widget-state (fixed-width-offset interface))))))
  (:method ((algorithm (eql :fit)) interface)
    "Select the Fit algorithm  in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :fit
		(append
		 (widget-state (fit-variant interface))
		 (widget-state (fit-fallback interface))
		 (widget-state (fit-discriminating-function interface))
		 (cdr (widget-state (fit-options interface)))
		 (widget-state (fit-line-penalty interface))
		 (widget-state (fit-hyphen-penalty interface))
		 (widget-state (fit-explicit-hyphen-penalty interface))
		 (widget-state (fit-width-offset interface))))))
  (:method ((algorithm (eql :barnett)) interface)
    "Select the Barnett algorithm in INTERFACE's context."
    (setf (algorithm (context interface)) '(:barnett)))
  (:method ((algorithm (eql :duncan)) interface)
    "Select the Duncan algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :duncan
		(widget-state (duncan-discriminating-function interface)))))
  (:method ((algorithm (eql :kp)) interface)
    "Select the Knuth-Plass algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :knuth-plass
		(append
		 (widget-state (kp-variant interface))
		 (widget-state (kp-line-penalty interface))
		 (widget-state (kp-hyphen-penalty interface))
		 (widget-state (kp-explicit-hyphen-penalty interface))
		 (widget-state (kp-adjacent-demerits interface))
		 (widget-state (kp-double-hyphen-demerits interface))
		 (widget-state (kp-final-hyphen-demerits interface))
		 (widget-state (kp-pre-tolerance interface))
		 (widget-state (kp-tolerance interface))
		 (widget-state (kp-emergency-stretch interface))
		 (widget-state (kp-looseness interface))))))
  (:method ((algorithm (eql :kpx)) interface)
    "Select the KPX algorithm in INTERFACE's context."
    (setf (algorithm (context interface))
	  (cons :kpx
		(append
		 (widget-state (kpx-variant interface))
		 (widget-state (kpx-fitness interface))
		 (widget-state (kpx-line-penalty interface))
		 (widget-state (kpx-hyphen-penalty interface))
		 (widget-state (kpx-explicit-hyphen-penalty interface))
		 (widget-state (kpx-adjacent-demerits interface))
		 (widget-state (kpx-double-hyphen-demerits interface))
		 (widget-state (kpx-final-hyphen-demerits interface))
		 (widget-state (kpx-similar-demerits interface))
		 (widget-state (kpx-pre-tolerance interface))
		 (widget-state (kpx-tolerance interface))
		 (widget-state (kpx-emergency-stretch interface))
		 (widget-state (kpx-looseness interface)))))))

(defun select-algorithm
    (interface
     &aux (algorithm (first (choice-selected-item (algorithms-tab interface)))))
  "Select INTERFACE's algorithm."
  ;; #### WARNING: hack alert. The Knuth-Plass prefix is :kp throughout,
  ;; except that it's :knuth-plass in contexts, and also in the interface
  ;; algorithm selection pane where the title needs to be human readable.
  ;; Hence the title conversion below.
  (when (eq algorithm :knuth-plass) (setq algorithm :kp))
  (%select-algorithm algorithm interface))

(defun algorithm-callback (interface)
  "Select INTERFACE's algorithm and update everything."
  (select-algorithm interface)
  (update interface))

;; #### WARNING: moving a slider with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that I can safely ignore :MOVE callbacks which means saving two
;; calls out of 3! I will need to check this again when I introduce focus and
;; keyboard control though.
(defun algorithm-cursor-callback (cursor value gesture)
  "Update CURSOR's title, select INTERFACE's algorithm, and update everything."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (let ((interface (top-level-interface cursor)))
      (select-algorithm interface)
      (update interface))))


;; ---------
;; Callbacks
;; ---------

(defun algorithms-tab-callback (tab interface)
  "If INTERFACE is enabled, set algorithm to the selected one in TAB.
Otherwise, reselect the previously selected one."
  (cond ((enabled interface)
	 (select-algorithm interface)
	 (update interface))
	(t
	 (setf (choice-selected-item tab)
	       (find (algorithm-type (algorithm (context interface)))
		   (collection-items tab)
		 :key #'first)))))

(defun set-disposition (value interface)
  "Set the current disposition in INTERFACE's context."
  (declare (ignore value))
  (setf (disposition (context interface))
	(cons (second (widget-state (disposition interface)))
	      (cdr (widget-state (disposition-options-panel interface)))))
  (update interface))

(defun set-features (value interface)
  "Set the current features in INTERFACE's context."
  (declare (ignore value))
  (setf (features (context interface))
	(cdr (widget-state (features interface))))
  (update interface))

(defun set-text (pane point old-length new-length
		 &aux (interface (top-level-interface pane)))
  "Set editor PANE's current text in PANE's context."
  (declare (ignore point old-length new-length))
  (setf (text (nlstring (context interface))) (editor-pane-text pane))
  (update interface))

;; #### WARNING: moving the slider with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that I can safely ignore :MOVE callbacks which means saving two
;; calls out of 3! I will need to check this again when I introduce focus and
;; keyboard control though.
(defun paragraph-width-callback
    (cursor value gesture &aux (interface (top-level-interface cursor)))
  "Update paragraph width CURSOR's title and re-break the current lineup."
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (setf (paragraph-width (context interface)) value)
    (update-from-lineup interface)))

;; #### WARNING: moving the slider with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that I can safely ignore :MOVE callbacks which means saving two
;; calls out of 3! I will need to check this again when I introduce focus and
;; keyboard control though.
(defun zoom-callback (cursor value gesture)
  "Update zoom CURSOR's title and invalidate the paragraph view."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (gp:invalidate-rectangle (view (top-level-interface cursor)))))

(defun clues-callback (interface)
  "Invalidate INTERFACE's paragraph view."
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
   (algorithms-tab tab-layout
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
     :callback-type '(:element :interface)
     :selection-callback 'algorithms-tab-callback
     ;; #### WARNING: with my emulation of an enabled/disabled status for the
     ;; main interface, the algorithms tab's selection callback may override
     ;; the selection that triggered its call. However, even though the
     ;; visible child function is called afterwards, the item passed along is
     ;; the old one (probably bound before the execution of the callback). The
     ;; solution around this is to ignore the (obsolete) item, and work
     ;; directly with the tab's selection.
     :visible-child-function (lambda (item)
			       (declare (ignore item))
			       (second (choice-selected-item algorithms-tab)))
     :reader algorithms-tab)
   (fixed-fallback radio-box
     :property :fallback
     :items *fixed-fallbacks*
     :help-keys *fixed-fallbacks-help-keys*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :reader fixed-fallback)
   (fixed-options check-box
     :property :options
     :items *fixed-options*
     :help-keys *fixed-options-help-keys*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :reader fixed-options)
   (fixed-width-offset pt-cursor
     :property :width-offset
     :caliber *fixed-width-offset*
     :callback 'algorithm-cursor-callback
     :reader fixed-width-offset)
   (fit-variant radio-box
     :property :variant
     :items *fit-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-variants-help-keys*
     :reader fit-variant)
   (fit-fallback radio-box
     :property :fallback
     :items *fit-fallbacks*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-fallbacks-help-keys*
     :reader fit-fallback)
   (fit-discriminating-function radio-box
     :property :discriminating-function
     :items *fit-discriminating-functions*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-discriminating-functions-help-keys*
     :reader fit-discriminating-function)
   (fit-options check-box
     :property :options
     :items *fit-options*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-options-help-keys*
     :reader fit-options)
   (fit-line-penalty cursor
     :property :line-penalty
     :caliber *fit-line-penalty*
     :callback 'algorithm-cursor-callback
     :reader fit-line-penalty)
   (fit-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *fit-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader fit-hyphen-penalty)
   (fit-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *fit-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader fit-explicit-hyphen-penalty)
   (fit-width-offset pt-cursor
     :property :width-offset
     :caliber *fit-width-offset*
     :callback 'algorithm-cursor-callback
     :reader fit-width-offset)
   (duncan-discriminating-function radio-box
     :property :discriminating-function
     :items *duncan-discriminating-functions*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *duncan-discriminating-functions-help-keys*
     :reader duncan-discriminating-function)
   (kp-variant radio-box
     :property :variant
     :items *kp-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kp-variants-help-keys*
     :reader kp-variant)
   (kp-line-penalty cursor
     :property :line-penalty
     :caliber *kp-line-penalty*
     :callback 'algorithm-cursor-callback
     :reader kp-line-penalty)
   (kp-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *kp-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader kp-hyphen-penalty)
   (kp-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *kp-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader kp-explicit-hyphen-penalty)
   (kp-adjacent-demerits cursor
     :property :adjacent-demerits
     :caliber *kp-adjacent-demerits*
     :callback 'algorithm-cursor-callback
     :reader kp-adjacent-demerits)
   (kp-double-hyphen-demerits cursor
     :property :double-hyphen-demerits
     :caliber *kp-double-hyphen-demerits*
     :callback 'algorithm-cursor-callback
     :reader kp-double-hyphen-demerits)
   (kp-final-hyphen-demerits cursor
     :property :final-hyphen-demerits
     :caliber *kp-final-hyphen-demerits*
     :callback 'algorithm-cursor-callback
     :reader kp-final-hyphen-demerits)
   (kp-pre-tolerance cursor
     :property :pre-tolerance
     :caliber *kp-pre-tolerance*
     :callback 'algorithm-cursor-callback
     :reader kp-pre-tolerance)
   (kp-tolerance cursor
     :property :tolerance
     :caliber *kp-tolerance*
     :callback 'algorithm-cursor-callback
     :reader kp-tolerance)
   (kp-emergency-stretch pt-cursor
     :property :emergency-stretch
     :caliber *kp-emergency-stretch*
     :callback 'algorithm-cursor-callback
     :reader kp-emergency-stretch)
   (kp-looseness cursor
     :property :looseness
     :caliber *kp-looseness*
     :callback 'algorithm-cursor-callback
     :reader kp-looseness)
   (kpx-variant radio-box
     :property :variant
     :items *kpx-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kpx-variants-help-keys*
     :reader kpx-variant)
   (kpx-fitness radio-box
     :property :fitness
     :items *kpx-fitnesses*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kpx-fitnesses-help-keys*
     :reader kpx-fitness)
   (kpx-line-penalty cursor
     :property :line-penalty
     :caliber *kpx-line-penalty*
     :callback 'algorithm-cursor-callback
     :reader kpx-line-penalty)
   (kpx-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *kpx-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader kpx-hyphen-penalty)
   (kpx-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *kpx-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback
     :reader kpx-explicit-hyphen-penalty)
   (kpx-adjacent-demerits cursor
     :property :adjacent-demerits
     :caliber *kpx-adjacent-demerits*
     :callback 'algorithm-cursor-callback
     :reader kpx-adjacent-demerits)
   (kpx-double-hyphen-demerits cursor
     :property :double-hyphen-demerits
     :caliber *kpx-double-hyphen-demerits*
     :callback 'algorithm-cursor-callback
     :reader kpx-double-hyphen-demerits)
   (kpx-final-hyphen-demerits cursor
     :property :final-hyphen-demerits
     :caliber *kpx-final-hyphen-demerits*
     :callback 'algorithm-cursor-callback
     :reader kpx-final-hyphen-demerits)
   (kpx-similar-demerits cursor
     :property :similar-demerits
     :caliber *kpx-similar-demerits*
     :callback 'algorithm-cursor-callback
     :reader kpx-similar-demerits)
   (kpx-pre-tolerance cursor
     :property :pre-tolerance
     :caliber *kpx-pre-tolerance*
     :callback 'algorithm-cursor-callback
     :reader kpx-pre-tolerance)
   (kpx-tolerance cursor
     :property :tolerance
     :caliber *kpx-tolerance*
     :callback 'algorithm-cursor-callback
     :reader kpx-tolerance)
   (kpx-emergency-stretch pt-cursor
     :property :emergency-stretch
     :caliber *kpx-emergency-stretch*
     :callback 'algorithm-cursor-callback
     :reader kpx-emergency-stretch)
   (kpx-looseness cursor
     :property :looseness
     :caliber *kpx-looseness*
     :callback 'algorithm-cursor-callback
     :reader kpx-looseness)
   (disposition radio-box
     :property :disposition
     :items *dispositions*
     :selection-callback 'set-disposition
     :reader disposition)
   (disposition-options check-box
     :property :disposition-options
     :items *disposition-options*
     :help-keys *disposition-options-help-keys*
     :selection-callback 'set-disposition
     :retract-callback 'set-disposition
     :reader disposition-options-panel)
   (features check-box
     :property :features
     :items *lineup-features*
     :selection-callback 'set-features
     :retract-callback 'set-features
     :reader features)
   (paragraph-width pt-cursor
     :property :paragraph-width
     :caliber *paragraph-width*
     :callback 'paragraph-width-callback
     :reader paragraph-width)
   (zoom %-cursor
     :property :zoom
     :caliber *gui-zoom*
     :callback 'zoom-callback
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
   (clues check-box
     :property :characters-&-clues
     :items *clues*
     :callback-type :interface
     :selection-callback 'clues-callback
     :retract-callback 'clues-callback
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
   (settings-2 column-layout '(algorithms-tab text-options text)
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

(defmethod initialize-instance :after ((etap etap) &rest keys &key zoom clues)
  "Adjust some creation-time GUI options.
This currently includes the initial ZOOMing factor and CLUES."
  (declare (ignore zoom))
  (setf (slot-value (river-detection-panel etap) 'main-interface) etap)
  ;; #### NOTE: this menu's selection is updated on pop-up.
  (setf (menu-items (slot-value etap 'language-menu))
	(list (make-instance 'menu-component
		:items (mapcar #'car *languages*)
		:interaction :single-selection
		:print-function 'title-capitalize
		:callback 'language-menu-callback
		:popup-callback 'language-menu-popup-callback)))
  (setf (widget-state (zoom etap)) keys)
  (setf (choice-selected-items (clues etap)) clues))

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

(defun update-interface (interface &aux (context (context interface)))
  "Update INTERFACE after a context change."
  (let* ((algorithm (algorithm-type (algorithm context)))
	 (options (algorithm-options (algorithm context)))
	 (tab (algorithms-tab interface))
	 (item (find algorithm (collection-items tab) :key #'first)))
    (setf (choice-selected-item tab) item)
    (map-pane-descendant-children (slot-value interface (second item))
      (lambda (child)
	(when (typep child 'widget)
	  (setf (widget-state child) options)))))
  (setf (widget-state (disposition interface))
	(disposition-type (disposition context)))
  (setf (widget-state (disposition-options-panel interface))
	(disposition-options (disposition context)))
  (setf (widget-state (features interface)) (features context))
  ;; #### TODO: the fake plist below is necessary because we don't have a
  ;; paragraph-width property (we have a context slot). This will be fixed
  ;; when this function understands the same keys as the entry points.
  (setf (widget-state (paragraph-width interface))
	(list :paragraph-width (paragraph-width context)))
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
