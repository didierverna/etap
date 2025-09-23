(in-package :etap)

;; #### WARNING: moving sliders with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that we can safely ignore :MOVE gestures in callbacks, at least
;; for now. I will need to check this again when I introduce focus and
;; keyboard control though.


;; ==========================================================================
;; Utilities
;; ==========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun title-capitalize (title)
    "Capitalize TITLE and substitute dashes with spaces."
    (nsubstitute #\Space #\- (string-capitalize title))))



;; ------------------------
;; Panes hierarchy enabling
;; ------------------------

;; #### NOTE: there is no mechanism to globally enable or disable an interface
;; or a sub-part of it, so we need to do it by hand.

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

(defun redraw (etap)
  "Redraw ETAP interface's paragraph view."
  (gp:invalidate-rectangle (view etap)))

;; #### FIXME: see comment in rivers.lisp
(defun remake-rivers (etap &aux (layout (layout etap)))
  "Remake rivers in ETAP interface."
  (setf (rivers etap)
	(when (and (river-detection-p etap) (not (zerop layout)))
	  (apply #'detect-rivers
	    (get-layout (1- layout) (breakup etap))
	    (widget-state (angle (river-detection-dialog etap)))))))

(defun %remake-from-lineup (etap lineup)
  "Remake ETAP interface's breakup from LINEUP and redraw."
  (let* ((breakup (%make-breakup
		   lineup
		   (second (widget-state (paragraph-width etap)))))
	 (layouts-# (layouts-# breakup)))
    (setf (breakup etap) breakup)
    (setf (layout etap) (if (zerop layouts-#) 0 1))
    (enable-pane (layouts-ctrl etap) (> layouts-# 1))
    (setf (titled-object-title (view etap))
	  (format nil "Layout ~D/~D" (layout etap) layouts-#)))
  (remake-rivers etap)
  (redraw etap))

(defun remake-from-lineup (etap)
  "Remake ETAP interface's breakup from its current lineup and redraw."
  (%remake-from-lineup etap (lineup (breakup etap))))

(defun remake (etap)
  "Remake ETAP interface's breakup and redraw."
  (%remake-from-lineup
   etap
   (%make-lineup
    (make-nlstring
     :text (editor-pane-text (text etap))
     :language (item-data
		(choice-selected-item
		 (first (menu-items (language-menu etap))))))
    (font etap)
    (cdr (widget-state (features etap)))
    (cons (second (widget-state (disposition etap)))
	  (cdr (widget-state (disposition-options-panel etap))))
    (let* ((item (choice-selected-item (algorithms-tab etap)))
	   (algorithm (first item)))
      (cons algorithm
	    (let ((options))
	      (map-pane-descendant-children
	       (slot-value etap (second item))
	       (lambda (child)
		 (typecase child
		   (check-box
		    (setq options (append options (cdr (widget-state child)))))
		   (widget
		    (setq options (append options (widget-state child)))))))
	      options))))))



  
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

(defun find-property-widget (property pane &aux widget)
  "Look into PANE's descendants for a PROPERTY widget."
  (map-pane-descendant-children
   pane
   (lambda (child)
     (when (and (typep child 'widget) (eq property (property child)))
       (setq widget child)))
   :test (lambda (child) (declare (ignore child)) (not widget)))
  widget)



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

;; #### NOTE: we have a redundancy here. Both widgets and calibers have an
;; associated property, so cursors get both (and they'd better be the same).
;; Caliber properties are useful to be remembered in lineup items such as
;; hyphenation points. We could limit widget properties to button boxes, and
;; this is not such a big deal.

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



;; Degree cursors

(defclass dg-cursor (cursor)
  ()
  (:documentation "The Degree Cursor class."))

;; #### TODO: ~3D in the format string below will not like +/-∞ if one day we
;; use percentage cursors with infinity handling calibers. This just doesn't
;; happen for now.
(defmethod cursor-title ((cursor dg-cursor))
  "Return a string of the form \"<Property>: <calibrated value>°\".
The calibrated value is displayed with 3 digits."
  (format nil "~A: ~3D°"
    (title-capitalize (property cursor))
    (calibrated-cursor-value cursor)))




;; ==========================================================================
;; River Detection Dialog
;; ==========================================================================

;; --------
;; Calibers
;; --------

(defmacro define-river-detection-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd RIVER-DETECTION caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber river-detection ,name ,min ,default ,max ,@keys))

(defmacro calibrate-river-detection (name)
  "Calibrate NAMEd RIVER-DETECTION variable."
  `(calibrate river-detection ,name :earmuffs nil))

;; #### TODO: in cases like this one, it would make sense for caliber clamping
;; to take circularity into account (i.e. 360 = 0, etc.).
(define-river-detection-caliber angle 0 0 45 :bounded t)



;; ---------
;; Callbacks
;; ---------

(defun river-detection-switch-callback
    (switch dialog &aux (etap (etap dialog)))
  "Function called when the river detection SWITCH is toggled.
- Remake rivers.
- Toggle the angle cursor's enabled status.
- Redraw the paragraph view."
  (remake-rivers etap)
  (setf (simple-pane-enabled (angle dialog)) (button-selected switch))
  (redraw etap))

(defun river-detection-angle-callback
    (cursor value gesture
     &aux (etap (etap (top-level-interface cursor))))
  "Function called when the river detection angle CURSOR is dragged.
- Update CURSOR's title.
- Remake rivers.
- Redraw the paragraph view."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (remake-rivers etap)
    (redraw etap)))

(define-interface river-detection ()
  ((etap :reader etap))
  (:panes
   (switch check-button
     :text "Detect rivers"
     :callback-type '(:element :interface)
     :selection-callback 'river-detection-switch-callback
     :retract-callback 'river-detection-switch-callback
     :reader switch)
   (angle dg-cursor
     :property :angle
     :caliber *river-detection-angle*
     :enabled nil
     :callback 'river-detection-angle-callback
     :reader angle))
  (:layouts
   (main column-layout '(switch angle)))
  (:default-initargs
   :title "River Detection"
   :window-styles '(:always-on-top t :toolbox t)))

(defmethod river-detection-p ((dialog river-detection))
  "Return T if river detection is enabled in river detection DIALOG."
  (button-selected (switch dialog)))




;; ==========================================================================
;; Penalty Adjustment Dialogs
;; ==========================================================================

(defun penalty-adjustment-destroy-callback (dialog)
  "Function called when penalty adjustment DIALOG is destroyed.
- Possibly reenable the Etap interface if DIALOG was the last one."
  (let ((etap (etap dialog)))
    (setf (penalty-adjustment-dialogs etap)
	  (remove dialog (penalty-adjustment-dialogs etap)))
    (unless (penalty-adjustment-dialogs etap)
      (enable-interface etap))))

;; #### WARNING: the global variables defining each algorithm's
;; parameterization are calibrated by the algorithms entry points, because
;; those entry points can be called programmatically. On the other hand, the
;; penalty sliders and reset buttons below affect an already existing lineup
;; and are accessible only from the GUI. Hence, the returned values need to be
;; calibrated (potentially to infinity) here.

(defun penalty-adjustment-value-callback
    (slider value gesture
     &aux (dialog (top-level-interface slider))
	  (hyphenation-point (hyphenation-point dialog))
	  (etap (etap dialog)))
  "Function called when the penalty adjustment value SLIDER is dragged.
- Calibrate VALUE.
- Advertise VALUE in dialog's title pane.
- Adjust the hyphenation point's penalty.
- Rebreak the lineup."
  (when (eq gesture :drag)
    (setq value (calibrated-value (range-slug-start slider)
				  (caliber hyphenation-point)))
    (setf (title-pane-text (title dialog)) (princ-to-string value))
    (setf (penalty hyphenation-point) value)
    (remake-from-lineup etap)))

(defun penalty-adjustment-reset-callback
    (item dialog
     &aux (slider (value dialog))
	  (hyphenation-point (hyphenation-point dialog))
	  (caliber (caliber hyphenation-point))
	  (value (ecase item
		   (:reset-to-original (original-value dialog))
		   (:reset-to-global   (global-value dialog))
		   (:reset-to-default  (caliber-default caliber)))))
  "Function called when a penalty adjustment reset button is clicked.
- Set the slider to the appropriate reset value.
- Call the slider callback (that is, perform as if the value slider had been
  dragged)."
  (setf (range-slug-start slider) value)
  (penalty-adjustment-value-callback slider value :drag)) ;; whooo...

(define-interface penalty-adjustment ()
  ((original-value :reader original-value)
   (global-value :reader global-value)
   (hyphenation-point :initarg :hyphenation-point :reader hyphenation-point)
   (etap :initarg :etap :reader etap))
  (:panes
   (title title-pane
     :reader title)
   (value slider
     :orientation :vertical
     :visible-min-height 220
     :tick-frequency 0
     :callback 'penalty-adjustment-value-callback
     :reader value)
   (reset push-button-panel
     :items '(:reset-to-original :reset-to-global :reset-to-default)
     :print-function 'title-capitalize
     :layout-class 'column-layout
     :selection-callback 'penalty-adjustment-reset-callback))
  (:layouts
   (main column-layout '(title row))
   (row row-layout '(value reset)))
  (:default-initargs
   :title "Penalty Adjustment"
   :destroy-callback 'penalty-adjustment-destroy-callback
   :window-styles '(:toolbox t
		    :never-iconic t :always-on-top t
		    :can-full-screen nil)))

(defmethod initialize-instance :after
    ((dialog penalty-adjustment)
     &key &aux (etap (etap dialog))
	       (slider (value dialog))
	       (hyphenation-point (hyphenation-point dialog))
	       (caliber (caliber hyphenation-point)))
  "Finish initializing penalty adjustment DIALOG.
- Memoize the original penalty.
- Set the slider's range start, end, and slug start based on the hyphenation
  point's caliber.
- Set DIALOG's title pane."
  (setf (slot-value dialog 'global-value)
	(range-slug-start
	 (find-property-widget
	  (caliber-property caliber)
	  (slot-value
	   etap
	   (second (choice-selected-item (algorithms-tab etap)))))))
  (setf (slot-value dialog 'original-value)
	(decalibrated-value (penalty hyphenation-point)
			    (caliber hyphenation-point)))
  (setf (range-start slider)      (caliber-min caliber)
	(range-end slider)        (caliber-max caliber)
	(range-slug-start slider) (original-value dialog))
  (setf (title-pane-text (title dialog))
	(princ-to-string (penalty hyphenation-point))))

(defun make-penalty-adjustment-dialog (hyphenation-point etap)
  "Make a penalty adjustment dialog from ETAP interface for HYPHENATION-POINT.
If one already exists, activate it and give it the focus. Otherwise, create a
new dialog and display it."
  (let ((dialog (find hyphenation-point (penalty-adjustment-dialogs etap)
		  :key #'hyphenation-point)))
    (if dialog
      (activate-pane dialog)
      (multiple-value-bind (x y) (top-level-interface-geometry etap)
	(setq dialog (make-instance 'penalty-adjustment
		       :hyphenation-point hyphenation-point
		       :etap etap))
	(set-top-level-interface-geometry dialog :x (+ x 200) :y (+ y 200))
	(push dialog (penalty-adjustment-dialogs etap))
	(display dialog :owner etap))))
  (when (enabled etap) (enable-interface etap nil)))




;; ==========================================================================
;; Etap Interface
;; ==========================================================================

(defparameter *clues*
  '(:characters :hyphenation-points
    :over/underfull-boxes :overshrunk/stretched-boxes
    :rivers
    :paragraph-box :line-boxes :character-boxes :baselines
    :properties-tooltips)
  "The visual clues available for conditional display.")

(defparameter *zoom* (make-caliber :zoom 1 100 500 :bounded :min))



;; ---------
;; Callbacks
;; ---------

;; Help

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *etap-tooltips*
    '(:layout--1 "Display previous layout."
      :layout-+1 "Display next layout.")))

(defun help-callback (interface pane type key)
  "Function called when a user gesture requests help.
- Currently handle tooltips."
  (declare (ignore pane))
  (case type (:tooltip (getf (tooltips interface) key))))



;; Destroy

(defun destroy-callback (etap)
  "Function called when ETAP interface is destroyed.
- Destroy the river detection dialog.
- Destroy all remaining penalty adjustment dialogs."
  (destroy (river-detection-dialog etap))
  (mapc #'destroy (penalty-adjustment-dialogs etap)))



;; ETAP menu

(defun menu-callback (item etap)
  "Function called when the ETAP menu is popped up."
  (ecase item
    (:reset-paragraph
     (remake etap))
    (:river-detection
     (display (river-detection-dialog etap) :owner etap))))



;; Disposition

(defun disposition-callback (etap)
  "Function called when the disposition or a disposition option is changed.
- Set ETAP interface's context to the current disposition.
- Remake ETAP interface's breakup."
  (setf (disposition (context etap))
	(cons (second (widget-state (disposition etap)))
	      (cdr (widget-state (disposition-options-panel etap)))))
  (remake etap))



;; Features

(defun features-callback (etap)
  "Function called when the features set changed.
- Set ETAP interface's context to the current feature set.
- Remake ETAP interface's breakup."
  (setf (features (context etap)) (cdr (widget-state (features etap))))
  (remake etap))



;; Clues

(defun clues-callback (etap)
  "Function called when the clues are changed.
- Redraw ETAP's paragraph view."
  (redraw etap))



;; Paragraph width

(defun paragraph-width-callback
    (cursor value gesture &aux (etap (top-level-interface cursor)))
  "Function called when paragraph width CURSOR is dragged.
- Update CURSOR's title.
- Rebreak the current lineup."
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (setf (paragraph-width (context etap)) value)
    (remake-from-lineup etap)))



;; Zoom

(defun zoom-callback (cursor value gesture)
  "Function called when zoom CURSOR is dragged.
- Update CURSOR's title.
- Redraw the paragraph view."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (redraw (top-level-interface cursor))))



;; Layouts

(defun layout-callback
    (+/-1 etap
     &aux (layouts-# (layouts-# (breakup etap)))
	  (layout (layout etap)))
  "Function called when another layout is selected.
- Select the next +/-1 layout and advertise its number.
- Possibly remake rivers.
- Redraw ETAP's paragraph view."
  (setq layout (1+ (mod (1- (funcall +/-1 layout)) layouts-#)))
  (setf (layout etap) layout)
  (setf (titled-object-title (view etap))
	(format nil "Layout ~D/~D" layout layouts-#))
  (when (river-detection-p etap) (remake-rivers etap))
  (redraw etap))



;; Algorithms

(defun set-algorithm
    (etap
     &aux (item (choice-selected-item (algorithms-tab etap)))
	  (algorithm (first item)))
  "Set ETAP interface's context to the current algorithm."
  (setf (algorithm (context etap))
	(cons algorithm
	      (let ((options))
		(map-pane-descendant-children
		 (slot-value etap (second item))
		 (lambda (child)
		   (typecase child
		     (check-box
		      (setq options
			    (append options (cdr (widget-state child)))))
		     (widget
		      (setq options (append options (widget-state child)))))))
		options))))

(defun algorithm-callback (etap)
  "Function called when an algorithm option is clicked.
- Update ETAP interface's context.
- Remake ETAP interface's breakup."
  (set-algorithm etap)
  (remake etap))

(defun algorithm-cursor-callback (cursor value gesture)
  "Function called when an algorithm cursor is dragged.
- Update CURSOR's title.
- Update Etap interface's context.
- Remake Etap interface's breakup."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (let ((etap (top-level-interface cursor)))
      (set-algorithm etap)
      (remake etap))))

(defun algorithms-tab-callback (tab etap)
  "Function called when an algorithm tab is selected.
If ETAP interface is enabled, set algorithm to the selected one in TAB.
Otherwise, reselect the previously selected one."
  (cond ((enabled etap)
	 (set-algorithm etap)
	 (remake etap))
	(t
	 (setf (choice-selected-item tab)
	       (find (algorithm-type (algorithm (context etap)))
		   (collection-items tab)
		 :key #'first)))))



;; Source text menu

(defun text-menu-callback (etap &aux (context (context etap)))
  "Function called when the source text menu is popped up.
- The only button currently resets the text and language, and remakes the Etap
  interface's breakup."
  (setf (nlstring context) (make-nlstring :text *text* :language *language*))
  (setf (editor-pane-text (text etap)) (text context))
  (setf (choice-selected-item (first (menu-items (language-menu etap))))
	*language*)
  (remake etap))



;; Language menu

(defun language-menu-callback (language etap)
  "Function called when a language is selected.
- Change the current text's language.
- Remake ETAP interface's breakup."
  (setf (language (nlstring (context etap))) language)
  (remake etap))



;; Text editor

(defun text-change-callback
    (text-editor point old-length new-length
     &aux (etap (top-level-interface text-editor)))
  "Function called when the source text is changed.
- Set TEXT-EDITOR's current text in the interface's context.
- Remake Etap interface's breakup."
  (declare (ignore point old-length new-length))
  (setf (text (nlstring (context etap))) (editor-pane-text text-editor))
  (remake etap))



;; --------------------------
;; Paragraph View Interaction
;; --------------------------

;; CLIM-like object under mouse utilities

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



;; Motion

(defun motion-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (zoom (/ (range-slug-start (zoom etap)) 100))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Function called when the mouse is moved in the paragraph VIEW.
- Display the properties of the object under mouse (the paragraph itself, a
  line, or a hyphenation point)."
  (when (member :properties-tooltips (choice-selected-items (clues etap)))
    (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
    ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
    ;; returning 0, but this is borderline.
    (decf y (height layout))
    (if (or (and (<= x 0) (<= y (depth layout)))
	    (and (<= y (- (height layout))) (<= x par-width)))
      (display-tooltip view :text (properties breakup :layout-# layout-#))
      (when layout
	(let (object)
	  (if (setq object
		    (or (and (member
			      :hyphenation-points
			      (choice-selected-items (clues etap)))
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
	    (display-tooltip view :text (properties object))
	    (display-tooltip view)))))))



;; Post Menu

;; #### TODO: when this gets enriched, we will eventually end up with the same
;; logic as in MOTION-CALLBACK in order to figure out what's under the mouse,
;; and we already wish we used CLIM...
(defun post-menu-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (zoom (/ (range-slug-start (zoom etap)) 100))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Function called when the user right clicks in the paragraph VIEW.
- Currently display a penalty adjustment dialog when appropriate."
  (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
  ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
  ;; returning 0, but this is borderline.
  (decf y (height layout))
  (when layout
    (let ((object (and (member :hyphenation-points
			       (choice-selected-items (clues etap)))
		       ;; #### NOTE: the +3 and (+ ... 5) are for hyphenation
		       ;; clues occurring at the end of the lines, or in the
		       ;; last line.
		       (>= x 0)
		       (<= x (+ par-width 3))
		       (>= y 0) ; no need to look above the 1st line
		       (<= y (+ (y (car (last (lines layout)))) 5))
		       (hyphenation-point-under x y (lines layout)))))
      (when object
	(make-penalty-adjustment-dialog object etap)))))



;; ------------------------
;; Paragraph View Rendering
;; ------------------------

(defun penalty-hue
    (break-point
     &aux (caliber (caliber break-point))
	  (penalty (decalibrated-value (penalty break-point) caliber)))
  "Return BREAK-POINT's penalty HUE in HSV model.
Colors are interpolated from  blue (min) through green (0), to red (max).
Min and max values depend on BREAK-POINT's caliber."
  (- 4s0 (* 4s0 (/ (- penalty (float (caliber-min caliber)))
		   (- (caliber-max caliber) (caliber-min caliber))))))

(defun display-callback
    (view x y width height
     &aux (etap (top-level-interface view))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (layout etap))
	  (layout (unless (zerop layout-#) (get-layout (1- layout-#) breakup)))
	  (par-y (height layout))
	  (par-h+d (+ par-y (depth layout)))
	  (zoom (/ (range-slug-start (zoom etap)) 100))
	  (clues (choice-selected-items (clues etap))))
  "Function called when paragraph VIEW needs to be redrawn."
  (declare (ignore x y width height))
  (set-horizontal-scroll-parameters view :max-range (+ (* par-width zoom) 40))
  (set-vertical-scroll-parameters view :max-range (+ (* par-h+d zoom) 40))
  (gp:with-graphics-translation (view 20 20)
    (gp:with-graphics-scale (view zoom zoom)
      (when (member :paragraph-box clues)
	(gp:draw-rectangle view 0 0 par-width par-h+d
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
		:do (gp:draw-rectangle view
			x
			(- y (height line))
			(width line)
			(+ (height line) (depth line))
		      :foreground :blue
		      :scale-thickness nil)
	      :when (member :over/underfull-boxes clues)
		:if (> (width line) par-width)
		  :do (gp:draw-rectangle view
			  full-x  (- y (height line))
			  5  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr rest) ;; not the last one
			       (eq (disposition-type (disposition breakup))
				   :justified)
			       (< (width line) par-width))
		  :do (gp:draw-rectangle view
			  full-x (- y (height line))
			  5 (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if ($< (esar line) (asar line))
		  :do (gp:draw-polygon view
			  (list (+ par-width 5)
				(- y (height line))
				(+ par-width 11)
				(- y (height line))
				(+ par-width 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled t :closed t)
		:else :if ($< (asar line) -1)
		  :do (gp:draw-polygon view
			  (list (+ par-width 5)
				(- y (height line))
				(+ par-width 11)
				(- y (height line))
				(+ par-width 8)
				(+ y (depth line)))
			  :foreground :blue
			  :scale-thickness nil :filled nil :closed t)
		:else :if ($> (esar line) (asar line))
		  :do (gp:draw-polygon view
			  (list (+ par-width 5)
				(+ y (depth line))
				(+ par-width 11)
				(+ y (depth line))
				(+ par-width 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($> (asar line) 1)
		  :do (gp:draw-polygon view
			  (list (+ par-width 5)
				(+ y (depth line))
				(+ par-width 11)
				(+ y (depth line))
				(+ par-width 8)
				(- y (height line)))
			:foreground :blue
			:scale-thickness nil :filled nil :closed t)
	      :when (member :baselines clues)
		:do (gp:draw-line view x y (+ x (width line)) y
		      :foreground :purple
		      :scale-thickness nil)
	      :when (or (member :characters clues)
			(member :character-boxes clues))
		:do (mapc (lambda (item)
			    (cond ((typep (object item)
					  'tfm:character-metrics)
				   (when (member :character-boxes clues)
				     (gp:draw-rectangle view
					 (+ x (x item))
					 (- y (height item))
					 (width item)
					 (+ (height item)
					    (depth item))
				       :scale-thickness nil))
				   (when (member :characters clues)
				     (gp:draw-character view
					 (aref *lm-ec*
					       (tfm:code (object item)))
					 (+ x (x item))
					 y)))
				  ((and (discretionary-clue-p (object item))
					(hyphenation-point-p
					 (discretionary (object item)))
					(member :hyphenation-points clues))
				   (gp:draw-polygon view
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
				       (discretionary (object item)))
				      1s0 .7s0)))))
		      (items line)))
	(when (and (member :rivers clues) (rivers etap))
	  (maphash (lambda (source arms)
		     (mapc (lambda (arm &aux (mouth (mouth arm)))
			     (gp:draw-line view
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
		   (rivers etap)))))))




;; ==========================================================================
;; Etap Interface
;; ==========================================================================

(define-interface etap ()
  ((context :initform *context* :initarg :context :reader context)
   (font :initarg :font :reader font)
   (breakup :accessor breakup)
   (layout :initform 0 :accessor layout)
   (enabled :initform t :accessor enabled)
   (rivers
    :documentation "The paragraph's detected rivers."
    :initform nil
    :accessor rivers)
   (penalty-adjustment-dialogs
    :initform nil
    :accessor penalty-adjustment-dialogs)
   (river-detection-dialog
    :initform (make-instance 'river-detection)
    :reader river-detection-dialog)
   (tooltips
    :documentation "This interface's tooltips."
    :allocation :class
    :reader tooltips
    :initform `(,@*etap-tooltips*
		,@*disposition-options-tooltips*
		,@*fixed-tooltips*
		,@*fit-tooltips*
		,@*duncan-tooltips*
		,@*kp-tooltips*
		,@*kpx-tooltips*)))
  (:panes
   (language-menu-component menu-component
     :items (mapcar #'car *languages*)
     :interaction :single-selection
     :print-function 'title-capitalize
     :callback 'language-menu-callback)
   (disposition radio-box
     :property :disposition
     :items *dispositions*
     :callback-type :interface
     :selection-callback 'disposition-callback
     :reader disposition)
   (disposition-options check-box
     :property :disposition-options
     :items *disposition-options*
     :help-keys *disposition-options-help-keys*
     :callback-type :interface
     :selection-callback 'disposition-callback
     :retract-callback 'disposition-callback
     :reader disposition-options-panel)
   (features check-box
     :property :features
     :items *lineup-features*
     :callback-type :interface
     :selection-callback 'features-callback
     :retract-callback 'features-callback
     :reader features)
   (clues check-box
     :property :characters-&-clues
     :items *clues*
     :callback-type :interface
     :selection-callback 'clues-callback
     :retract-callback 'clues-callback
     :reader clues)
   (paragraph-width pt-cursor
     :property :paragraph-width
     :caliber *paragraph-width*
     :callback 'paragraph-width-callback
     :reader paragraph-width)
   (zoom %-cursor
     :property :zoom
     :caliber *zoom*
     :callback 'zoom-callback
     :reader zoom)
   (layout--1 push-button
     :text "<"
     :data #'1-
     :callback 'layout-callback
     :help-key :layout--1)
   (layout-+1 push-button
     :text ">"
     :data #'1+
     :callback 'layout-callback
     :help-key :layout-+1)
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
     :selection-callback 'algorithm-callback)
   (fixed-options check-box
     :property :options
     :items *fixed-options*
     :help-keys *fixed-options-help-keys*
     :callback-type :interface
     :selection-callback 'algorithm-callback)
   (fixed-width-offset pt-cursor
     :property :width-offset
     :caliber *fixed-width-offset*
     :callback 'algorithm-cursor-callback)
   (fit-variant radio-box
     :property :variant
     :items *fit-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-variants-help-keys*)
   (fit-fallback radio-box
     :property :fallback
     :items *fit-fallbacks*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-fallbacks-help-keys*)
   (fit-discriminating-function radio-box
     :property :discriminating-function
     :items *fit-discriminating-functions*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-discriminating-functions-help-keys*)
   (fit-options check-box
     :property :options
     :items *fit-options*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *fit-options-help-keys*)
   (fit-line-penalty cursor
     :property :line-penalty
     :caliber *fit-line-penalty*
     :callback 'algorithm-cursor-callback)
   (fit-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *fit-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (fit-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *fit-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (fit-width-offset pt-cursor
     :property :width-offset
     :caliber *fit-width-offset*
     :callback 'algorithm-cursor-callback)
   (duncan-discriminating-function radio-box
     :property :discriminating-function
     :items *duncan-discriminating-functions*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *duncan-discriminating-functions-help-keys*)
   (kp-variant radio-box
     :property :variant
     :items *kp-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kp-variants-help-keys*)
   (kp-line-penalty cursor
     :property :line-penalty
     :caliber *kp-line-penalty*
     :callback 'algorithm-cursor-callback)
   (kp-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *kp-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (kp-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *kp-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (kp-adjacent-demerits cursor
     :property :adjacent-demerits
     :caliber *kp-adjacent-demerits*
     :callback 'algorithm-cursor-callback)
   (kp-double-hyphen-demerits cursor
     :property :double-hyphen-demerits
     :caliber *kp-double-hyphen-demerits*
     :callback 'algorithm-cursor-callback)
   (kp-final-hyphen-demerits cursor
     :property :final-hyphen-demerits
     :caliber *kp-final-hyphen-demerits*
     :callback 'algorithm-cursor-callback)
   (kp-pre-tolerance cursor
     :property :pre-tolerance
     :caliber *kp-pre-tolerance*
     :callback 'algorithm-cursor-callback)
   (kp-tolerance cursor
     :property :tolerance
     :caliber *kp-tolerance*
     :callback 'algorithm-cursor-callback)
   (kp-emergency-stretch pt-cursor
     :property :emergency-stretch
     :caliber *kp-emergency-stretch*
     :callback 'algorithm-cursor-callback)
   (kp-looseness cursor
     :property :looseness
     :caliber *kp-looseness*
     :callback 'algorithm-cursor-callback)
   (kpx-variant radio-box
     :property :variant
     :items *kpx-variants*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kpx-variants-help-keys*)
   (kpx-fitness radio-box
     :property :fitness
     :items *kpx-fitnesses*
     :callback-type :interface
     :selection-callback 'algorithm-callback
     :help-keys *kpx-fitnesses-help-keys*)
   (kpx-line-penalty cursor
     :property :line-penalty
     :caliber *kpx-line-penalty*
     :callback 'algorithm-cursor-callback)
   (kpx-hyphen-penalty cursor
     :property :hyphen-penalty
     :caliber *kpx-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (kpx-explicit-hyphen-penalty cursor
     :property :explicit-hyphen-penalty
     :caliber *kpx-explicit-hyphen-penalty*
     :callback 'algorithm-cursor-callback)
   (kpx-adjacent-demerits cursor
     :property :adjacent-demerits
     :caliber *kpx-adjacent-demerits*
     :callback 'algorithm-cursor-callback)
   (kpx-double-hyphen-demerits cursor
     :property :double-hyphen-demerits
     :caliber *kpx-double-hyphen-demerits*
     :callback 'algorithm-cursor-callback)
   (kpx-final-hyphen-demerits cursor
     :property :final-hyphen-demerits
     :caliber *kpx-final-hyphen-demerits*
     :callback 'algorithm-cursor-callback)
   (kpx-similar-demerits cursor
     :property :similar-demerits
     :caliber *kpx-similar-demerits*
     :callback 'algorithm-cursor-callback)
   (kpx-pre-tolerance cursor
     :property :pre-tolerance
     :caliber *kpx-pre-tolerance*
     :callback 'algorithm-cursor-callback)
   (kpx-tolerance cursor
     :property :tolerance
     :caliber *kpx-tolerance*
     :callback 'algorithm-cursor-callback)
   (kpx-emergency-stretch pt-cursor
     :property :emergency-stretch
     :caliber *kpx-emergency-stretch*
     :callback 'algorithm-cursor-callback)
   (kpx-looseness cursor
     :property :looseness
     :caliber *kpx-looseness*
     :callback 'algorithm-cursor-callback)
   (text-button popup-menu-button
     :text "Source text" :menu text-menu)
   (language-button popup-menu-button
     :text "Language" :menu language-menu)
   (text editor-pane
     :visible-min-width '(character 80)
     :visible-min-height '(character 10)
     :visible-max-height '(character 30)
     :change-callback 'text-change-callback
     :reader text)
   (view output-pane
     :title "Layout" :title-position :frame
     :font (gp:make-font-description :family "Latin Modern Roman"
	     :weight :normal :slant :roman :size 10)
     :visible-min-height 300
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'display-callback
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
  (:menus
   (etap-menu "ETAP" (:reset-paragraph :river-detection)
     :print-function 'title-capitalize
     :callback 'menu-callback)
   (text-menu nil #| no title |# (:reset)
    :print-function 'title-capitalize
    :callback-type :interface
    :callback 'text-menu-callback)
   (language-menu nil #| no title |# (language-menu-component)
     :reader language-menu))
  (:menu-bar etap-menu)
  (:default-initargs
   :title "Experimental Typesetting Algorithms Platform"
   :help-callback 'help-callback
   :destroy-callback 'destroy-callback))

(defmethod initialize-instance :after ((etap etap) &rest keys &key zoom clues)
  "Adjust some creation-time GUI options.
This currently includes the initial ZOOMing factor and CLUES."
  (declare (ignore zoom))
  (setf (slot-value (river-detection-dialog etap) 'etap) etap)
  (setf (widget-state (zoom etap)) keys)
  (setf (choice-selected-items (clues etap)) clues))

(defmethod enable-interface ((etap etap) &optional (enabled t))
  "Change ETAP interface's enabled status.
The zooming and clues controls are always enabled.
The only interface controls which are subject to enabling / disabling are
those which may affect the typesetting."
  (setf (simple-pane-enabled (paragraph-width etap)) enabled)
  (enable-pane (layouts-ctrl etap) enabled)
  (enable-pane (options-1 etap) enabled)
  (enable-pane (settings-2 etap) enabled)
  (setf (enabled etap) enabled))

(defmethod river-detection-p ((etap etap))
  "Return T if river detection is enabled in ETAP interface."
  (river-detection-p (river-detection-dialog etap)))


;; Interface display

(defun update-interface (etap &aux (context (context etap)))
  "Update ETAP interface after a context change."
  (let* ((algorithm (algorithm-type (algorithm context)))
	 (options (algorithm-options (algorithm context)))
	 (tab (algorithms-tab etap))
	 (item (find algorithm (collection-items tab) :key #'first)))
    (setf (choice-selected-item tab) item)
    (map-pane-descendant-children (slot-value etap (second item))
      (lambda (child)
	(when (typep child 'widget)
	  (setf (widget-state child) options)))))
  (setf (widget-state (disposition etap))
	(disposition-type (disposition context)))
  (setf (widget-state (disposition-options-panel etap))
	(disposition-options (disposition context)))
  (setf (widget-state (features etap)) (features context))
  ;; #### TODO: the fake plist below is necessary because we don't have a
  ;; paragraph-width property (we have a context slot). This will be fixed
  ;; when this function understands the same keys as the entry points.
  (setf (widget-state (paragraph-width etap))
	(list :paragraph-width (paragraph-width context)))
  (setf (choice-selected-item (first (menu-items (language-menu etap))))
	(language (nlstring context)))
  (setf (editor-pane-text (text etap)) (text context))
  (values))

;; #### NOTE: I'm not sure, but I suppose that twiddling with the geometry is
;; better done here than in an INITIALIZE-INSTANCE :after method.
(defmethod interface-display :before ((etap etap))
  "Finalize ETAP interface's display settings.
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
  (setq zoom (calibrated-value zoom *zoom*))
  (unless (listp clues) (setq clues (list clues)))
  (display (make-instance 'etap
	     :context context
	     :font (font context)
	     :zoom zoom
	     :clues clues)))
