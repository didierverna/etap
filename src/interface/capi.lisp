(in-package :etap)

;; #### WARNING: moving sliders with the mouse (dragging or clicking
;; elsewhere) seems to generate :DRAG gestures followed by two :MOVE ones. So
;; it seems that we can safely ignore :MOVE gestures in callbacks, at least
;; for now. I will need to check this again when I introduce focus and
;; keyboard control though.

(defvar *interface* nil "The default GUI.")



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

(defgeneric widget-value (widget)
  (:documentation "Return WIDGET's value."))

(defgeneric (setf widget-value) (value widget)
  (:documentation "Set WIDGET's VALUE."))

(defun widget-specification (widget)
  "Return the list (<widget property> <widget value>)."
  (list (property widget) (widget-value widget)))

(defun find-widget (property pane &aux widget)
  "Find a widget for PROPERTY in PANE's descendants."
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

(defmethod widget-value ((box radio-box))
  "Return radio BOX's selected item data."
  (let ((item (choice-selected-item box)))
    (if (typep item 'capi:item) (item-data item) item)))

(defmethod (setf widget-value) (data (box radio-box))
  "Set radio BOX's selection to item DATA.
DATA must be one of BOX's items data, or NIL, in which case the first item
data will be selected."
  (setf (choice-selected-item box)
	(or (find data (collection-items box)
	      :key (lambda (item)
		     (if (typep item 'capi:item) (item-data item) item)))
	    (svref (collection-items box) 0))))



;; Check Boxes

(defclass check-box (button-box check-button-panel)
  ()
  (:documentation "The Check Box class."))

(defmethod widget-value ((box check-box))
  "Return check BOX's value.
This is an exhaustive property list of BOX's items data and their selected
state."
  (loop :with selection := (choice-selected-items box)
	:for item :across (collection-items box) ; a vector
	:collect (if (typep item 'capi:item) (item-data item) item)
	:if (member item selection)
	  :collect t
	:else
	  :collect nil))

;; #### NOTE: see comment in SET-STATE about the apparent laxism of this
;; method (unknown options are ignored instead of signalling an error).
(defmethod (setf widget-value) (plist (box check-box))
  "Set check BOX's selected items based on PLIST.
More specifically, every BOX item found to be true in PLIST is selected. The
rest is deselected (i.e., no items are left in their previous state). Unknown
items are ignored."
  (setf (choice-selected-items box)
	(loop :for item :across (collection-items box) ; a vector
	      :when (getf plist
			  (if (typep item 'capi:item) (item-data item) item))
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


(defun calibrated-cursor-value (cursor)
  "Return the calibrated current CURSOR value."
  (calibrated-value (range-slug-start cursor) (caliber cursor)))

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
  (setf (range-slug-start cursor)
	(decalibrated-value (caliber-default caliber) caliber))
  (update-cursor-title cursor))


(defmethod widget-value ((cursor cursor))
  "Return CURSOR's calibrated value."
  (calibrated-cursor-value cursor))

(defmethod (setf widget-value)
    (value (cursor cursor) &aux (caliber (caliber cursor)))
  "Set CURSOR's (decalibrated) VALUE.
If VALUE is NIL, use the default value of CURSOR's caliber instead."
  (setf (range-slug-start cursor)
	(decalibrated-value (or value (caliber-default caliber)) caliber))
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
;; Updaters
;; ==========================================================================

(defun redraw (etap)
  "Redraw ETAP interface's paragraph view."
  (gp:invalidate-rectangle (view-area etap)))



;; Rivers updater

;; #### FIXME: see comment in rivers.lisp
(defun remake-rivers (etap &aux (layout (layout etap)))
  "Remake ETAP interface's rivers and redraw."
  (setf (rivers etap)
	(when (and (river-detection-p etap) (not (zerop layout)))
	  (apply #'detect-rivers
	    (get-layout (1- layout) (breakup etap))
	    (widget-specification
	     (angle-cursor (river-detection-dialog etap))))))
  (redraw etap))



;; Layout-based updater

(defun remake-with-layout (etap layout)
  "Remake ETAP interface with LAYOUT number, and redraw."
  (setf (layout etap) layout)
  (setf (titled-object-title (view-area etap))
	(format nil "Layout ~D/~D" (layout etap) (layouts-# (breakup etap))))
  (remake-rivers etap))



;; Breakup-based updater

(defun remake-with-breakup
    (etap breakup &optional layout &aux (layouts-# (layouts-# breakup)))
  "Remake ETAP interface with BREAKUP, and redraw.
Display LAYOUT number (1 by default)."
  (setf (breakup etap) breakup)
  (enable-pane (layouts-ctrl-layout etap) (> layouts-# 1))
  (unless layout (setq layout (if (zerop layouts-#) 0 1)))
  (remake-with-layout etap layout))



;; Lineup-based updater

(defun remake-with-lineup (etap lineup)
  "Remake ETAP interface's breakup with LINEUP, and redraw."
  (remake-with-breakup
   etap
   (%make-breakup
    lineup
    (widget-value (paragraph-width-cursor etap)))))

(defun remake-with-current-lineup (etap)
  "Remake ETAP interface's breakup from its current lineup and redraw."
  (remake-with-lineup etap (lineup (breakup etap))))



;; Global updater

(defun disposition-specification (etap)
  "Return ETAP interface's current disposition specification."
  (cons (widget-value (disposition-type-box etap))
	(widget-value (disposition-options-box etap))))

(defun language-specification (etap)
  "Return ETAP interface's current language specification."
  (item-data (choice-selected-item (first (menu-items (language-menu etap))))))

(defun algorithm-specification
    (etap &aux (item (choice-selected-item (algorithm-tabs etap))))
  "Return ETAP interface's current algorithm specification."
  (cons (first item)
	(let ((options))
	  (map-pane-descendant-children
	   (slot-value etap (second item))
	   (lambda (child)
	     (typecase child
	       (check-box
		;; #### WARNING: this special case is because check boxes in
		;; algorithms currently only represent additional options for
		;; which the widget's property is meaningless (typically
		;; :options). We do not have anything working like the clues
		;; check box in algorithms right now, but if that changes, we
		;; will have a problem here.
		(setq options (append options (widget-value child))))
	       (widget
		(setq options (append options (widget-specification child)))))))
	  options)))

(defun remake (etap)
  "Remake ETAP interface's breakup, and redraw."
  (remake-with-lineup
   etap
   (%make-lineup
    (make-nlstring
     :text (editor-pane-text (text etap))
     :language (language-specification etap))
    (capi-object-property etap :font)
    (widget-value (features-box etap))
    (disposition-specification etap)
    (algorithm-specification etap))))




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
- Toggle the angle cursor's enabled status.
- Remake rivers and redraw."
  (setf (simple-pane-enabled (angle-cursor dialog)) (button-selected switch))
  (remake-rivers etap))

(defun river-detection-angle-callback
    (cursor value gesture
     &aux (etap (etap (top-level-interface cursor))))
  "Function called when the river detection angle CURSOR is dragged.
- Update CURSOR's title.
- Remake rivers and redraw."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (remake-rivers etap)))

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
     :reader angle-cursor))
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

;; #### NOTE: the logic used with penalty adjustment dialogs is meant to
;; simplify dependency management between the UI and the lineup. Every UI
;; component that entails computing a new lineup is deactivated for as long as
;; there's a live penalty adjustment dialog.

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
	  (break-point (break-point dialog))
	  (etap (etap dialog)))
  "Function called when the penalty adjustment value SLIDER is dragged.
- Calibrate VALUE.
- Advertise VALUE in dialog's title pane.
- Adjust the break point's penalty.
- Remake from the current lineup."
  (when (eq gesture :drag)
    (setq value (calibrated-value (range-slug-start slider)
				  (caliber break-point)))
    (setf (title-pane-text (title-area dialog)) (princ-to-string value))
    (setf (penalty break-point) value)
    (remake-with-current-lineup etap)))

(defun penalty-adjustment-reset-callback
    (item dialog
     &aux (slider (value-slider dialog))
	  (break-point (break-point dialog))
	  (caliber (caliber break-point))
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
   (break-point :initarg :break-point :reader break-point)
   (etap :initarg :etap :reader etap))
  (:panes
   (title title-pane
     :reader title-area #| Can't use TITLE-PANE here (CAPI symbol). |#)
   (value slider
     :orientation :vertical
     :visible-min-height 220
     :tick-frequency 0
     :callback 'penalty-adjustment-value-callback
     :reader value-slider)
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
	       (slider (value-slider dialog))
	       (break-point (break-point dialog))
	       (caliber (caliber break-point)))
  "Finish initializing penalty adjustment DIALOG.
- Memoize the original penalty.
- Set the slider's range start, end, and slug start based on the break point's
  caliber.
- Set DIALOG's title pane."
  ;; #### FIXME: when the caliber is not associated with a customizable
  ;; variable (e.g. glue penalties), it is redundant to have both a "reset to
  ;; global" and a "reset to default" button.
  (setf (slot-value dialog 'global-value)
	(let ((widget
		(find-widget
		 (caliber-property caliber)
		 (slot-value
		  etap
		  (second (choice-selected-item (algorithm-tabs etap)))))))
	  (if widget
	    (range-slug-start widget)
	    (caliber-default caliber))))
  (setf (slot-value dialog 'original-value)
	(decalibrated-value (penalty break-point)
			    (caliber break-point)))
  (setf (range-start slider)      (caliber-min caliber)
	(range-end slider)        (caliber-max caliber)
	(range-slug-start slider) (original-value dialog))
  (setf (title-pane-text (title-area dialog))
	(princ-to-string (penalty break-point))))

(defun find-penalty-adjustment-dialog (break-point etap)
  "Find a penalty adjustment dialog for BREAK-POINT in ETAP."
  (find break-point (penalty-adjustment-dialogs etap) :key #'break-point))

(defun make-penalty-adjustment-dialog (break-point etap)
  "Make a penalty adjustment dialog from ETAP interface for BREAK-POINT.
If one already exists, activate it and give it the focus. Otherwise, create a
new dialog and display it."
  (let ((dialog (find-penalty-adjustment-dialog break-point etap)))
    (if dialog
      (activate-pane dialog)
      (multiple-value-bind (x y) (top-level-interface-geometry etap)
	(setq dialog (make-instance 'penalty-adjustment
		       :break-point break-point
		       :etap etap))
	(set-top-level-interface-geometry dialog :x (+ x 200) :y (+ y 200))
	(push dialog (penalty-adjustment-dialogs etap))
	(display dialog :owner etap))))
  (when (enabled etap) (enable-interface etap nil)))




;; ==========================================================================
;; Etap Interface
;; ==========================================================================

(defparameter *clues*
  '(:characters :hyphenation-points :whitespaces
    :over/underfull-boxes :overshrunk/stretched-boxes
    :rivers
    :paragraph-box :line-boxes :character-boxes :baselines)
  "The visual clues available for conditional display.")

(defparameter *inspector-options*
  '(:activate :tooltips)
  "The inspector options.")

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
    (:river-detection
     (display (river-detection-dialog etap) :owner etap))))



;; Inspector

(defun enable-inspector
    (inspector
     &aux (activep (getf (widget-value inspector) :activate)))
  "Enable or disable the INSPECTOR box's buttons.
Enabling or disabling is done based on the current status of the activation
button (itself not subject to enabling / disabling)"
  (map nil (lambda (item)
	     (unless (eq (item-data item) :activate)
	       (setf (simple-pane-enabled item) activep)))
       (collection-items inspector)))

(defun inspector-callback (data etap &aux (inspector (inspector-box etap)))
  "Function called when an inspector button is clicked."
  (case data
    (:activate
     (enable-inspector inspector)
     (unless (getf (widget-value inspector) :activate)
       (mapc #'destroy (penalty-adjustment-dialogs etap))))))



;; Paragraph width

(defun paragraph-width-callback
    (cursor value gesture &aux (etap (top-level-interface cursor)))
  "Function called when paragraph width CURSOR is dragged.
- Update CURSOR's title.
- Remake from the current lineup."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (remake-with-current-lineup etap)))



;; Zoom

(defun zoom-callback (cursor value gesture)
  "Function called when zoom CURSOR is dragged.
- Update CURSOR's title.
- Redraw."
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
- Remake rivers and redraw."
  (setq layout (1+ (mod (1- (funcall +/-1 layout)) layouts-#)))
  (remake-with-layout etap layout))



;; Algorithms

(defun algorithm-cursor-callback (cursor value gesture)
  "Function called when an algorithm cursor is dragged.
- Update CURSOR's title.
- Remake everything."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (remake (top-level-interface cursor))))

(defun algorithm-tabs-callback (tab etap)
  "Function called when an algorithm tab is selected.
If ETAP interface is enabled, remember the selection and remake everything.
Otherwise, reselect the previously selected one."
  (cond ((enabled etap)
	 (setf (capi-object-property tab :current-item)
	       (choice-selected-item tab))
	 (remake etap))
	(t
	 (setf (choice-selected-item tab)
	       (capi-object-property tab :current-item)))))



;; Source text menu

(defun text-menu-callback (item etap &aux nlstring)
  "Function called when the ITEM source text menu button is clicked in ETAP."
  (setq nlstring (case item
		   (:reset-to-original
		    (capi-object-property etap :original-nlstring))
		   (:reset-to-default
		    (make-nlstring :text *text* :language *language*))))
  (setf (editor-pane-text (text etap)) (text nlstring))
  (setf (choice-selected-item (first (menu-items (language-menu etap))))
	(language nlstring))
  (remake etap))



;; Text editor

;; See "callback mess" comment in SET-STATE.
(defun text-change-callback
    (text-editor point old-length new-length
     &aux (etap (top-level-interface text-editor)))
  "Function called when the source text is changed.
- Remake everything."
  (declare (ignore point old-length new-length))
  (remake etap))



;; --------------------------
;; Paragraph View Interaction
;; --------------------------

;; CLIM-like object under mouse utilities

(defun line-under (y lines)
  "Return the line from LINES which is under Y coordinate, or NIL."
  (find-if (lambda (line)
	     (<= (- (y line) (height line)) y (+ (y line) (depth line))))
	   lines))

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

;; #### FIXME: the hyphenation clues geometry (the small triangles under the
;; lines in between characters) is hard coded at different places, which is
;; not very cool.
(defun hyphenation-under (x y lines &aux (p (cons x y)))
  "Return the discretionary clue from LINES which is under (X, Y), or nil.
The discretionary clue is returned only if it corresponds to an hyphenation
point (as opposed to a general discretionary), and the object returned is in
fact the pin containing the hyphenation clue.
Technically, (X, Y) is not over the hyphenation clue (which has a width of 0),
but over its visual representation (the small triangle beneath it).
This function returns the corresponding line as a second value."
  (let ((line (find-if (lambda (line)
			 (and (>= y (y line)) (<= y (+ (y line) 5))))
		       lines)))
    (when line
      (let* ((x (x line))
	     (y (y line)))
	(values (find-if (lambda (item)
			   (and (discretionary-clue-p (object item))
				(hyphenation-point-p
				 (discretionary (object item)))
				(triangle-under-p
				 p
				 (cons (+ x (x item)) y)
				 (cons (+ x (x item) -3) (+ y 5))
				 (cons (+ x (x item) +3) (+ y 5)))))
			 (items line))
		line)))))

(defun whitespace-under (x y lines &aux (line (line-under y lines)))
  "Return the whitespace from LINES which is under (X, Y), or nil.
This function returns the corresponding line as a second value."
  (when line
    (values (find-if (lambda (item)
		       (and (whitespacep item)
			    (<= (+ (x line) (x item))
				x
				(+ (x line) (x item) (width item)))
			    (<= (- (y line) (height item)) y (y line))))
		     (items line))
	    line)))

(defun object-under (x y lines)
  "Return the object from LINES which is under (X, Y), or nil.
This currently includes whitespaces and hyphenation points.
For hyphenation points, (X, Y) is not technically over it, but over the
corresponding hyphenation clue.
This function returns the corresponding line as a second value."
  (multiple-value-bind (object line) (hyphenation-under x y lines)
    (if object ;; OR doesn't propagate secondary values on its first args!
      (values object line)
      (whitespace-under x y lines))))



;; Motion

(defun motion-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (inspect (widget-value (inspector-box etap)))
	  (zoom (/ (range-slug-start (zoom-cursor etap)) 100))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Function called when the mouse is moved in the paragraph VIEW.
- Display the paragraph properties when the mouse is above the first line.
- Display the line properties when the mouse is in the left margin of a line.
- Otherwise, display the properties of the object under mouse (currently, a
  hyphenation point)."
  (setf (capi-object-property view :pointer) (cons x y))
  (when (getf inspect :activate)
    ;; We have the view directly here, so no need to go through REDRAW.
    ;; #### TODO: this could be optimized in order to avoid redrawing at every
    ;; single move. We could remember the previous move state and see if
    ;; something has changed.
    (gp:invalidate-rectangle view)
    (when (getf inspect :tooltips)
      (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
      ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and
      ;; DEPTH returning 0, but this is borderline.
      (decf y (height layout))
      (cond ((and (< y (- (height layout))) (<= x par-width))
	     (display-tooltip view
	       :text (properties breakup :layout-# layout-#)))
	    ((and (< x 0) (<= y (depth layout)))
	     (let ((line (when layout (line-under y (lines layout )))))
	       (if line
		 (display-tooltip view :text (properties line))
		 (display-tooltip view))))
	    ;; #### NOTE: the +3 and +5 are for hyphenation clues occurring at
	    ;; the end of the lines, or in the last line.
	    ((and (<= 0 x (+ par-width 3))
		  (<= (- (height layout)) y (+ (depth layout) 5)))
	     (let ((object (when layout (object-under x y (lines layout)))))
	       ;; #### FIXME: this is really shaky. We know that currently
	       ;; OBJECT-UNDER will only return a whitespace or a pinned
	       ;; hyphenation clue. Simplifying the code below would require
	       ;; defining a PROPERTIES method on the PINNED class
	       ;; (advertising the properties of the pinned object), but we
	       ;; don't want to define such a method for every kind of pinable
	       ;; object (at least not right now).
	       (if object
		 (display-tooltip view
		   :text (if (whitespacep object)
			   (properties object)
			   (properties (object object))))
		 (display-tooltip view))))
	    (t
	     (display-tooltip view))))))



;; Post Menu

;; #### TODO: when this gets enriched, we will eventually end up with the same
;; logic as in MOTION-CALLBACK in order to figure out what's under the mouse,
;; and we already wish we used CLIM...
(defun post-menu-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (zoom (/ (range-slug-start (zoom-cursor etap)) 100))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Function called when the user right clicks in the paragraph VIEW.
This does nothing if the inspector is not active. Otherwise, it currently
displays a penalty adjustment dialog when appropriate."
  (when (getf (widget-value (inspector-box etap)) :activate)
    (setq x (/ (- x 20) zoom) y (/ (- y 20) zoom))
    ;; #### WARNING: if there's no layout, we rely on WIDTH, HEIGHT, and DEPTH
    ;; returning 0, but this is borderline.
    (decf y (height layout))
    (when layout
      (let ((object (and ;; #### NOTE: the +3 and (+ ... 5) are for
			 ;; hyphenation clues occurring at the end of the
			 ;; lines, or in the last line.
			 (>= x 0)
			 (<= x (+ par-width 3))
			 (<= y (+ (y (car (last (lines layout)))) 5))
			 (object-under x y (lines layout)))))
	(when object
	  (setq object
		(etypecase (object object)
		  (discretionary-clue (discretionary (object object)))
		  (glue (object object))))
	  ;; #### FIXME: see comment on top of BREAK-POINT. This entails the
	  ;; complexity of handling null calibers below.
	  (when (caliber object)
	    (make-penalty-adjustment-dialog object etap)))))))



;; ------------------------
;; Paragraph View Rendering
;; ------------------------

(defun penalty-hue (break-point &aux (caliber (caliber break-point)))
  "Return BREAK-POINT's penalty HUE in HSV model.
Colors are interpolated from  blue (min) through green (0), to red (max).
Min and max values depend on BREAK-POINT's penalty and caliber."
  ;; #### FIXME: see comment on top of BREAK-POINT. This entails the
  ;; complexity of handling null calibers below.
  (if caliber
    (- 4s0 (* 4s0 (/ (- (decalibrated-value (penalty break-point) caliber)
			(float (caliber-min caliber)))
		     (- (caliber-max caliber) (caliber-min caliber)))))
    2s0))

(defun draw-hyphenation-clue (view x y discretionary)
  "Draw an hyphenation clue in VIEW at (X,Y) for DISCRETIONARY."
  (gp:draw-polygon
   view
   (list x y (- x 3) (+ y 5) (+ x 3) (+ y 5) x y)
   :filled (not (explicitp discretionary))
   :foreground (color:make-hsv (penalty-hue discretionary) 1s0 .7s0)))

(defun draw-whitespace-clue
    (view x y whitespace &optional force &aux (glue (object whitespace)))
  "Draw a whitespace clue in VIEW relatively to (X,Y) for WHITESPACE.
(X,Y) is the point which WHITESPACE is positioned relatively to.
Unless FORCE, the clue is drawn only if the corresponding glue's penalty is
not 0."
  (when (or force
	    (not (zerop (decalibrated-value (penalty glue) (caliber glue)))))
    (gp:draw-rectangle view
	(+ x (x whitespace))
	(- y (height whitespace))
	(width whitespace)
	(+ (height whitespace) (depth whitespace))
      :filled t ;; (not (explicitp discretionary))
      :foreground (color:make-hsv (penalty-hue glue) 1s0 .7s0))))

(defun display-callback
    (view x y width height
     &aux (etap (top-level-interface view))
	  (breakup (breakup etap))
	  (par-width (paragraph-width breakup))
	  (layout-# (layout etap))
	  (layout (unless (zerop layout-#) (get-layout (1- layout-#) breakup)))
	  (par-y (height layout))
	  (par-h+d (+ par-y (depth layout)))
	  (clues (choice-selected-items (clues-box etap)))
	  (inspect (mapcar #'item-data
		     (choice-selected-items (inspector-box etap))))
	  (zoom (/ (range-slug-start (zoom-cursor etap)) 100)))
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
				   (draw-hyphenation-clue
				    view (+ x (x item)) y
				    (discretionary (object item))))
				  ((and (whitespacep item)
					(member :whitespaces clues))
				   (draw-whitespace-clue
				    view (x line) (+ par-y (y line)) item))))
		      (items line)))
	(when (member :activate inspect)
	  (let* ((pointer (capi-object-property view :pointer))
		 (x (/ (- (car pointer) 20) zoom))
		 (y (/ (- (cdr pointer) 20) zoom)))
	    (decf y (height layout))
	    (multiple-value-bind (object line)
		(object-under x y (lines layout))
	      (cond ((whitespacep object)
		     (draw-whitespace-clue
		      view (x line) (+ par-y (y line)) object 'force))
		    (object
		     (unless (member :hyphenation-points clues)
		       (draw-hyphenation-clue
			view (+ (x line) (x object)) (+ par-y (y line))
			(discretionary (object object)))))))))
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
  ((breakup
    :documentation "This interface's current breakup."
    :accessor breakup)
   (layout
    :documentation "This interface's currently displayed layout number.
The layout number starts at 1 (technically, layout index + 1). 0 indicates
that the breakup does not contain any layout."
    :accessor layout)
   (enabled
    :documentation "Whether this interface is currently enabled."
    :initform t :accessor enabled)
   (rivers
    :documentation "The paragraph's detected rivers."
    :accessor rivers)
   (penalty-adjustment-dialogs
    :documentation "This interface's live penalty adjustment dialogs."
    :initform nil
    :accessor penalty-adjustment-dialogs)
   (river-detection-dialog
    :documentation "This interface's river detection dialog."
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
     :callback-type :interface
     :callback 'remake)
   (disposition radio-box
     :property :disposition
     :items *dispositions*
     :callback-type :interface
     :selection-callback 'remake
     :visible-max-width nil
     :reader disposition-type-box)
   (disposition-options check-box
     :property :disposition-options
     :items *disposition-options*
     :help-keys *disposition-options-help-keys*
     :callback-type :interface
     :selection-callback 'remake
     :retract-callback 'remake
     :visible-max-width nil
     :reader disposition-options-box)
   (features check-box
     :property :features
     :items *lineup-features*
     :callback-type :interface
     :selection-callback 'remake
     :retract-callback 'remake
     :visible-max-width nil
     :reader features-box)
   (clues check-box
     :property :characters-&-clues
     :items *clues*
     :callback-type :interface
     :selection-callback 'redraw
     :retract-callback 'redraw
     :visible-max-width nil
     :reader clues-box)
   ;; The inspector items are created dynamically in the INITIALIZE-INSTANCE
   ;; after method.
   (inspector check-box
     :property :inspector
     :selection-callback 'inspector-callback
     :retract-callback 'inspector-callback
     :visible-max-width nil
     :reader inspector-box)
   (paragraph-width pt-cursor
     :property :paragraph-width
     :caliber *paragraph-width*
     :callback 'paragraph-width-callback
     :reader paragraph-width-cursor)
   (zoom %-cursor
     :property :zoom
     :caliber *zoom*
     :callback 'zoom-callback
     :reader zoom-cursor)
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
   (algorithm-tabs tab-layout
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
     :selection-callback 'algorithm-tabs-callback
     ;; #### WARNING: with my emulation of an enabled/disabled status for the
     ;; main interface, the algorithms tab's selection callback may override
     ;; the selection that triggered its call. However, even though the
     ;; visible child function is called afterwards, the item passed along is
     ;; the old one (probably bound before the execution of the callback). The
     ;; solution around this is to ignore the (obsolete) item, and work
     ;; directly with the tab's selection.
     :visible-child-function (lambda (item)
			       (declare (ignore item))
			       (second (choice-selected-item algorithm-tabs)))
     :reader algorithm-tabs)
   (fixed-fallback radio-box
     :property :fallback
     :items *fixed-fallbacks*
     :help-keys *fixed-fallbacks-help-keys*
     :callback-type :interface
     :selection-callback 'remake)
   (fixed-options check-box
     :property :options
     :items *fixed-options*
     :help-keys *fixed-options-help-keys*
     :callback-type :interface
     :selection-callback 'remake)
   (fixed-width-offset pt-cursor
     :property :width-offset
     :caliber *fixed-width-offset*
     :callback 'algorithm-cursor-callback)
   (fit-variant radio-box
     :property :variant
     :items *fit-variants*
     :callback-type :interface
     :selection-callback 'remake
     :help-keys *fit-variants-help-keys*)
   (fit-fallback radio-box
     :property :fallback
     :items *fit-fallbacks*
     :callback-type :interface
     :selection-callback 'remake
     :help-keys *fit-fallbacks-help-keys*)
   (fit-discriminating-function radio-box
     :property :discriminating-function
     :items *fit-discriminating-functions*
     :callback-type :interface
     :selection-callback 'remake
     :help-keys *fit-discriminating-functions-help-keys*)
   (fit-options check-box
     :property :options
     :items *fit-options*
     :callback-type :interface
     :selection-callback 'remake
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
     :selection-callback 'remake
     :help-keys *duncan-discriminating-functions-help-keys*)
   (kp-variant radio-box
     :property :variant
     :items *kp-variants*
     :callback-type :interface
     :selection-callback 'remake
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
     :selection-callback 'remake
     :help-keys *kpx-variants-help-keys*)
   (kpx-fitness radio-box
     :property :fitness
     :items *kpx-fitnesses*
     :callback-type :interface
     :selection-callback 'remake
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
     ;; See "callback mess" comment in SET-STATE.
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
     :reader view-area
     :input-model '((:motion motion-callback)
		    (:post-menu post-menu-callback))))
  (:layouts
   (main column-layout '(settings view))
   (settings row-layout '(settings-1 settings-2))
   (settings-1 column-layout '(options paragraph-width zoom layouts-ctrl))
   (layouts-ctrl row-layout '(layout--1 layout-+1)
     :reader layouts-ctrl-layout)
   (options row-layout '(options-1 options-2))
   (options-1 column-layout '(clues inspector))
   (options-2 column-layout '(disposition disposition-options features)
     :reader options-2-layout)
   (settings-2 column-layout '(algorithm-tabs text-options text)
     :reader settings-2-layout)
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
   (etap-menu "ETAP" (:river-detection)
     :print-function 'title-capitalize
     :callback 'menu-callback)
   (text-menu nil #| no title |# (:reset-to-original :reset-to-default)
    :print-function 'title-capitalize
    :callback 'text-menu-callback)
   (language-menu nil #| no title |# (language-menu-component)
     :reader language-menu))
  (:menu-bar etap-menu)
  (:default-initargs
   :title "Experimental Typesetting Algorithms Platform"
   :help-callback 'help-callback
   :destroy-callback 'destroy-callback))

(defmethod initialize-instance :after ((etap etap) &rest keys)
  "Adjust creation-time GUI options and dynamically constructed elements."
  ;; Make sure that we have at least numbers here.
  (setf (capi-object-property (view-area etap) :pointer) (cons -1 -1))
  (setf (slot-value (river-detection-dialog etap) 'etap) etap)
  (let ((inspector (inspector-box etap)))
    (setf (collection-items inspector)
	  (mapcar (lambda (property)
		    (make-instance 'check-button
		      :data property
		      :print-function #'title-capitalize
		      :collection inspector))
	    *inspector-options*))))



;; Interface display

;; #### NOTE: I'm not sure, but I suppose that twiddling with the geometry is
;; better done here than in an INITIALIZE-INSTANCE :after method. Also, our
;; initialization function uses MAP-PANE-DESCENDANT-CHILDREN which only works
;; on displayed items, so we cannot set the state of the interface earlier
;; than this.
(defmethod interface-display :before ((etap etap))
  "Finalize ETAP interface's display settings.
This currently involves setting ETAP to the required state and fixating the
geometry of option panes so that resizing the interface is done sensibly."
  (funcall (capi-object-property etap :initialization-function))
  (let* ((layout (slot-value etap 'settings-1))
	 (size (multiple-value-list (simple-pane-visible-size layout))))
    (set-hint-table layout
      `(:visible-min-width ,(car size) :visible-max-width t
	:visible-min-height ,(cadr size) :visible-max-height t)))
  (let ((size (multiple-value-list
	       (simple-pane-visible-size (settings-2-layout etap)))))
    (set-hint-table (settings-2-layout etap)
      `(:visible-min-height ,(cadr size) :visible-max-height t))))



;; Utility protocols

;; #### WARNING: currently, this protocol is only used by penalty adjustment
;; dialogs, in order to avoid recomputing a new lineup while some of the
;; current break points are manipulated. Meanwhile, the paragraph width can
;; still be safely modified since it doesn't affect the lineup. This protocol
;; will need to be generalized for more complicated applications.

(defmethod enable-interface ((etap etap) &optional (enabled t))
  "Change ETAP interface's enabled status.
The zooming, clues, and paragraph width controls are always enabled.
The only interface controls which are subject to enabling / disabling are
those which may affect the lineup."
  ;; (setf (simple-pane-enabled (paragraph-width-cursor etap)) enabled)
  (enable-pane (options-2-layout etap) enabled)
  (enable-pane (settings-2-layout etap) enabled)
  (setf (enabled etap) enabled))

(defmethod river-detection-p ((etap etap))
  "Return T if river detection is enabled in ETAP interface."
  (river-detection-p (river-detection-dialog etap)))



;; State

(defun set-state
    (etap nlstring font features disposition algorithm width
     clues inspector zoom)
  "Set ETAP interface's widgets state."
  (setf (capi-object-property etap :original-nlstring) nlstring)
  (setf (capi-object-property etap :font) font)
  (setf (widget-value (features-box etap)) features)
  (setf (widget-value (disposition-type-box etap))
	(disposition-type disposition))
  (setf (widget-value (disposition-options-box etap))
	(disposition-options disposition))
  (let* ((algorithm-type (algorithm-type algorithm))
	 (algorithm-options (algorithm-options algorithm))
	 (tabs (algorithm-tabs etap))
	 (item (find algorithm-type (collection-items tabs) :key #'first)))
    (setf (choice-selected-item tabs) item)
    (setf (capi-object-property tabs :current-item) item)
    ;; #### WARNING: we're doing something a bit shaky here. In the algorithms
    ;; tabs, we currently have radio boxes corresponding to specific
    ;; properties, individual cursors lurking around, and finally "other
    ;; options" check boxes which regroup remaining boolean options. The
    ;; problem is that those check boxes are represented as widgets, although
    ;; their attached property is in fact meaningless. If someday we introduce
    ;; actual options with check-box behavior (as e.g. the visual clues),
    ;; we're gonna have a problem here. One solution would be to avoid using
    ;; check boxes as a mean to group options, and have check buttons lurking
    ;; around as cursors. In the meantime, we need to special-case check boxes
    ;; below and pass them the whole algorithm options list instead of trying
    ;; to getf their (inexistant) property. That also explains why the (SETF
    ;; WIDGET-VALUE) protocol on check boxes ignores unknown items.
    (map-pane-descendant-children (slot-value etap (second item))
      (lambda (child)
	(typecase child
	  (check-box
	   (setf (widget-value child) algorithm-options))
	  (widget
	   (setf (widget-value child)
		 (getf algorithm-options (property child))))))))
  (setf (widget-value (paragraph-width-cursor etap)) width)
  (setf (widget-value (clues-box etap)) clues)
  (setf (widget-value (inspector-box etap)) inspector)
  (enable-inspector (inspector-box etap))
  (setf (widget-value (zoom-cursor etap)) zoom)
  (setf (choice-selected-item (first (menu-items (language-menu etap))))
	(language nlstring))
  ;; #### WARNING: this callback mess is needed because programmatically
  ;; changing the editor pane's text triggers its change callback. This
  ;; entails two problems:
  ;; 1. it doesn't work when the interface is initialized (the call to
  ;;    top-level-interface returns nil),
  ;; 2. when calling this function from outside the interface, the application
  ;; logic (remake) would be executed twice.
  (setf (editor-pane-change-callback (text etap)) nil)
  (setf (editor-pane-text (text etap)) (text nlstring))
  (setf (editor-pane-change-callback (text etap)) 'text-change-callback))

(defun set-state-from-lineup (etap lineup width clues inspector zoom)
  "Set ETAP interface's widgets state using LINEUP."
  (set-state etap
    (nlstring lineup) (font lineup)
    (features lineup) (disposition lineup)
    (algorithm lineup) width
    clues inspector zoom))

(defun set-state-from-breakup (etap breakup clues inspector zoom)
  "Set ETAP interface's widgets state using BREAKUP."
  (set-state-from-lineup etap
    (lineup breakup) (paragraph-width breakup)
    clues inspector zoom))

(defun set-state-and-remake
    (etap
     breakup lineup nlstring font features disposition algorithm width
     layout clues inspector zoom)
  "Set ETAP interface's widgets state and remake as needed."
  (cond ((and (null lineup) (null breakup))
	 (set-state etap
	   nlstring font features disposition algorithm width
	   clues inspector zoom)
	 (remake etap))
	(lineup
	 (set-state-from-lineup etap lineup width clues inspector zoom)
	 (remake-with-lineup etap lineup))
	(breakup
	 (set-state-from-breakup etap breakup clues inspector zoom)
	 (remake-with-breakup etap breakup layout))))



;; ==========================================================================
;; Entry Points
;; ==========================================================================

(defun interface-state (interface)
  "Return the current state of INTERFACE as two values.
- The first value is a fully qualified context representing INTERFACE's
  current typesetting options (see `context').
- The second value is a fully qualified property list representing INTERFACE's
  current visualization options. This includes the visual clues, inspector
  options, and zoom factor.

See also `interface-breakup'."
  (values
   (make-context
    :font (capi-object-property interface :font)
    :algorithm (algorithm-specification interface)
    :disposition (disposition-specification interface)
    :features (widget-value (features-box interface))
    :paragraph-width (widget-value (paragraph-width-cursor interface))
    :text (editor-pane-text (text interface))
    :language (language-specification interface))
   (list
    :clues (widget-value (clues-box interface))
    :inspector (widget-value (inspector-box interface))
    :zoom (widget-value (zoom-cursor interface)))))

(defun interface-breakup (interface)
  "Return INTERFACE's breakup and displayed layout number as two values.
The layout number starts at 1 (technically, layout index + 1). 0 indicates
that the breakup does not contain any layout.

See also `interface-state'."
  (values (breakup interface) (layout interface)))

(defun visualize
    (&key (interface *interface*)
	  (context *context*)
	  (text
	   (if (and context (nlstring context))
	     (text (nlstring context))
	     *text*)
	   textp)
	  (language
	   (if (and context (nlstring context))
	     (language (nlstring context))
	     *language*)
	   languagep)
	  (font (if context (font context) *font*) fontp)
	  (features (when context (features context)) featuresp)
	  (kerning (getf features :kerning) kerningp)
	  (ligatures (getf features :ligatures) ligaturesp)
	  (hyphenation (getf features :hyphenation) hyphenationp)
	  (disposition (if context (disposition context) :flush-left)
		       dispositionp)
	  (algorithm (if context (algorithm context) :fixed) algorithmp)
	  lineup
	  (width (if context (paragraph-width context)
		     (caliber-default *paragraph-width*))
		 widthp)
	  breakup
	  layout
	  (clues '(:characters t))
	  inspector
	  (zoom 100)
     &aux (nlstring (if (or textp languagep (null context))
		      (make-nlstring :text text :language language)
		      (nlstring context)))
	  reuse)
  "Run a typesetting visualization with the specified parameters.
INTERFACE defaults to *INTERFACE*, which is initially null.
If INTERFACE is null, create a new interface. Otherwise, reuse the provided
one. In all cases, the interface is returned.

LAYOUT, ZOOM, and CLUES, are visualization options. The rest are typesetting
options.

- CONTEXT defaults to *CONTEXT*. See `context' for more information.
- Most other typesetting options are defaulted from the context, or to their
  corresponding global variable otherwise, but may be overridden on demand.
- Explicit features take precedence over FEATURES.
- Providing any typesetting option, except for CONTEXT, WIDTH, and LINEUP,
  will force recomputing the lineup and the breakup (see `make-lineup' and
  `make-breakup').
- Providing LINEUP or WIDTH will also force recomputing the breakup.

- LAYOUT is the breakup's layout number to display. This option is ignored if
  a new breakup is (re)computed (in which case the first layout is displayed).
- CLUES is a property list of things to display (see `*clues*' for more
  information). Only characters are displayed by default.
- INSPECTOR is a property list of inspector options (see `*inspector*' for
  more information).
- ZOOM factor is expressed in percentage (must be at least 1).

See also `interface-state' and `interface-breakup'."
  (cond (interface
	 (execute-with-interface-if-alive interface
	   (lambda (dialogs) (mapc #'destroy dialogs))
	   (penalty-adjustment-dialogs interface))
	 (setq reuse t))
	(t
	 (setq interface (make-instance 'etap))))
  (setq features (list :kerning kerning
		       :ligatures ligatures
		       :hyphenation hyphenation))
  (when (or textp languagep
	    fontp
	    featuresp kerningp ligaturesp hyphenationp
	    dispositionp
	    algorithmp)
    (setq lineup nil breakup nil))
  (when (or lineup widthp)
    (setq breakup nil))
  (cond (reuse
	 (execute-with-interface-if-alive interface
	   #'set-state-and-remake
	   interface
	   breakup lineup
	   nlstring font features disposition algorithm width
	   layout clues inspector zoom))
	(t
	 ;; See comment atop INTERFACE-DISPLAY about this.
	 (setf (capi-object-property interface :initialization-function)
	       (lambda ()
		 (set-state-and-remake
		  interface
		  breakup lineup
		  nlstring font features disposition algorithm width
		  layout clues inspector zoom)))
	 (display interface)))
  interface)
