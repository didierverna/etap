(in-package :etap)
(in-readtable :etap)

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
  ((prefix
    :documentation "This cursor's title prefix (defaults to the property."
    :initform nil :initarg :prefix :reader prefix)
   (caliber
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
    "Return a string of the form \"<prefix>: <calibrated value>\".
This is the default method."
    (format nil "~A: ~A"
      (title-capitalize (prefix cursor))
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
  (unless (prefix cursor) (setf (slot-value cursor 'prefix) (property cursor)))
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
    (title-capitalize (prefix cursor))
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
    (title-capitalize (prefix cursor))
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
    (title-capitalize (prefix cursor))
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
;; Living Text Dialog
;; ==========================================================================

;; --------------------
;; Line Waves Animation
;; --------------------

(defmacro define-lwaves-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd lwaves caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber lwaves ,name ,min ,default ,max ,@keys))

(defmacro calibrate-lwaves (name)
  "Calibrate NAMEd lwaves variable."
  `(calibrate sine ,name :earmuffs nil))

(define-lwaves-caliber amplitude 0 0 10 :bounded t)
(define-lwaves-caliber ondulation 0 0 400 :bounded t)
(define-lwaves-caliber propagation 0 0 100 :bounded t)

(defstruct lwave amplitude ondulation propagation phase)

;; #### NOTE: lines are differentiated from each other only by their vertical
;; position, which explains why Y is used for both horizontal and vertical
;; shifting.
(defun lwaves-shift (y lwave)
  "Return an LWAVE shifting amount for Y position."
  (+ (lwave-amplitude lwave) ; preserve the paragraph's left border
     (* (lwave-amplitude lwave)
	(sin (+ (lwave-phase lwave)
		(/ (* 2 pi (lwave-ondulation lwave) y) 20000))))))

(defun lwaves-step (lwave-x lwave-y)
  (incf (lwave-phase lwave-x) (/ (lwave-propagation lwave-x) 100))
  (incf (lwave-phase lwave-y) (/ (lwave-propagation lwave-y) 100)))



;; Line waves GUI elements

(defun lwaves-cursor-callback
    (cursor value gesture
     &aux (dialog (top-level-interface cursor))
	  (etap (etap dialog))
	  (view (view-area etap)))
  "Function called when a Line Waves animation's CURSOR is dragged.
Update CURSOR's title and propagate the new value where appropriate."
  (declare (ignore value))
  (when (eq gesture :drag)
    (update-cursor-title cursor)
    (let ((lwave-x (capi-object-property view :lwave-x))
	  (lwave-y (capi-object-property view :lwave-y)))
      (ecase (property cursor) ; Yuck!
	(:xamp (setf (lwave-amplitude lwave-x) (widget-value cursor)))
	(:xond (setf (lwave-ondulation lwave-x) (widget-value cursor)))
	(:xprop (setf (lwave-propagation lwave-x) (widget-value cursor)))
	(:yamp (setf (lwave-amplitude lwave-y) (widget-value cursor)))
	(:yond (setf (lwave-ondulation lwave-y) (widget-value cursor)))
	(:yprop (setf (lwave-propagation lwave-y) (widget-value cursor))))
      (unless (capi-object-property view :living-text-animation)
	(redraw etap)))))

(defun lwaves-phase-reset-callback
    (data dialog &aux (etap (etap dialog)) (view (view-area etap)))
  "Function called when a Line Waves animation's phase reset button is pushed."
  (ecase data
    (:x (setf (lwave-phase (capi-object-property view :lwave-x)) 0))
    (:y (setf (lwave-phase (capi-object-property view :lwave-y)) 0)))
  (unless (capi-object-property view :living-text-animation) (redraw etap)))



;; Living Text Interface
;; ---------------------

(defun living-text-timer (view)
  "Living text timer function.
If the animation should continue running, call the animation's stepper
function and redraw. Otherwise, return :STOP."
  (cond ((capi-object-property view :living-text-animation)
	 (funcall (capi-object-property view :living-text-step))
	 (redisplay-element view))
	(t
	 :stop)))

(defun living-text-start/stop-callback
    (button dialog &aux (view (view-area (etap dialog))))
  "Function called when the living text start/stop BUTTON is pushed.
Switch the animation:
- indicate the new status in the Etap view's property,
- update the button's data (hence its title),
- Upon running, start the animation timer."
  (cond ((capi-object-property view :living-text-animation)
	 (setf (capi-object-property view :living-text-animation) nil)
	 (setf (item-data button) :run-animation))
	(t
	 (setf (item-data button) :stop-animation)
	 (setf (capi-object-property view :living-text-animation) t)
	 (mp:schedule-timer-relative-milliseconds
	  (mp:make-timer 'living-text-timer view) 30 30))))

(defun living-text-destroy-callback
    (dialog &aux (etap (etap dialog)) (view (view-area etap)))
  "Function called when the living text DIALOG is destroyed.
Stop animation if running, uninstall the living text, and redraw."
  (setf (capi-object-property view :living-text-animation) nil)
  (setf (item-data (start/stop-button dialog)) :run-animation)
  (setf (capi-object-property view :line-x-shift) nil)
  (setf (capi-object-property view :line-y-shift) nil)
  (setf (capi-object-property view :elt-x-shift) nil)
  (setf (capi-object-property view :elt-y-shift) nil)
  (redraw etap))

(defgeneric living-text-install-animation (animation view)
  (:documentation "Install ANIMATION in VIEW.")
  (:method ((animation (eql :line-waves)) view)
    (let ((lwave-x (capi-object-property view :lwave-x))
	  (lwave-y (capi-object-property view :lwave-y)))
      (unless lwave-x
	(setq lwave-x (make-lwave
		       :phase 0
		       :amplitude (caliber-default *lwaves-amplitude*)
		       :ondulation (caliber-default *lwaves-ondulation*)
		       :propagation (caliber-default *lwaves-propagation*)))
	(setf (capi-object-property view :lwave-x) lwave-x))
      (unless lwave-y
	(setq lwave-y (make-lwave
		       :phase 0
		       :amplitude (caliber-default *lwaves-amplitude*)
		       :ondulation (caliber-default *lwaves-ondulation*)
		       :propagation (caliber-default *lwaves-propagation*)))
	(setf (capi-object-property view :lwave-y) lwave-y))
      (setf (capi-object-property view :line-x-shift)
	    (lambda (line) (lwaves-shift (y line) lwave-x)))
      (setf (capi-object-property view :line-y-shift)
	    (lambda (line) (lwaves-shift (y line) lwave-y)))
      (setf (capi-object-property view :living-text-step)
	    (lambda () (lwaves-step lwave-x lwave-y))))))

(defun living-text-animation-tabs-callback
    (item dialog &aux (view (view-area (etap dialog))))
  "Function called when a living text animation tab is selected.
Stop animation if running and install the new one."
  (when (capi-object-property view :living-text-animation)
    (setf (capi-object-property view :living-text-animation) nil)
    (setf (item-data (start/stop-button dialog)) :run-animation))
  (living-text-install-animation (first item) view))

(define-interface living-text ()
  ((etap :reader etap))
  (:panes
   (animation-tabs tab-layout
     :visible-max-width nil
     :combine-child-constraints t
     :items '((:line-waves lwaves-settings))
     :print-function (lambda (item) (title-capitalize (car item)))
     :callback-type '(:item :interface)
     :selection-callback 'living-text-animation-tabs-callback
     :visible-child-function #'second
     :reader animation-tabs)
   (xamp pt-cursor
     :prefix :amplitude
     :property :xamp
     :caliber *lwaves-amplitude*
     :callback 'lwaves-cursor-callback)
   (xond cursor
     :prefix :ondulation
     :property :xond
     :caliber *lwaves-ondulation*
     :callback 'lwaves-cursor-callback)
   (xprop cursor
     :prefix :propagation
     :property :xprop
     :caliber *lwaves-propagation*
     :callback 'lwaves-cursor-callback)
   (xphase push-button
     :text "Reset Phase"
     :data :x
     :callback 'lwaves-phase-reset-callback)
   (yamp pt-cursor
     :prefix :amplitude
     :property :yamp
     :caliber *lwaves-amplitude*
     :callback 'lwaves-cursor-callback)
   (yond cursor
     :prefix :ondulation
     :property :yond
     :caliber *lwaves-ondulation*
     :callback 'lwaves-cursor-callback)
   (yprop cursor
     :prefix :propagation
     :property :yprop
     :caliber *lwaves-propagation*
     :callback 'lwaves-cursor-callback)
   (yphase push-button
     :text "Reset Phase"
     :data :y
     :callback 'lwaves-phase-reset-callback)
   (start/stop push-button
     :data :run-animation
     :print-function 'title-capitalize
     :callback-type '(:item :interface)
     :callback 'living-text-start/stop-callback
     :reader start/stop-button))
  (:layouts
   (main column-layout '(animation-tabs start/stop) :adjust :center)
   (lwaves-settings row-layout '(lwaves-horizontal lwaves-vertical))
   (lwaves-horizontal column-layout '(xamp xond xprop xphase)
     :title "Horizontal" :title-position :frame :adjust :center)
   (lwaves-vertical column-layout '(yamp yond yprop yphase)
     :title "Vertical" :title-position :frame :adjust :center))
  (:default-initargs
   :title "Living Text"
   :window-styles '(:always-on-top t :toolbox t)
   :destroy-callback 'living-text-destroy-callback))

(defmethod interface-display :before
    ((dialog living-text) &aux (etap (etap dialog)))
  "Function called when the living text DIALOG is displayed.
Install the Line Waves functions and data structures, and redraw."
  (living-text-install-animation
   (first (choice-selected-item (animation-tabs dialog))) (view-area etap))
  (redraw etap))




;; ==========================================================================
;; River Detection Dialog
;; ==========================================================================


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

(defun river-angle-callback
    (cursor value gesture
     &aux (etap (etap (top-level-interface cursor))))
  "Function called when the river angle CURSOR is dragged.
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
     :caliber *river-angle*
     :enabled nil
     :callback 'river-angle-callback
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

(defun penalty-adjustment-destroy-callback (dialog &aux (etap (etap dialog)))
  "Function called when penalty adjustment DIALOG is destroyed.
- Possibly reenable the Etap interface if DIALOG was the last one."
  (setf (penalty-adjustment-dialogs etap)
	(delete dialog (penalty-adjustment-dialogs etap)))
  ;; We need to redraw the view because depending on other options, this
  ;; dialog's corresponding clue may have to go away.
  (redraw etap)
  (unless (penalty-adjustment-dialogs etap) (enable-interface etap)))

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
- Advertise it in dialog's title pane.
- Set the break point's penalty to it.
- Remake from the current lineup."
  (when (eq gesture :drag)
    (setq value (calibrated-value (range-slug-start slider)
				  (caliber break-point)))
    (setf (title-pane-text (title-area dialog)) (princ-to-string value))
    (setf (penalty break-point) value)
    (remake-with-current-lineup etap)))

(defun penalty-adjustment-reset-callback
    (value dialog &aux (slider (value-slider dialog)))
  "Function called when a penalty adjustment reset button is clicked.
- Set the slider to the reset VALUE.
- Call the slider's callback (that is, perform as if the value slider had been
  dragged)."
  (setf (range-slug-start slider) value)
  (penalty-adjustment-value-callback slider value :drag)) ;; whooo...

(define-interface penalty-adjustment ()
  ((break-point :initarg :break-point :reader break-point)
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
     ;; The reset buttons are created dynamically in the INITIALIZE-INSTANCE
     ;; after method (which see) because their number varies depending on the
     ;; kind of penalty we're adjusting.
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
	       (caliber (caliber break-point))
	       (original-value
		(decalibrated-value (penalty break-point) caliber))
	       reset-buttons)
  "Finish initializing penalty adjustment DIALOG.
- Create the reset buttons (two or three, depending on whether the default
  value is customizable).
- Set the slider's range start, end, and slug start based on the break point's
  caliber.
- Set DIALOG's title pane."
  (push (make-instance 'push-button
	  :text "Reset to Default"
	  :data (caliber-default caliber)
	  :visible-max-width nil)
	reset-buttons)
  (let ((widget (find-widget
		 (caliber-property caliber)
		 (slot-value
		  etap
		  (second (choice-selected-item (algorithm-tabs etap)))))))
    (when widget
      (push (make-instance 'push-button
	      :text "Reset to Global"
	      :data (range-slug-start widget)
	      :visible-max-width nil)
	    reset-buttons)))
  (push (make-instance 'push-button
	  :text "Reset to Original"
	  :data original-value
	  :visible-max-width nil)
	reset-buttons)
  (setf (collection-items (slot-value dialog 'reset)) reset-buttons)
  (setf (range-start slider)      (caliber-min caliber)
	(range-end slider)        (caliber-max caliber)
	(range-slug-start slider) original-value)
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

(defparameter *border-width* 20
  "The width of the border around the paragraph.")

(defparameter *clues*
  '(:characters :hyphenation-points :whitespaces :ends-of-line
    :over/underfull-boxes :overshrunk/stretched-boxes
    :rivers
    :paragraph-box :line-boxes :character-boxes :baselines)
  "The visual clues available for conditional display.")

(defparameter *inspector-options*
  '(:activate :tooltips)
  "The inspector options.")

(defparameter *zoom* (make-caliber :zoom 1 100 500 :bounded :min)
  "The paragraph zoom caliber.")

(defun zoom-value (etap)
  "Return ETAP's zoom value from its percentage cursor value."
  (/ (range-slug-start (zoom-cursor etap)) 100))



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
- Destroy the living text dialog.
- Destroy all remaining penalty adjustment dialogs."
  (destroy (river-detection-dialog etap))
  (destroy (living-text-dialog etap))
  (mapc #'destroy (penalty-adjustment-dialogs etap)))



;; ETAP menu

(defun menu-callback (item etap)
  "Function called when the ETAP menu is popped up."
  (ecase item
    (:select-font
     (when-let (font (prompt-for-font "Please select Latin Modern Roman 10pt"))
       (setf (simple-pane-font (view-area etap)) font)
       (redraw etap)))
    (:river-detection
     (display (river-detection-dialog etap) :owner etap))
    (:living-text
     (display (living-text-dialog etap) :owner etap))))



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
       (display-tooltip (view-area etap))
       (mapc #'destroy (penalty-adjustment-dialogs etap))))
    (:tooltips
     (unless (getf (widget-value inspector) :tooltips)
       (display-tooltip (view-area etap))))))



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

;; #### NOTE: the paragraph geometry (box) currently doesn't follow the living
;; text. This has an impact on the motion and post-menu callbacks, which may
;; end up /not/ detecting objects under the pointer when we're too far away
;; from the paragraph box. We don't really care though, since the living text
;; is a different experiment from actual typesetting.

;; CLIM-like object under mouse utilities

;; #### NOTE: this function is *not* a `line-under' function. It only checks
;; for a vertical match since it is also used to advertise a line's properties
;; when the mouse is in the left margin.
(defun line-under-y (y lines line-y-shift)
  "Return the line from LINES which is under Y coordinate, or NIL."
  (find-if (lambda (line &aux (ly (+ (y line) (funcall line-y-shift line))))
	     (<= (- ly (height line)) y (+ ly (depth line))))
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

;; #### TODO: triangular clues are not completely factored out. The triangle
;; coordinates (involving +/-3 on X and +5 on Y) are hardwired here and also
;; in DRAW-TRIANGLE.
(defun clue-under (x y lines line-x-shift line-y-shift elt-x-shift elt-y-shift)
  "Return the shifted clue from LINES which is under (X, Y), or nil.
In the case of a discretionary clue, it is returned only if it corresponds to
an hyphenation point (as opposed to a general discretionary). The object
returned is in fact the pin containing the clue.
Technically, (X, Y) is not over the clue (which is a 0-sized object), but over
its visual representation (the small triangle beneath it).
This function returns the corresponding line as a second value."
  (when-let (line (find-if (lambda (line
				    &aux (ly (+ (y line)
						(funcall line-y-shift line))))
			     (<= ly y (+ ly 5)))
			   lines))
    (let ((p (cons x y))
	  (lx (+ (x line) (funcall line-x-shift line)))
	  (ly (+ (y line) (funcall line-y-shift line))))
      (values (find-if (lambda (item
				&aux (ix (+ lx
					    (x item)
					    (funcall elt-x-shift item)))
				     (iy (+ ly (funcall elt-y-shift item))))
			 (and (cluep (object item))
			      (or (not (discretionaryp (helt (object item))))
				  (hyphenation-point-p (helt (object item))))
			      (triangle-under-p
			       p
			       (cons ix iy)
			       (cons (+ ix -3) (+ iy 5))
			       (cons (+ ix +3) (+ iy 5)))))
		       (items line))
	      line))))

(defun whitespace-under
    (x y lines line-x-shift line-y-shift elt-x-shift elt-y-shift)
  "Return the shifted whitespace from LINES which is under (X, Y), or nil.
This function returns the corresponding line as a second value."
  (when-let (line (line-under-y y lines line-y-shift))
    (values (find-if (lambda (item
			      &aux (ix (+ (x line) (funcall line-x-shift line)
					  (x item) (funcall elt-x-shift item)))
				   (iy (+ (y line) (funcall line-y-shift line)
					  (funcall elt-y-shift item))))
		       (and (whitespacep item)
			    (<= ix x (+ ix (width item)))
			    (<= (- iy (height item)) y iy)))
		     (items line))
	    line)))

(defun object-under
    (x y lines line-x-shift line-y-shift elt-x-shift elt-y-shift)
  "Return the object from LINES which is under (X, Y), or nil.
This currently includes whitespaces and pinned clues.
For clues, (X, Y) is not technically over it, but over the corresponding
visual representation (the small triangle beneath it).
This function returns the corresponding line as a second value."
  ;; No-can-do with OR. Need second values.
  (multiple-value-bind (object line)
      (clue-under
       x y lines line-x-shift line-y-shift elt-x-shift elt-y-shift)
    (if object
      (values object line)
      (whitespace-under
       x y lines line-x-shift line-y-shift elt-x-shift elt-y-shift))))



;; #### NOTE: the paragraph coordinates systems is defined as follows. The X
;; axis is the baseline of the first line, and the Y axis is the paragraph's
;; left border, oriented downwards. This means, in particular that the
;; top-left corner has coordinates (0, - first line's height).

;; #### WARNING: in the few functions below, in case there's no layout, we
;; rely on (HEIGHT NIL) or (DEPTH NIL) = 0, which is perhaps a bit
;; borderline...

(defmacro to-layout-coordinates (x y layout zoom)
  "Convert X and Y to LAYOUT coordinates with ZOOM factor.
Originally, X and Y are expressed in the paragraph view's coordinate system.
X and Y must be variable names (not evaluated). They are modified in-place."
  (let ((the-zoom (gensym "the-zoom")))
    `(let ((,the-zoom ,zoom))
       (setq ,x (/ (- ,x *border-width*) ,the-zoom)
	     ,y (/ (- ,y *border-width*) ,the-zoom))
       (decf ,y (height ,layout)))))



;; Motion

;; #### NOTE: the visual clues occurring at an end of line (resp. last line)
;; will extend beyond the paragraph's right (resp. bottom) side. In order to
;; handle those and still optimize a bit, we only call OBJECT-UNDER if the
;; pointer is above the paragraph, but *BORDER-WIDTH* included on the right
;; and bottom sides.

(defun motion-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (inspect (widget-value (inspector-box etap))))
  "Function called when the mouse is moved in the paragraph VIEW.
- Display the paragraph properties when the mouse is above the first line.
- Display the line properties when the mouse is in the left margin of a line.
- Otherwise, display the properties of the object under mouse (currently, a
  hyphenation point)."
  (setf (capi-object-property view :pointer) (cons x y))
  (when (getf inspect :activate)
    ;; We have the view directly here, so no need to go through REDRAW. ####
    ;; TODO: this could be optimized in order to avoid redrawing at every
    ;; single move. We could remember the previous move state and see if
    ;; something has changed.
    (gp:invalidate-rectangle view)
    (when (getf inspect :tooltips)
      (let* ((breakup (breakup etap))
	     (par-width (paragraph-width breakup))
	     (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	     (layout (when layout-# (get-layout layout-# breakup))))
	(to-layout-coordinates x y layout (zoom-value etap))
	(cond ((and (< y (- (height layout))) (<= x par-width))
	       (display-tooltip view
	       :text (properties breakup :layout-# layout-#)))
	      (layout
	       (let ((line-x-shift
		       (or (capi-object-property view :line-x-shift)
			   (lambda (line) (declare (ignore line)) 0)))
		     (line-y-shift
		       (or (capi-object-property view :line-y-shift)
			   (lambda (line) (declare (ignore line)) 0)))
		     (elt-x-shift
		       (or (capi-object-property view :elt-x-shift)
			   (lambda (elt) (declare (ignore elt)) 0)))
		     (elt-y-shift
		       (or (capi-object-property view :elt-y-shift)
			   (lambda (elt) (declare (ignore elt)) 0))))
		 (cond ((and (< x 0) (<= y (depth layout)))
			(if-let (line (line-under-y y (lines layout)
					line-y-shift))
			  (display-tooltip view :text (properties line))
			  (display-tooltip view)))
		       ((and (<= 0 x (+ par-width *border-width*))
			     (<= (- (height layout))
				 y
				 (+ (depth layout) *border-width*)))
			(if-let (object (object-under x y (lines layout)
				  line-x-shift line-y-shift
				  elt-x-shift elt-y-shift))
			  (display-tooltip view :text (properties object))
			  (display-tooltip view))))))
	      (t
	       (display-tooltip view)))))))



;; Post Menu

;; #### NOTE: the visual clues occurring at an end of line (resp. last line)
;; will extend beyond the paragraph's right (resp. bottom) side. In order to
;; handle those and still optimize a bit, we only call OBJECT-UNDER if the
;; pointer is above the paragraph, but *BORDER-WIDTH* included on the right
;; and bottom sides.

;; #### TODO: when this gets enriched, we will eventually end up with the same
;; logic as in MOTION-CALLBACK in order to figure out what's under the mouse,
;; and we already wish we used CLIM...
(defun post-menu-callback
    (view x y
     &aux (etap (top-level-interface view))
	  (breakup (breakup etap))
	  (layout-# (let ((i (1- (layout etap)))) (when (>= i 0) i)))
	  (layout (when layout-# (get-layout layout-# breakup))))
  "Function called when the user right clicks in the paragraph VIEW.
This does nothing if the inspector is not active. Otherwise, it currently
displays a penalty adjustment dialog when appropriate."
  (when (and (getf (widget-value (inspector-box etap)) :activate) layout)
    (to-layout-coordinates x y layout (zoom-value etap))
    (let* ((line-x-shift (or (capi-object-property view :line-x-shift)
			     (lambda (line) (declare (ignore line)) 0)))
	   (line-y-shift (or (capi-object-property view :line-y-shift)
			     (lambda (line) (declare (ignore line)) 0)))
	   (elt-x-shift (or (capi-object-property view :elt-x-shift)
			    (lambda (elt) (declare (ignore elt)) 0)))
	   (elt-y-shift (or (capi-object-property view :elt-y-shift)
			    (lambda (elt) (declare (ignore elt)) 0)))
	   (object (and (<= 0 x (+ (paragraph-width breakup) *border-width*))
			(<= (- (height layout))
			    y
			    (+ (depth layout) *border-width*))
			(object-under x y (lines layout)
			  line-x-shift line-y-shift
			  elt-x-shift elt-y-shift))))
      (when object
	(setq object
	      (etypecase (object object)
		(clue (helt (object object)))
		(glue (object object))))
	;; #### FIXME: see comment on top of BREAK-POINT. This entails the
	;; complexity of handling null calibers below.
	(when (caliber object)
	  (make-penalty-adjustment-dialog object etap))))))



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

;; #### TODO: see comment atop CLUE-UNDER.
(defun draw-triangle (view x y &rest args)
  "Draw a triangular clue in VIEW at (X,Y).
ARGS are subsequently passed to the drawing function."
  (apply #'gp:draw-polygon view (list x y (- x 3) (+ y 5) (+ x 3) (+ y 5) x y)
	 args))

(defun draw-penalty-clue (view x y break-point &optional (filled t))
  "Draw a triangle clue in VIEW at (X,Y) for BREAK-POINT.
The triangle may be FILLED (T by default), and its color is computed based on
  BREAK-POINT's penalty."
  (draw-triangle view x y
      :filled filled
      :foreground (color:make-hsv (penalty-hue break-point) 1s0 .7s0)))

(defgeneric draw-clue (view x y helt &optional force)
  (:documentation "Draw a clue in VIEW at (X,Y) for HELT.")
  (:method (view x y (hyphenation-point hyphenation-point) &optional force)
    "Draw clue in VIEW at (X,Y) for HYPHENATION-POINT.
The clue is outlined or filled, depending on whether HYPHENATION-POINT is
explicit or computed."
    (declare (ignore force))
    (draw-penalty-clue view x y hyphenation-point
      (not (explicitp hyphenation-point))))
  (:method (view x y (glue glue) &optional force)
    "Draw clue in VIEW at (X,Y) for end of line GLUE.
Unless FORCE, the clue is drawn only if the corresponding glue's penalty is
not 0."
  (when (or force
	    (not (zerop (decalibrated-value (penalty glue) (caliber glue)))))
    (draw-penalty-clue view x y glue))))

(defun draw-whitespace-clue
    (view x y elt-x-shift elt-y-shift whitespace
     &optional force
     &aux (glue (object whitespace)))
  "Draw a whitespace clue in VIEW relatively to (X,Y) for WHITESPACE.
(X,Y) is the point which WHITESPACE is positioned relatively to.
Unless FORCE, the clue is drawn only if the corresponding glue's penalty is
not 0."
  (when (or force
	    (not (zerop (decalibrated-value (penalty glue) (caliber glue)))))
    (let ((h/2 (/ (+ (height whitespace) (depth whitespace)) 2)))
      (gp:draw-circle view
	(+ x (x whitespace) (funcall elt-x-shift whitespace)
	   (/ (width whitespace) 2))
	(- (+ y (funcall elt-y-shift whitespace)) h/2)
	h/2
	:filled t ;; (not (explicitp discretionary))
	:foreground (color:make-hsv (penalty-hue glue) 1s0 .7s0)))))

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
	  (zoom (zoom-value etap))
	  (line-x-shift (or (capi-object-property view :line-x-shift)
			    (lambda (line) (declare (ignore line)) 0)))
	  (line-y-shift (or (capi-object-property view :line-y-shift)
			    (lambda (line) (declare (ignore line)) 0)))
	  (elt-x-shift (or (capi-object-property view :elt-x-shift)
			   (lambda (elt) (declare (ignore elt)) 0)))
	  (elt-y-shift (or (capi-object-property view :elt-y-shift)
			   (lambda (elt) (declare (ignore elt)) 0))))
  "Function called when paragraph VIEW needs to be redrawn."
  (declare (ignore x y width height))
  (set-horizontal-scroll-parameters view
    :max-range (+ (* par-width zoom) (* 2 *border-width*)))
  (set-vertical-scroll-parameters view
    :max-range (+ (* par-h+d zoom) (* 2 *border-width*)))
  (gp:with-graphics-translation (view *border-width* *border-width*)
    (gp:with-graphics-scale (view zoom zoom)
      (when (member :paragraph-box clues)
	(gp:draw-rectangle view 0 0 par-width par-h+d
	  :foreground :red
	  :scale-thickness nil))
      (when layout
	(loop :with full-x
		:= (+ (loop :for line :in (lines layout)
			    :maximize (+ (x line)
					 (funcall line-x-shift line)
					 (width line)))
				 5)
	      :for rest :on (lines layout)
	      :for line := (car rest)
	      :for lx := (+ (x line) (funcall line-x-shift line))
	      :for ly := (+ par-y (y line) (funcall line-y-shift line))
	      :when (member :line-boxes clues)
		:do (gp:draw-rectangle view
			lx
			(- ly (height line))
			(width line)
			(+ (height line) (depth line))
		      :foreground :blue
		      :scale-thickness nil)
	      :when (member :over/underfull-boxes clues)
		:if (> (width line) par-width)
		  :do (gp:draw-rectangle view
			  full-x  (- ly (height line))
			  5  (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr rest) ;; not the last one
			       (eq (disposition-type (disposition breakup))
				   :justified)
			       (< (width line) par-width))
		  :do (gp:draw-rectangle view
			  full-x (- ly (height line))
			  5 (+ (height line) (depth line))
			:foreground :orange
			:scale-thickness nil :filled nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if ($< (esar line) (asar line))
		  :do (gp:draw-polygon view
			  (list (+ full-x 5)
				(- ly (height line))
				(+ full-x 11)
				(- ly (height line))
				(+ full-x 8)
				(+ ly (depth line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($< (asar line) -1)
		  :do (gp:draw-polygon view
			  (list (+ full-x 5)
				(- ly (height line))
				(+ full-x 11)
				(- ly (height line))
				(+ full-x 8)
				(+ ly (depth line)))
			:foreground :blue
			:scale-thickness nil :filled nil :closed t)
		:else :if ($> (esar line) (asar line))
		  :do (gp:draw-polygon view
			  (list (+ full-x 5)
				(+ ly (depth line))
				(+ full-x 11)
				(+ ly (depth line))
				(+ full-x 8)
				(- ly (height line)))
			:foreground :blue
			:scale-thickness nil :filled t :closed t)
		:else :if ($> (asar line) 1)
		  :do (gp:draw-polygon view
			  (list (+ full-x 5)
				(+ ly (depth line))
				(+ full-x 11)
				(+ ly (depth line))
				(+ full-x 8)
				(- ly (height line)))
			:foreground :blue
			:scale-thickness nil :filled nil :closed t)
	      :when (member :baselines clues)
		:do (gp:draw-line view lx ly (+ lx (width line)) ly
		      :foreground :purple
		      :scale-thickness nil)
	      :when (or (member :characters clues)
			(member :character-boxes clues))
		:do (mapc (lambda (item
				   &aux (ix (+ lx
					       (x item)
					       (funcall elt-x-shift item)))
					(iy (+ ly
					       (funcall elt-y-shift item))))
			    (cond ((typep (object item)
					  'tfm:character-metrics)
				   (when (member :character-boxes clues)
				     (gp:draw-rectangle view
					 ix
					 (- iy (height item))
					 (width item)
					 (+ (height item) (depth item))
				       :scale-thickness nil))
				   (when (member :characters clues)
				     (gp:draw-character view
					 (svref *lm-ec*
					       (tfm:code (object item)))
					 ix iy)))
				  ((and (cluep (object item))
					(hyphenation-point-p
					 (helt (object item)))
					(or (member :hyphenation-points clues)
					    (find-penalty-adjustment-dialog
					     (helt (object item))
					     etap)))
				   (draw-clue view ix iy (helt (object item))))
				  ((and (cluep (object item))
					(gluep (helt (object item)))
					(or (member :ends-of-line clues)
					    (find-penalty-adjustment-dialog
					     (helt (object item))
					     etap)))
				   (draw-clue view ix iy (helt (object item))
				     (find-penalty-adjustment-dialog
				      (helt (object item))
				      etap)))
				  ((and (whitespacep item)
					(or (member :whitespaces clues)
					    (find-penalty-adjustment-dialog
					     (object item)
					     etap)))
				   (draw-whitespace-clue
				    view lx ly elt-x-shift elt-y-shift item
				    (find-penalty-adjustment-dialog
				     (object item) etap)))))
		      (items line)))
	(when (getf (widget-value (inspector-box etap)) :activate)
	  (multiple-value-bind (object line)
	      (let* ((pointer (capi-object-property view :pointer))
		     (x (car pointer))
		     (y (cdr pointer)))
		(to-layout-coordinates x y layout zoom)
		(object-under x y (lines layout)
		  line-x-shift line-y-shift elt-x-shift elt-y-shift))
	    ;; #### WARNING: we may end up drawing a clue for the second time
	    ;; here, but this is probably not such a big deal.
	    (when object
	      (let* ((lx (+ (x line) (funcall line-x-shift line)))
		     (ly (+ par-y (y line) (funcall line-y-shift line)))
		     (ix (+ lx (x object) (funcall elt-x-shift object)))
		     (iy (+ ly (funcall elt-y-shift object))))
		(cond ((and (cluep (object object))
			    (discretionaryp (helt (object object))))
		       (draw-clue view ix iy (helt (object object))))
		      ((and (cluep (object object))
			    (gluep (helt (object object))))
		       (draw-clue view ix iy (helt (object object)) :force))
		      ((whitespacep object)
		       (draw-whitespace-clue view lx ly
			 elt-x-shift elt-y-shift object 'force)))))))
	;; #### NOTE: Rivers are currently *not* recomputed in living text.
	;; They just follow the text movement. In theory, rivers could be
	;; different at each step of the ondulation though.
	(when (and (member :rivers clues) (rivers etap))
	  (maphash (lambda (source arms)
		     (mapc (lambda (arm &aux (mouth (mouth arm)))
			     (gp:draw-line view
				 (+ (x (board source))
				    (funcall line-x-shift (board source))
				    (x source)
				    (funcall elt-x-shift source)
				    (/ (width source) 2))
				 (+ par-y
				    (y (board source))
				    (funcall line-y-shift (board source))
				    (y source)
				    (funcall elt-y-shift source))
				 (+ (x (board mouth))
				    (funcall line-x-shift (board mouth))
				    (x mouth)
				    (funcall elt-x-shift mouth)
				    (/ (width mouth) 2))
				 (+ par-y
				    (y (board mouth))
				    (funcall line-y-shift (board mouth))
				    (y mouth)
				    (funcall elt-y-shift mouth))
			       :foreground :red :thickness 1))
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
   (living-text-dialog
    :initform (make-instance 'living-text)
    :reader living-text-dialog)
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
   (etap-menu "ETAP" (:select-font :river-detection :living-text)
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
  (setf (slot-value (living-text-dialog etap) 'etap) etap)
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
