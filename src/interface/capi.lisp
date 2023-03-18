(in-package :etap)

;; =========
;; Utilities
;; =========

(defun update (interface &aux (context (context interface)))
  "Update INTERFACE.
This recreates the typeset paragraph and invalidates the GUI's view."
  (setf (paragraph interface) (make-paragraph :context context))
  (gp:invalidate-rectangle (view interface)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun title-capitalize (title)
    "Capitalize TITLE and substitute dashes with spaces."
    (nsubstitute #\Space #\- (string-capitalize title))))


;; -------
;; Sliders
;; -------

(defmacro define-slider-callback
    (name &aux (name (string name))
	       (dash-position (position #\- name))
	       (prefix (subseq name 0 dash-position))
	       (generic (subseq name (1+ dash-position))))
  "Define a SET-NAME slider callback function.
NAME (a symbol) must be of the form PREFIX-PROPERTY."
    `(defun ,(intern (concatenate 'string "SET-" name)) (pane value status)
       (declare (ignore status))
       (setf (titled-object-title pane)
	     (format nil
		 ,(concatenate 'string (title-capitalize generic) ": ~D")
	       value))
       (,(intern (concatenate 'string "SET-" prefix "-ALGORITHM"))
	nil (top-level-interface pane))))

(defmacro define-slider-callbacks (&rest names)
  "Define slider callback functions for NAMES."
  `(progn ,@(mapcar (lambda (name) `(define-slider-callback ,name)) names)))

(defmacro slider-value (prefix key interface)
  "Return (:KEY (RANGE-SLUG-START (PREFIX-KEY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (string prefix) "-" (string key)))))
    `(list ,key (range-slug-start (,accessor ,interface)))))


;; ------
;; Radios
;; ------

(defmacro radio-selection (prefix key interface)
  "Return (:KEY (CHOICE-SELECTED-ITEM (PREFIX-KEY INTERFACE)))."
  (let ((accessor (intern (concatenate 'string
			    (string prefix) "-" (string key)))))
    `(list ,key (choice-selected-item (,accessor ,interface)))))



;; =================
;; Interface Actions
;; =================

;; ----------
;; Algorithms
;; ----------

;; Fixed
(defun set-fixed-algorithm (value interface)
  "Set the current algorithm to Fixed in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :fixed
	      (apply #'append
		(radio-selection fixed :fallback interface)
		(slider-value fixed :width-offset interface)
		(choice-selected-items (fixed-options interface)))))
  (update interface))

;; DEFINE-SLIDER-CALLBACK doesn't handle more informative titles than just
;; displaying the values.
(defun set-fixed-width-offset (pane value status)
  (declare (ignore status))
  (setf (titled-object-title pane)
	(format nil "Width Offset: ~Dpt (~Fcm)"
	  value (/ value 28.452755)))
  (set-fixed-algorithm nil (top-level-interface pane)))


;; Fit
(defun set-fit-algorithm (value interface)
  "Set the current algorithm to Fit in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :fit
	      (apply #'append
		(radio-selection fit :variant interface)
		(radio-selection fit :fallback interface)
		(radio-selection fit :discriminating-function interface)
		(slider-value fit :hyphen-penalty interface)
		(slider-value fit :explicit-hyphen-penalty interface)
		(slider-value fit :width-offset interface)
		(choice-selected-items (fit-options interface)))))
  (update interface))

(define-slider-callback fit-hyphen-penalty)
(define-slider-callback fit-explicit-hyphen-penalty)
;; DEFINE-SLIDER-CALLBACK doesn't handle more informative titles than just
;; displaying the values.
(defun set-fit-width-offset (pane value status)
  (declare (ignore status))
  (setf (titled-object-title pane)
	(format nil "Width Offset: ~Dpt (~Fcm)"
	  value (/ value 28.452755)))
  (set-fit-algorithm nil (top-level-interface pane)))


;; Barnett
(defun set-barnett-algorithm (value interface)
  "Set the current algorithm to Barnett in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface)) '(:barnett))
  (update interface))


;; Duncan
(defun set-duncan-algorithm (value interface)
  "Set the current algorithm to Duncan in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :duncan
	      (radio-selection duncan :discriminating-function interface)))
  (update interface))


;; Knuth-Plass
(defun set-kp-algorithm (value interface)
  "Set the current algorithm to Knuth-Plass in INTERFACE's context."
  (declare (ignore value))
  (setf (algorithm (context interface))
	(cons :knuth-plass
	      (append
	       (radio-selection kp :variant interface)
	       (slider-value kp :line-penalty interface)
	       (slider-value kp :hyphen-penalty interface)
	       (slider-value kp :explicit-hyphen-penalty interface)
	       (slider-value kp :adjacent-demerits interface)
	       (slider-value kp :double-hyphen-demerits interface)
	       (slider-value kp :final-hyphen-demerits interface)
	       (slider-value kp :pre-tolerance interface)
	       (slider-value kp :tolerance interface)
	       (slider-value kp :emergency-stretch interface)
	       (slider-value kp :looseness interface))))
  (update interface))

(define-slider-callbacks kp-line-penalty
  kp-hyphen-penalty kp-explicit-hyphen-penalty
  kp-adjacent-demerits kp-double-hyphen-demerits kp-final-hyphen-demerits
  kp-pre-tolerance kp-tolerance
  kp-emergency-stretch
  kp-looseness)


(defun set-algorithm (value interface)
  "Select algorithm specified by VALUE in INTERFACE."
  (case (car value)
    (:fixed (set-fixed-algorithm value interface))
    (:fit (set-fit-algorithm value interface))
    (:barnett (set-barnett-algorithm value interface))
    (:duncan (set-duncan-algorithm value interface))
    (:knuth-plass (set-kp-algorithm value interface))))


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
  (setf (text (context interface)) (editor-pane-text pane))
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

(defun render-paragraph
    (pane x y width height
     &aux (interface (top-level-interface pane))
	  (context (context interface))
	  (paragraph (paragraph interface))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (clues (choice-selected-items (clues interface))))
  "Render PANE's paragraph."
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
	    :foreground :red
	    :scale-thickness nil))
	(loop :with par-y := (height (first (pinned-lines paragraph)))
	      :for pinned-lines :on (pinned-lines paragraph)
	      :for pinned-line := (car pinned-lines)
	      :for x := (x pinned-line)
	      :for y := (+ par-y (y pinned-line))
	      :when (member :line-boxes clues)
		:do (gp:draw-rectangle pane
			x
			(- y (height pinned-line))
			(width pinned-line)
			(+ (height pinned-line) (depth pinned-line))
		      :foreground :blue
		      :scale-thickness nil)
	      :when (member :over/underfull-boxes clues)
		:if (> (width pinned-line) (width paragraph))
		  :do (gp:draw-rectangle pane
			  (+ x (width pinned-line) 5)
			  (- y (height pinned-line))
			  5
			  (+ (height pinned-line) (depth pinned-line))
			:foreground :orange
			:scale-thickness nil :filled t)
		:else :if (and (cdr pinned-lines) ;; not the last one
			       (eq (disposition-type (disposition context))
				   :justified)
			       (< (width pinned-line) (width paragraph)))
			:do (gp:draw-rectangle pane
				(+ (width paragraph) 5)
				(- y (height pinned-line))
				5
				(+ (height pinned-line) (depth pinned-line))
			      :foreground :orange
			      :scale-thickness nil)
	      :when (member :overshrunk/stretched-boxes clues)
		:if (< (scale pinned-line) -1)
		  :do (gp:draw-rectangle pane
			  (+ (width paragraph) 5)
			  (- y (height pinned-line))
			  5
			  (+ (height pinned-line) (depth pinned-line))
			:foreground :blue
			:scale-thickness nil :filled t)
		:else :if (> (scale pinned-line) 1)
			:do (gp:draw-rectangle pane
				(+ (width paragraph) 5)
				(- y (height pinned-line))
				5
				(+ (height pinned-line) (depth pinned-line))
			      :foreground :blue
			      :scale-thickness nil)
	      :when (member :baselines clues)
		:do (gp:draw-line pane x y (+ x (width pinned-line)) y
		      :foreground :purple
		      :scale-thickness nil)
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
					    (depth object))
				       :scale-thickness nil))
				   (when (member :characters clues)
				     (gp:draw-character pane
					 (aref *lm-ec*
					       (tfm:code
						(character-metrics object)))
					 (+ x (x object))
					 y)))
				  ((pinned-hyphenation-clue-p object)
				   (when (member :hyphenation-points clues)
				     (gp:draw-polygon pane
				       (list (+ x (x object)) y
					     (+ x (x object) -3) (+ y 5)
					     (+ x (x object) +3) (+ y 5))
				       :filled t
				       :foreground (if (explicitp object)
						     :blue
						     :orange))))))
		      (pinned-objects (line pinned-line))))))))


;; Tooltips
(defparameter *tooltips*
  `(,@*fixed-tooltips* ,@*fit-tooltips* ,@*kp-tooltips*
    ,@*disposition-options-tooltips*)
  "The GUI's tooltips.")

(defun show-help (interface pane type key)
  "The GUI's help callback."
  (declare (ignore interface pane))
  (case type
    (:tooltip
     (typecase key
       (symbol (cadr (member key *tooltips*)))))))


(defun display-properties
    (pane x y
     &aux (interface (top-level-interface pane))
	  (zoom (/ (range-slug-start (zoom interface)) 100))
	  (paragraph (paragraph interface))
	  (pinned-lines (pinned-lines paragraph)))
  "Display the properties of the paragraph, or the line clicked on."
  (when pinned-lines
    (setq x (/ (- x 20) zoom)
	  y (- (/ (- y 20) zoom) (height (first (pinned-lines paragraph)))))
    (let ((line (when (and (>= x 0) (<= x (width paragraph)))
		  (find-if (lambda (line)
			     (and (>= y (- (y line) (height line)))
				  (<= y (+ (y line) (depth line)))))
			   (pinned-lines paragraph)))))
      (if line
	(display-tooltip pane
	  :text (format nil "Line scale: ~F." (coerce (scale line) 'float)))
	(display-tooltip pane)))))

;; Interface
(define-interface etap ()
  ((context :initform *context* :initarg :context :reader context)
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
	      (:knuth-plass kp-settings))
     :print-function (lambda (item) (title-capitalize (car item)))
     :visible-child-function 'second
     :selection-callback 'set-algorithm
     :reader algorithms)
   (fixed-fallback radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Fallback" :title-position :frame
     :items *fixed-fallbacks*
     :help-keys *fixed-fallbacks-help-keys*
     :print-function 'title-capitalize
     :selection-callback 'set-fixed-algorithm
     :reader fixed-fallback)
   (fixed-options check-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Options" :title-position :frame
     :items *fixed-options*
     :help-keys *fixed-options-help-keys*
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-fixed-algorithm
     :retract-callback 'set-fixed-algorithm
     :reader fixed-options)
   (fixed-width-offset slider
     :title (format nil "Width Offset: ~Dpt (~Fcm))"
	      (caliber-default *fixed-width-offset*)
	      (/ (caliber-default *fixed-width-offset*) 28.452755))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *fixed-width-offset*)
     :end (caliber-max *fixed-width-offset*)
     :slug-start (caliber-default *fixed-width-offset*)
     :tick-frequency 0
     :callback 'set-fixed-width-offset
     :reader fixed-width-offset)
   (fit-variant radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Variant" :title-position :frame
     :items *fit-variants*
     :help-keys *fit-variants-help-keys*
     :print-function 'title-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-variant)
   (fit-fallback radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Fallback" :title-position :frame
     :items *fit-fallbacks*
     :help-keys *fit-fallbacks-help-keys*
     :print-function 'title-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-fallback)
   (fit-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items *fit-options*
     :help-keys *fit-options-help-keys*
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-fit-algorithm
     :retract-callback 'set-fit-algorithm
     :reader fit-options)
   (fit-discriminating-function option-pane
     :title "Discriminating Function:"
     :items *fit-discriminating-functions*
     :print-function 'title-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-discriminating-function)
   (fit-hyphen-penalty slider
     :title (format nil "Hyphen Penalty: ~D"
	      (caliber-default *fit-hyphen-penalty*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *fit-hyphen-penalty*)
     :end (caliber-max *fit-hyphen-penalty*)
     :slug-start (caliber-default *fit-hyphen-penalty*)
     :tick-frequency 0
     :callback 'set-fit-hyphen-penalty
     :reader fit-hyphen-penalty)
   (fit-explicit-hyphen-penalty slider
     :title (format nil "Explicit-Hyphen Penalty: ~D"
	      (caliber-default *fit-explicit-hyphen-penalty*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *fit-explicit-hyphen-penalty*)
     :end (caliber-max *fit-explicit-hyphen-penalty*)
     :slug-start (caliber-default *fit-explicit-hyphen-penalty*)
     :tick-frequency 0
     :callback 'set-fit-explicit-hyphen-penalty
     :reader fit-explicit-hyphen-penalty)
   (fit-width-offset slider
     :title (format nil "Width Offset: ~Dpt (~Fcm))"
	      (caliber-default *fit-width-offset*)
	      (/ (caliber-default *fit-width-offset*) 28.452755))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *fit-width-offset*)
     :end (caliber-max *fit-width-offset*)
     :slug-start (caliber-default *fit-width-offset*)
     :tick-frequency 0
     :callback 'set-fit-width-offset
     :reader fit-width-offset)
   (duncan-discriminating-function option-pane
     :title "Discriminating Function:"
     :items *duncan-discriminating-functions*
     :print-function 'title-capitalize
     :selection-callback 'set-duncan-algorithm
     :reader duncan-discriminating-function)
   (kp-variant radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Variant" :title-position :frame
     :items *kp-variants*
     :help-keys *kp-variants-help-keys*
     :print-function 'title-capitalize
     :selection-callback 'set-kp-algorithm
     :reader kp-variant)
   (kp-line-penalty slider
     :title (format nil "Line Penalty: ~D"
	      (caliber-default *kp-line-penalty*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-line-penalty*)
     :end (caliber-max *kp-line-penalty*)
     :slug-start (caliber-default *kp-line-penalty*)
     :tick-frequency 0
     :callback 'set-kp-line-penalty
     :reader kp-line-penalty)
   (kp-hyphen-penalty slider
     :title (format nil "Hyphen Penalty: ~D"
	      (caliber-default *kp-hyphen-penalty*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-hyphen-penalty*)
     :end (caliber-max *kp-hyphen-penalty*)
     :slug-start (caliber-default *kp-hyphen-penalty*)
     :tick-frequency 0
     :callback 'set-kp-hyphen-penalty
     :reader kp-hyphen-penalty)
   (kp-explicit-hyphen-penalty slider
     :title (format nil "Explicit Hyphen Penalty: ~D"
	      (caliber-default *kp-explicit-hyphen-penalty*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-explicit-hyphen-penalty*)
     :end (caliber-max *kp-explicit-hyphen-penalty*)
     :slug-start (caliber-default *kp-explicit-hyphen-penalty*)
     :tick-frequency 0
     :callback 'set-kp-explicit-hyphen-penalty
     :reader kp-explicit-hyphen-penalty)
   (kp-adjacent-demerits slider
     :title (format nil "Adjacent Demerits: ~D"
	      (caliber-default *kp-adjacent-demerits*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-adjacent-demerits*)
     :end (caliber-max *kp-adjacent-demerits*)
     :slug-start (caliber-default *kp-adjacent-demerits*)
     :tick-frequency 0
     :callback 'set-kp-adjacent-demerits
     :reader kp-adjacent-demerits)
   (kp-double-hyphen-demerits slider
     :title (format nil "Double Hyphen Demerits: ~D"
	      (caliber-default *kp-double-hyphen-demerits*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-double-hyphen-demerits*)
     :end (caliber-max *kp-double-hyphen-demerits*)
     :slug-start (caliber-default *kp-double-hyphen-demerits*)
     :tick-frequency 0
     :callback 'set-kp-double-hyphen-demerits
     :reader kp-double-hyphen-demerits)
   (kp-final-hyphen-demerits slider
     :title (format nil "Final Hyphen Demerits: ~D"
	      (caliber-default *kp-final-hyphen-demerits*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-final-hyphen-demerits*)
     :end (caliber-max *kp-final-hyphen-demerits*)
     :slug-start (caliber-default *kp-final-hyphen-demerits*)
     :tick-frequency 0
     :callback 'set-kp-final-hyphen-demerits
     :reader kp-final-hyphen-demerits)
   (kp-pre-tolerance slider
     :title (format nil "Pre Tolerance: ~D"
	      (caliber-default *kp-pre-tolerance*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-pre-tolerance*)
     :end (caliber-max *kp-pre-tolerance*)
     :slug-start (caliber-default *kp-pre-tolerance*)
     :tick-frequency 0
     :callback 'set-kp-pre-tolerance
     :reader kp-pre-tolerance)
   (kp-tolerance slider
     :title (format nil "Tolerance: ~D" (caliber-default *kp-tolerance*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-tolerance*)
     :end (caliber-max *kp-tolerance*)
     :slug-start (caliber-default *kp-tolerance*)
     :tick-frequency 0
     :callback 'set-kp-tolerance
     :reader kp-tolerance)
   (kp-emergency-stretch slider
     :title (format nil "Emergency Stretch: ~D"
	      (caliber-default *kp-emergency-stretch*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-emergency-stretch*)
     :end (caliber-max *kp-emergency-stretch*)
     :slug-start (caliber-default *kp-emergency-stretch*)
     :tick-frequency 0
     :callback 'set-kp-emergency-stretch
     :reader kp-emergency-stretch)
   (kp-looseness slider
     :title (format nil "Looseness: ~D"
	      (caliber-default *kp-looseness*))
     :orientation :horizontal
     :visible-min-width 220
     :start (caliber-min *kp-looseness*)
     :end (caliber-max *kp-looseness*)
     :slug-start (caliber-default *kp-looseness*)
     :tick-frequency 0
     :callback 'set-kp-looseness
     :reader kp-looseness)
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
     :items *lineup-features*
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
     :visible-max-height nil
     :items '(:characters :hyphenation-points
	      :paragraph-box :line-boxes :character-boxes :baselines
	      :over/underfull-boxes :overshrunk/stretched-boxes)
     :selected-items '(:characters)
     :print-function 'title-capitalize
     :selection-callback 'set-clues
     :retract-callback 'set-clues
     :reader clues)
   (text editor-pane
     :title "Source text" :title-position :frame
     :visible-min-width '(character 80)
     ;;:visible-max-width '(character 80)
     :visible-min-height '(character 10)
     :visible-max-height '(character 30)
     :change-callback 'set-text
     :reader text)
   (view output-pane
     :title "Typeset paragraph" :title-position :frame
     :font (gp:make-font-description :family "Latin Modern Roman"
	     :weight :normal :slant :roman :size 10)
     :visible-min-height 300
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'render-paragraph
     :reader view
	 ;;:input-model '(((:button-1 :press) display-properties))))
     :input-model '((:motion display-properties))))
  (:layouts
   (main column-layout '(settings view))
   (settings row-layout '(settings-1 settings-2))
   (settings-1 column-layout '(options paragraph-width zoom)
     :reader settings-1)
   (options row-layout '(options-1 options-2))
   (options-1 column-layout '(disposition disposition-options features))
   (options-2 column-layout '(clues))
   (settings-2 column-layout '(algorithms text)
     :reader settings-2)
   (fixed-settings row-layout '(fixed-fallback fixed-options fixed-parameters))
   (fixed-parameters column-layout
     '(fixed-width-offset)
     :title "Other Parameters"
     :title-position :frame
     :visible-max-height nil)
   (fit-settings row-layout
		 '(fit-variant fit-fallback fit-options fit-parameters))
   (fit-parameters column-layout
     '(fit-discriminating-function
       fit-hyphen-penalty fit-explicit-hyphen-penalty fit-width-offset)
     :title "Other Parameters"
     :title-position :frame
     :visible-max-height nil)
   (barnett-settings row-layout '())
   (duncan-settings row-layout '(duncan-discriminating-function))
   (kp-settings row-layout '(kp-variant kp-sliders))
   (kp-sliders grid-layout
     '(kp-line-penalty kp-hyphen-penalty kp-explicit-hyphen-penalty
       kp-adjacent-demerits kp-double-hyphen-demerits kp-final-hyphen-demerits
       kp-pre-tolerance kp-tolerance kp-emergency-stretch kp-looseness)
     :orientation :horizontal
     :columns 3))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))


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
	 (set-sliders fit :hyphen-penalty :explicit-hyphen-penalty)
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
	   :pre-tolerance :tolerance :emergency-stretch :looseness)))))
  (setf (choice-selected-item (disposition etap))
	(disposition-type (disposition context)))
  (set-choice-selection (disposition-options-panel etap)
			(disposition-options (disposition context))
			*disposition-options*)
  (set-choice-selection (features etap) (features context) *lineup-features*)
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

(defun run (&optional (context *context*))
  "Run ETAP's GUI for CONTEXT (the global context by default)."
  (display (make-instance 'etap :context context :help-callback 'show-help)))
