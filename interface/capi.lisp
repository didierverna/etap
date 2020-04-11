(in-package :etap)

(defun title-capitalize (title)
  (nsubstitute #\Space #\- (string-capitalize title)))


(defmacro define-slider-callback
    (name &aux (name (string name))
	       (dash-position (position #\- name))
	       (prefix (subseq name 0 dash-position))
	       (generic (subseq name (1+ dash-position))))
    `(defun ,(intern (concatenate 'string "SET-" name)) (pane value status)
       (declare (ignore status))
       (setf (titled-object-title pane)
	     (format nil
		 ,(concatenate 'string (title-capitalize generic) ": ~D")
	       value))
       (,(intern (concatenate 'string "SET-" prefix "-ALGORITHM"))
	nil (top-level-interface pane))))

(defmacro define-slider-callbacks (&rest names)
  `(progn ,@(mapcar (lambda (name) (list 'define-slider-callback name))
	      names)))


(defun update (interface &aux (context (context interface)))
  (setf (paragraph interface) (create-paragraph context))
  (gp:invalidate-rectangle (view interface)))


(defun set-fixed-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (context interface))
	`(:fixed
	  :variant ,(choice-selected-item (fixed-variant interface))
	  ,@(apply #'append
	      (choice-selected-items (fixed-options interface)))))
  (update interface))


(defun set-fit-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (context interface))
	`(:fit
	  :variant ,(choice-selected-item (fit-variant interface))
	  :discriminating-function
	  ,(choice-selected-item (fit-discriminating-function interface))
	  :hyphen-penalty ,(range-slug-start (fit-hyphen-penalty interface))
	  ,@(apply #'append (choice-selected-items (fit-options interface)))))
  (update interface))

(define-slider-callback fit-hyphen-penalty)


(defun set-barnett-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (context interface))
	`(:barnett
	  #+(),@(apply #'append
	      (choice-selected-items (barnett-options interface)))))
  (update interface))

(defun set-duncan-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (context interface))
	`(:duncan
	  :discriminating-function
	  ,(choice-selected-item (duncan-discriminating-function interface))
	  #+(),@(apply #'append
	      (choice-selected-items (duncan-options interface)))))
  (update interface))

(defun set-kp-algorithm (value interface)
  (declare (ignore value))
  (setf (algorithm (context interface))
	`(:knuth-plass
	  :variant ,(choice-selected-item (kp-variant interface))
	  :line-penalty ,(range-slug-start (kp-line-penalty interface))
	  :hyphen-penalty ,(range-slug-start (kp-hyphen-penalty interface))
	  :explicit-hyphen-penalty
	  ,(range-slug-start (kp-explicit-hyphen-penalty interface))
	  :adjacent-demerits
	  ,(range-slug-start (kp-adjacent-demerits interface))
	  :double-hyphen-demerits
	  ,(range-slug-start (kp-double-hyphen-demerits interface))
	  :final-hyphen-demerits
	  ,(range-slug-start (kp-final-hyphen-demerits interface))
	  :pre-tolerance ,(range-slug-start (kp-pre-tolerance interface))
	  :tolerance ,(range-slug-start (kp-tolerance interface))
	  :emergency-stretch
	  ,(range-slug-start (kp-emergency-stretch interface))
	  :looseness ,(range-slug-start (kp-looseness interface))))
  (update interface))

(define-slider-callbacks kp-line-penalty
  kp-hyphen-penalty kp-explicit-hyphen-penalty
  kp-adjacent-demerits kp-double-hyphen-demerits kp-final-hyphen-demerits
  kp-pre-tolerance kp-tolerance
  kp-emergency-stretch
  kp-looseness)


(defun set-algorithm (value interface)
  (case (car value)
    (:fixed (set-fixed-algorithm value interface))
    (:fit (set-fit-algorithm value interface))
    (:barnett (set-barnett-algorithm value interface))
    (:duncan (set-duncan-algorithm value interface))
    (:knuth-plass (set-kp-algorithm value interface))))


(defun set-disposition (value interface)
  (declare (ignore value))
  (setf (disposition (context interface))
	`(,(choice-selected-item (disposition interface))
	  ,@(apply #'append
	      (choice-selected-items (disposition-options-panel interface)))))
  (update interface))


(defun set-features (value interface)
  (declare (ignore value))
  (setf (features (context interface))
	(apply #'append (choice-selected-items (features interface))))
  (update interface))


(defun set-text (pane point old-length new-length
		 &aux (interface (top-level-interface pane)))
  (declare (ignore point old-length new-length))
  (setf (text (context interface)) (editor-pane-text pane))
  (update interface))


(defun set-paragraph-width
    (pane value status &aux (interface (top-level-interface pane)))
  (declare (ignore status))
  (setf (titled-object-title pane)
	(format nil "Paragraph width: ~Dpt (~,2Fcm)"
	  value (/ value 28.452755)))
  (setf (paragraph-width (context interface)) value)
  (update interface))


(defun set-zoom (pane value status)
  (declare (ignore status))
  (setf (titled-object-title pane) (format nil "Paragraph zoom: ~D%" value))
  (gp:invalidate-rectangle (view (top-level-interface pane))))


(defun set-clues (value interface)
  (declare (ignore value))
  (gp:invalidate-rectangle (view interface)))


(defun render-paragraph
    (pane x y width height
     &aux (interface (top-level-interface pane))
	  (context (context interface))
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
	      :else :if (and (eq (disposition-type (disposition context))
				 :justified)
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


(define-constant +tooltips+
    `(,@+fixed-tooltips+ ,@+fit-tooltips+ ,@+kp-tooltips+
      ,@+disposition-options-tooltips+))

(defun show-help (interface pane type key)
  (declare (ignore interface pane))
  (case type
    (:tooltip
     (typecase key
       (symbol (cadr (member key +tooltips+)))))))


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
   (fixed-variant radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Variant" :title-position :frame
     :items +fixed-variants+
     :help-keys +fixed-variants-help-keys+
     :print-function 'title-capitalize
     :selection-callback 'set-fixed-algorithm
     :reader fixed-variant)
   (fixed-options check-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Options" :title-position :frame
     :items +fixed-options+
     :help-keys +fixed-options-help-keys+
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-fixed-algorithm
     :retract-callback 'set-fixed-algorithm
     :reader fixed-options)
   (fit-variant radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Variant" :title-position :frame
     :items +fit-variants+
     :help-keys +fit-variants-help-keys+
     :print-function 'title-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-variant)
   (fit-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items +fit-options+
     :help-keys +fit-options-help-keys+
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-fit-algorithm
     :retract-callback 'set-fit-algorithm
     :reader fit-options)
   (fit-discriminating-function option-pane
     :title "Discriminating Function:"
     :items +fit-discriminating-functions+
     :print-function 'title-capitalize
     :selection-callback 'set-fit-algorithm
     :reader fit-discriminating-function)
   (fit-hyphen-penalty slider
     :title (format nil "Hyphen Penalty: ~D" (cadr +fit-hyphen-penalty+))
     :orientation :horizontal
     :start (car +fit-hyphen-penalty+)
     :end (caddr +fit-hyphen-penalty+)
     :slug-start (cadr +fit-hyphen-penalty+)
     :tick-frequency 0
     :callback 'set-fit-hyphen-penalty
     :reader fit-hyphen-penalty)
   #+()(barnett-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '()
     :help-keys '()
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-barnett-algorithm
     :retract-callback 'set-barnett-algorithm
     :reader barnett-options)
   #+()(duncan-options check-button-panel
     :layout-class 'column-layout
     :title "Options" :title-position :frame
     :items '()
     :help-keys '()
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-duncan-algorithm
     :retract-callback 'set-duncan-algorithm
     :reader duncan-options)
   (duncan-discriminating-function option-pane
     :title "Discriminating Function:"
     :items +duncan-discriminating-functions+
     :print-function 'title-capitalize
     :selection-callback 'set-duncan-algorithm
     :reader duncan-discriminating-function)
   (kp-variant radio-button-panel
     :layout-class 'column-layout
     :visible-max-height nil
     :title "Variant" :title-position :frame
     :items +kp-variants+
     :help-keys +kp-variants-help-keys+
     :print-function 'title-capitalize
     :selection-callback 'set-kp-algorithm
     :reader kp-variant)
   (kp-line-penalty slider
     :title (format nil "Line Penalty: ~D" (cadr +kp-line-penalty+))
     :orientation :horizontal
     :start (car +kp-line-penalty+)
     :end (caddr +kp-line-penalty+)
     :slug-start (cadr +kp-line-penalty+)
     :tick-frequency 0
     :callback 'set-kp-line-penalty
     :reader kp-line-penalty)
   (kp-hyphen-penalty slider
     :title (format nil "Hyphen Penalty: ~D" (cadr +kp-hyphen-penalty+))
     :orientation :horizontal
     :start (car +kp-hyphen-penalty+)
     :end (caddr +kp-hyphen-penalty+)
     :slug-start (cadr +kp-hyphen-penalty+)
     :tick-frequency 0
     :callback 'set-kp-hyphen-penalty
     :reader kp-hyphen-penalty)
   (kp-explicit-hyphen-penalty slider
     :title (format nil "Explicit Hyphen Penalty: ~D"
	      (cadr +kp-explicit-hyphen-penalty+))
     :orientation :horizontal
     :start (car +kp-explicit-hyphen-penalty+)
     :end (caddr +kp-explicit-hyphen-penalty+)
     :slug-start (cadr +kp-explicit-hyphen-penalty+)
     :tick-frequency 0
     :callback 'set-kp-explicit-hyphen-penalty
     :reader kp-explicit-hyphen-penalty)
   (kp-adjacent-demerits slider
     :title (format nil "Adjacent Demerits: ~D" (cadr +kp-adjacent-demerits+))
     :orientation :horizontal
     :start (car +kp-adjacent-demerits+)
     :end (caddr +kp-adjacent-demerits+)
     :slug-start (cadr +kp-adjacent-demerits+)
     :tick-frequency 0
     :callback 'set-kp-adjacent-demerits
     :reader kp-adjacent-demerits)
   (kp-double-hyphen-demerits slider
     :title (format nil "Double Hyphen Demerits: ~D"
	      (cadr +kp-double-hyphen-demerits+))
     :orientation :horizontal
     :start (car +kp-double-hyphen-demerits+)
     :end (caddr +kp-double-hyphen-demerits+)
     :slug-start (cadr +kp-double-hyphen-demerits+)
     :tick-frequency 0
     :callback 'set-kp-double-hyphen-demerits
     :reader kp-double-hyphen-demerits)
   (kp-final-hyphen-demerits slider
     :title (format nil "Final Hyphen Demerits: ~D"
	      (cadr +kp-final-hyphen-demerits+))
     :orientation :horizontal
     :start (car +kp-final-hyphen-demerits+)
     :end (caddr +kp-final-hyphen-demerits+)
     :slug-start (cadr +kp-final-hyphen-demerits+)
     :tick-frequency 0
     :callback 'set-kp-final-hyphen-demerits
     :reader kp-final-hyphen-demerits)
   (kp-pre-tolerance slider
     :title (format nil "Pre Tolerance: ~D" (cadr +kp-pre-tolerance+))
     :orientation :horizontal
     :start (car +kp-pre-tolerance+)
     :end (caddr +kp-pre-tolerance+)
     :slug-start (cadr +kp-pre-tolerance+)
     :tick-frequency 0
     :callback 'set-kp-pre-tolerance
     :reader kp-pre-tolerance)
   (kp-tolerance slider
     :title (format nil "Tolerance: ~D" (cadr +kp-tolerance+))
     :orientation :horizontal
     :start (car +kp-tolerance+)
     :end (caddr +kp-tolerance+)
     :slug-start (cadr +kp-tolerance+)
     :tick-frequency 0
     :callback 'set-kp-tolerance
     :reader kp-tolerance)
   (kp-emergency-stretch slider
     :title (format nil "Emergency Stretch: ~D" (cadr +kp-emergency-stretch+))
     :orientation :horizontal
     :start (car +kp-emergency-stretch+)
     :end (caddr +kp-emergency-stretch+)
     :slug-start (cadr +kp-emergency-stretch+)
     :tick-frequency 0
     :callback 'set-kp-emergency-stretch
     :reader kp-emergency-stretch)
   (kp-looseness slider
     :title (format nil "Looseness: ~D" (cadr +kp-looseness+))
     :orientation :horizontal
     :start (car +kp-looseness+)
     :end (caddr +kp-looseness+)
     :slug-start (cadr +kp-looseness+)
     :tick-frequency 0
     :callback 'set-kp-looseness
     :reader kp-looseness)
   (disposition radio-button-panel
     :layout-class 'column-layout
     :title "Disposition" :title-position :frame
     :visible-max-width nil
     :items +dispositions+
     :print-function 'title-capitalize
     :selection-callback 'set-disposition
     :reader disposition)
   (disposition-options check-button-panel
     :layout-class 'column-layout
     :title "Disposition Options" :title-position :frame
     :visible-max-width nil
     :items +disposition-options+
     :help-keys +disposition-options-help-keys+
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-disposition
     :retract-callback 'set-disposition
     :reader disposition-options-panel)
   (features check-button-panel
     :layout-class 'column-layout
     :title "Features" :title-position :frame
     :visible-max-width nil
     :visible-max-height nil
     :items +features+
     :print-function (lambda (item) (title-capitalize (car item)))
     :selection-callback 'set-features
     :retract-callback 'set-features
     :reader features)
   (paragraph-width slider
     :title "Paragraph width: XXXpt (XXcm)"
     :orientation :horizontal
     :start +paragraph-min-width+
     :end +paragraph-max-width+
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
	      :over/underfull-boxes)
     :selected-items '(:characters)
     :print-function 'title-capitalize
     :selection-callback 'set-clues
     :retract-callback 'set-clues
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
     :visible-min-height 350
     :horizontal-scroll t
     :vertical-scroll t
     :display-callback 'render-paragraph
     :reader view))
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
   (fixed-settings row-layout '(fixed-variant fixed-options))
   (fit-settings row-layout '(fit-variant fit-options fit-parameters))
   (fit-parameters column-layout
     '(fit-discriminating-function fit-hyphen-penalty)
     :title "Other Parameters"
     :title-position :frame
     :visible-max-height nil)
   (barnett-settings row-layout '(#+()barnett-options))
   (duncan-settings row-layout
     '(#+()duncan-options duncan-discriminating-function))
   (kp-settings row-layout '(kp-variant kp-sliders))
   (kp-sliders grid-layout
     '(kp-line-penalty kp-hyphen-penalty kp-explicit-hyphen-penalty
       kp-adjacent-demerits kp-double-hyphen-demerits kp-final-hyphen-demerits
       kp-pre-tolerance kp-tolerance kp-emergency-stretch kp-looseness)
     :orientation :horizontal
     :columns 3))
  (:default-initargs :title "Experimental Typesetting Algorithms Platform"))

(defmethod interface-display :before
    ((etap etap) &aux (context (context etap)))
  (let ((algorithm (algorithm-type (algorithm context)))
	(options (algorithm-options (algorithm context))))
    (case algorithm
      (:fixed
       (setf (choice-selection (algorithms etap)) 0)
       (setf (choice-selected-item (fixed-variant etap))
	     (or (cadr (member :variant options)) (car +fixed-variants+)))
       (setf (choice-selection (fixed-options etap))
	     (loop :for option :in +fixed-options+
		   :for i :from 0
		   :when (cadr (member (car option) options))
		     :collect i)))
      (:fit
       (setf (choice-selection (algorithms etap)) 1)
       (setf (choice-selected-item (fit-variant etap))
	     (or (cadr (member :variant options)) (car +fit-variants+)))
       (setf (choice-selected-item (fit-discriminating-function etap))
	     (or (cadr (member :discriminating-function options))
		 (car +fit-discriminating-functions+)))
       (setf (choice-selection (fit-options etap))
	     (loop :for option :in +fit-options+
		   :for i :from 0
		   :when (cadr (member (car option) options))
		     :collect i))
       (setf (range-slug-start (fit-hyphen-penalty etap))
	     (or (cadr (member :hyphen-penalty options))
		 (cadr +fit-hyphen-penalty+)))
       (setf (titled-object-title (fit-hyphen-penalty etap))
	     (format nil "Hyphen Penalty: ~D"
	       (range-slug-start (fit-hyphen-penalty etap)))))
      (:barnett
       (setf (choice-selection (algorithms etap)) 2)
       #+()(setf (choice-selection (barnett-options etap))
		 (let ((selection (list)))
		   selection)))
      (:duncan
       (setf (choice-selection (algorithms etap)) 3)
       #+()(setf (choice-selection (duncan-options etap))
		 (let ((selection (list)))
		   selection))
       (setf (choice-selected-item (duncan-discriminating-function etap))
	     (or (cadr (member :discriminating-function options))
		 (car +duncan-discriminating-functions+))))
      (:knuth-plass
       (setf (choice-selection (algorithms etap)) 4)
       (setf (choice-selected-item (kp-variant etap))
	     (or (cadr (member :variant options)) (car +kp-variants+)))
       (setf (range-slug-start (kp-line-penalty etap))
	     (or (cadr (member :line-penalty options))
		 (cadr +kp-line-penalty+)))
       (setf (titled-object-title (kp-line-penalty etap))
	     (format nil "Line Penalty: ~D"
	       (range-slug-start (kp-line-penalty etap))))
       (setf (range-slug-start (kp-hyphen-penalty etap))
	     (or (cadr (member :hyphen-penalty options))
		 (cadr +kp-hyphen-penalty+)))
       (setf (titled-object-title (kp-hyphen-penalty etap))
	     (format nil "Hyphen Penalty: ~D"
	       (range-slug-start (kp-hyphen-penalty etap))))
       (setf (range-slug-start (kp-explicit-hyphen-penalty etap))
	     (or (cadr (member :explicit-hyphen-penalty options))
		 (cadr +kp-explicit-hyphen-penalty+)))
       (setf (titled-object-title (kp-explicit-hyphen-penalty etap))
	     (format nil "Explicit Hyphen Penalty: ~D"
	       (range-slug-start (kp-explicit-hyphen-penalty etap))))
       (setf (range-slug-start (kp-adjacent-demerits etap))
	     (or (cadr (member :adjacent-demerits options))
		 (cadr +kp-adjacent-demerits+)))
       (setf (titled-object-title (kp-adjacent-demerits etap))
	     (format nil "Adjacent Demerits: ~D"
	       (range-slug-start (kp-adjacent-demerits etap))))
       (setf (range-slug-start (kp-double-hyphen-demerits etap))
	     (or (cadr (member :double-hyphen-demerits options))
		 (cadr +kp-double-hyphen-demerits+)))
       (setf (titled-object-title (kp-double-hyphen-demerits etap))
	     (format nil "Double Hyphen Demerits: ~D"
	       (range-slug-start (kp-double-hyphen-demerits etap))))
       (setf (range-slug-start (kp-final-hyphen-demerits etap))
	     (or (cadr (member :final-hyphen-demerits options))
		 (cadr +kp-final-hyphen-demerits+)))
       (setf (titled-object-title (kp-final-hyphen-demerits etap))
	     (format nil "Final Hyphen Demerits: ~D"
	       (range-slug-start (kp-final-hyphen-demerits etap))))
       (setf (range-slug-start (kp-pre-tolerance etap))
	     (or (cadr (member :pre-tolerance options))
		 (cadr +kp-pre-tolerance+)))
       (setf (titled-object-title (kp-pre-tolerance etap))
	     (format nil "Pre Tolerance: ~D"
	       (range-slug-start (kp-pre-tolerance etap))))
       (setf (range-slug-start (kp-tolerance etap))
	     (or (cadr (member :tolerance options))
		 (cadr +kp-tolerance+)))
       (setf (titled-object-title (kp-tolerance etap))
	     (format nil "Tolerance: ~D"
	       (range-slug-start (kp-tolerance etap))))
       (setf (range-slug-start (kp-emergency-stretch etap))
	     (or (cadr (member :emergency-stretch options))
		 (cadr +kp-emergency-stretch+)))
       (setf (titled-object-title (kp-emergency-stretch etap))
	     (format nil "Emergency Stretch: ~D"
	       (range-slug-start (kp-emergency-stretch etap))))
       (setf (range-slug-start (kp-looseness etap))
	     (or (cadr (member :looseness options))
		 (cadr +kp-looseness+)))
       (setf (titled-object-title (kp-looseness etap))
	     (format nil "Looseness: ~D"
	       (range-slug-start (kp-looseness etap)))))))
  (setf (choice-selected-item (disposition etap))
	(disposition-type (disposition context)))
  (let ((options (disposition-options (disposition context))))
    (setf (choice-selection (disposition-options-panel etap))
	  (loop :for option :in +disposition-options+
		:for i :from 0
		:when (cadr (member (car option) options))
		  :collect i)))
  (let ((features (features context)))
    (setf (choice-selection (features etap))
	  (loop :for feature :in +features+
		:for i :from 0
		:when (cadr (member (car feature) features))
		  :collect i)))
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
  (display (make-instance 'etap :context context :help-callback 'show-help)))
