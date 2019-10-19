(in-package :etap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (net.didierverna.tfm:nickname-package))

(defconstant +initial-text+
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defconstant +font-file+
  (asdf:system-relative-pathname :etap #p"share/ec-lmr10.tfm"))

(defclass state ()
  ((font :initform (tfm:load-font +font-file+) :reader font)
   (glue :reader glue)
   (algorithm :initform :fixed :accessor algorithm)
   (disposition :initform :flush-left :accessor disposition)
   (features :initform (list) :accessor features)
   ;; 284.52756pt = 10cm
   (paragraph-width :initform 284 :accessor paragraph-width)
   (text :initform +initial-text+ :accessor text)
   (paragraph :accessor paragraph)))

(defclass kern () ((value :initarg :value :reader value)))
(defclass glue ()
  ((value :initarg :value :reader value)
   (stretch :initarg :stretch :reader stretch)
   (shrink :initarg :shrink :reader shrink)))

(defmethod initialize-instance :after
    ((state state) &key &aux (font (font state))
			     (design-size (tfm:design-size font)))
  (setf (slot-value state 'glue)
	(make-instance 'glue
	  :value (* (tfm:interword-space font) design-size)
	  :stretch (* (tfm:interword-stretch font) design-size)
	  :shrink (* (tfm:interword-shrink font) design-size))))


(defconstant +blanks+ '(#\Space #\Tab #\Newline))

(defun blankp (character) (member character +blanks+))

(defun lineup (state &aux  lineup)
  (setq lineup (loop :with font := (font state)
		     :with glue := (glue state)
		     :with text := (string-trim +blanks+ (text state))
		     :with length := (length text)
		     :with i := 0

		     :while (< i length)
		     :for character := (tfm:get-character
					(char-code (aref text i)) font)

		     :if (blankp (aref text i))
		       :collect glue
		       :and :do (setq i (position-if-not #'blankp text
							 :start i))
		     :else :if character
			     :collect character
			     :and :do (incf i)
		     :else
		       :do (incf i)))
  (when (member :ligatures (features state))
    (setq lineup
	  (loop :with elements := lineup
		:while elements
		:for elt1 := (car elements)
		:for elt2 := (cadr elements)
		:for lig := (when (and (typep elt1 'tfm::character-metrics)
				       (typep elt2 'tfm::character-metrics))
			      (tfm:ligature elt1 elt2))
		:if lig
		  :do (let ((composition (list)))
			(unless (tfm:delete-after lig)
			  (push elt2 composition))
			(push (tfm:composite lig) composition)
			(unless (tfm:delete-before lig)
			  (push elt1 composition))
			(setq elements (append composition (cddr elements))))
		  :and :unless (zerop (tfm:pass-over lig))
			 :append (subseq elements 0 (tfm:pass-over lig))
			 :and :do (setq elements
					(nthcdr (tfm:pass-over lig) elements))
		       :end
		:else
		  :collect elt1
		  :and :do (setq elements (cdr elements)))))
  (when (member :kerning (features state))
    (setq lineup
	  (loop :for elements :on lineup
		:for elt1 := (car elements)
		:for elt2 := (cadr elements)
		:for kern := (when (and (typep elt1 'tfm::character-metrics)
					(typep elt2 'tfm::character-metrics))
			       (tfm:kerning elt1 elt2))
		:collect elt1
		:when kern
		  :collect (make-instance 'kern
			     :value (* (tfm:design-size (tfm:font elt1))
				       kern)))))
  lineup)


(defclass pinned ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defclass pinned-character (pinned)
  ((character-metrics
    :initform nil :initarg :character-metrics :accessor character-metrics)))

(defmethod width ((pinned-character pinned-character))
  (with-slots ((width tfm:width) (font tfm:font))
      (character-metrics pinned-character)
    (* (tfm:design-size font) width)))

(defmethod height ((pinned-character pinned-character))
  (with-slots ((height tfm:height) (font tfm:font))
      (character-metrics pinned-character)
    (* (tfm:design-size font) height)))

(defmethod depth ((pinned-character pinned-character))
  (with-slots ((depth tfm:depth) (font tfm:font))
      (character-metrics pinned-character)
    (* (tfm:design-size font) depth)))

(defclass line ()
  ((width :initform 0 :initarg :width :accessor width)
   (height :initform 0 :initarg :height :accessor height)
   (depth :initform 0 :initarg :depth :accessor depth)
   (pinned-characters
    :initform nil :initarg :pinned-characters :accessor pinned-characters)))

(defclass pinned-line (pinned)
  ((line :initform nil :initarg :line :accessor line)))

(defmethod width ((pinned-line pinned-line))
  (width (line pinned-line)))

(defmethod height ((pinned-line pinned-line))
  (height (line pinned-line)))

(defmethod depth ((pinned-line pinned-line))
  (depth (line pinned-line)))


(defclass paragraph ()
  ((width :initform 0 :initarg :width :accessor width)
   (height :initform 0 :initarg :height :accessor height)
   (depth :initform 0 :initarg :depth :accessor depth)
   (pinned-lines :initform nil :initarg :pinned-lines :accessor pinned-lines)))

(defun lineup-widths (lineup &optional (start 0) end)
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for element :in (nthcdr start lineup)
	:repeat (- end start)
	:if (typep element 'tfm::character-metrics)
	  :do (incf width (* (tfm:design-size (tfm:font element))
			     (tfm:width element)))
	:else :if (typep element 'kern)
		:do (incf width (value element))
	:else :if (typep element 'glue)
		:do (incf width (value element))
		:and :do (incf stretch (stretch element))
		:and :do (incf shrink (shrink element))
	:finally (return (list width (+ width stretch) (- width shrink)))))

(defun next-glue-position (lineup &optional (start 0))
  (position-if (lambda (element) (typep element 'glue)) lineup :start start))

(defun collect-line
    (lineup state &aux (paragraph-width (paragraph-width state))
		       (algorithm (algorithm state)))
  (loop :with i := (next-glue-position lineup)
	:with ii := (when i (next-glue-position lineup (1+ i)))
	:with width := (let ((widths (lineup-widths lineup 0 i)))
			 (case algorithm
			   ((:fixed :best-fit) (car widths))
			   (:first-fit (cadr widths))
			   (:last-fit (caddr widths))))
	:while (and i (<= (+ width
			     (let ((widths (lineup-widths lineup i ii)))
			       (case algorithm
				 ((:fixed :best-fit) (car widths))
				 (:first-fit (cadr widths))
				 (:last-fit (caddr widths)))))
			  paragraph-width))
	:do (incf width (let ((widths (lineup-widths lineup i ii)))
			  (case algorithm
			    ((:fixed :best-fit) (car widths))
			    (:first-fit (cadr widths))
			    (:last-fit (caddr widths)))))
	:do (setq i ii ii (when i (next-glue-position lineup (1+ i))))
	:finally (return (if i
			   (values (subseq lineup 0 i) (subseq lineup (1+ i)))
			   lineup))))

(defun create-line (lineup state &aux (algorithm (algorithm state)) line)
  (multiple-value-bind (elements lineup-remainder) (collect-line lineup state)
    (setq line (make-instance 'line
		 :pinned-characters
		 (loop :with x := 0
		       :for element :in elements
		       :if (typep element 'tfm::character-metrics)
			 :collect (make-instance 'pinned-character
				    :x x :character-metrics element)
			 :and :do (incf x (* (tfm:width element)
					     (tfm:design-size
					      (tfm:font element))))
		       :else :if (typep element 'kern)
			       :do (incf x (value element))
		       :else :if (typep element 'glue)
			       :do (incf x (case algorithm
					     ((:fixed :best-fit)
					      (value element))
					     (:first-fit
					      (+ (value element)
						 (stretch element)))
					     (:last-fit
					      (- (value element)
						 (shrink element))))))))
    (loop :for pinned-character :in (pinned-characters line)
	  :maximize (height pinned-character) :into height
	  :maximize (depth pinned-character) :into depth
	  :finally (setf (height line) height (depth line) depth))
    (let ((last-pinned-character (car (last (pinned-characters line)))))
      (setf (width line)
	    (+ (x last-pinned-character) (width last-pinned-character))))
    (list line lineup-remainder)))

(defun create-paragraph
    (lineup state &aux (paragraph (make-instance 'paragraph
				    :width (paragraph-width state))))
  (when lineup
    (with-slots (width height depth pinned-lines) paragraph
      (setf pinned-lines
	    (loop :while lineup
		  :for y := 0 :then (+ y 12)
		  :for (line remainder) := (create-line lineup state)
		  :collect (make-instance 'pinned-line :y y :line line)
		  :do (setq lineup remainder)))
      (case (disposition state)
	(:flush-right
	 (dolist (pinned-line pinned-lines)
	   (setf (x pinned-line) (- width (width pinned-line)))))
	(:centered
	 (dolist (pinned-line pinned-lines)
	   (setf (x pinned-line) (/ (- width (width pinned-line)) 2)))))
      (setf height (height (first pinned-lines))
	    depth (+ (depth (car (last pinned-lines)))
		     (* (1- (length pinned-lines)) 12)))))
  paragraph)

(defun update-paragraph (state)
  (setf (paragraph state) (create-paragraph (lineup state) state)))
