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
    ((state state)
     &key
     &aux (font (font state))
	  (design-size (tfm:design-size font)))
  (setf (slot-value state 'glue)
	(make-instance 'glue
	  :value (* (tfm:interword-space font) design-size)
	  :stretch (* (tfm:interword-stretch font) design-size)
	  :shrink (* (tfm:interword-shrink font) design-size))))


(defconstant +blanks+ '(#\Space #\Tab #\Newline))

(defun blankp (character) (member character +blanks+))

(defun characters-lineup
    (state &aux (design-size (tfm:design-size (font state))) lineup)
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
		  :collect (print elt1)
		  :and :do (setq elements (cdr elements)))))
  (when (member :kerning (features state))
    (setq lineup
	  (loop :for elements :on lineup
		:for elt1 := (print (car elements))
		:for elt2 := (print (cadr elements))
		:for kern := (when (and (typep elt1 'tfm::character-metrics)
					(typep elt2 'tfm::character-metrics))
			       (tfm:kerning elt1 elt2))
		:collect elt1
		:when kern :collect (make-instance 'kern
				       :value (* design-size kern)))))
  lineup)

;; #### NOTE: add y for raisebox.
(defclass line-character ()
  ((x :initarg :x :reader x)
   (character-metrics :initarg :character-metrics :reader character-metrics)))

;; #### FIXME: this should be split into line (without x and y) and
;; paragraph-line (with them).
(defclass paragraph-line ()
  ((x :accessor x)
   (y :accessor y)
   (width :accessor width)
   (height :accessor height)
   (depth :accessor depth)
   (characters :initarg :characters :reader characters)))

(defclass paragraph ()
  ((width :initarg :width :reader width)
   (height :accessor height)
   (lines :initarg :lines :reader lines)))

(defun next-glue-position (lineup &optional (start 0))
  (position-if (lambda (element) (typep element 'glue)) lineup :start start))

(defun lineup-width (lineup &optional (start 0) end)
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:for element :in (nthcdr start lineup)
	:repeat (- end start)
	:if (typep element 'tfm::character-metrics)
	  :do (incf width (* (tfm:design-size (tfm:font element))
			     (tfm:width element)))
	:else :if (typep element 'kern)
		:do (incf width (value element))
	:else :if (typep element 'glue)
		:do (incf width (value element))
	:finally (return width)))

(defun collect-line
    (lineup state &aux (paragraph-width (paragraph-width state)))
  (loop :with i := (next-glue-position lineup)
	:with ii := (when i (next-glue-position lineup (1+ i)))
	:with width := (lineup-width lineup 0 i)
	:while (and i (<= (+ width (lineup-width lineup i ii)) paragraph-width))
	:do (incf width (lineup-width lineup i ii))
	:do (setq i ii ii (when i (next-glue-position lineup (1+ i))))
	:finally (return (if i
			   (values (subseq lineup 0 i) (subseq lineup (1+ i)))
			   lineup))))

(defun render-line
    (lineup state &aux (design-size (tfm:design-size (font state))) line)
  (multiple-value-bind (elements lineup-remainder) (collect-line lineup state)
    (setq line (make-instance 'paragraph-line
		 :characters
		 (loop :with x := 0
		       :for element :in elements
		       :if (typep element 'tfm::character-metrics)
			 :collect (make-instance 'line-character
				   :x x :character-metrics element)
			 :and :do (incf x (* (tfm:width element) design-size))
		       :else :if (typep element 'kern)
			       :do (incf x (value element))
		       :else :if (typep element 'glue)
			       :do (incf x (value element)))))
    (loop :for line-character :in (characters line)
	  :maximize (tfm:height (character-metrics line-character))
	    :into height
	  :maximize (tfm:depth (character-metrics line-character))
	    :into depth
	  :finally (setf (height line) (* design-size height)
			 (depth line) (* design-size depth)))
    (let ((last-line-character (car (last (characters line)))))
      (setf (width line)
	    (+ (x last-line-character)
	       (* design-size
		  (tfm:width (character-metrics last-line-character))))))
    (list line lineup-remainder)))

(defun render-lineup
    (lineup state
     &aux (paragraph-width (paragraph-width state))
	  (paragraph
	   (make-instance 'paragraph
	     :width paragraph-width
	     :lines (loop :while lineup
			  :for (line lineup-remainder)
			    := (render-line lineup state)
			  :collect line
			  :do (setq lineup lineup-remainder)))))
  (case (disposition state)
    (:flush-left
     (loop :for y := 0 :then (+ y 12)
	   :for line :in (lines paragraph)
	   :do (setf (x line) 0
		     (y line) y)))
    (:flush-right
     (loop :for y := 0 :then (+ y 12)
	   :for line :in (lines paragraph)
	   :do (setf (x line) (- paragraph-width (width line))
		     (y line) y)))
    (:centered
     (loop :for y := 0 :then (+ y 12)
	   :for line :in (lines paragraph)
	   :do (setf (x line)(/ (abs (- paragraph-width (width line))) 2)
		     (y line) y))))
  (setf (height paragraph) (+ (height (first (lines paragraph)))
			      (depth (car (last (lines paragraph))))
			      (* (1- (length (lines paragraph))) 12)))
  paragraph)

(defun render (state)
  (setf (paragraph state) (render-lineup (characters-lineup state) state)))
