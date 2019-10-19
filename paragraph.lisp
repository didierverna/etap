(in-package :etap)

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

(defmethod initialize-instance :after ((line line) &key)
  (loop :for pinned-character :in (pinned-characters line)
	:maximize (height pinned-character) :into height
	:maximize (depth pinned-character) :into depth
	:finally (setf (height line) height (depth line) depth))
  (let ((last-pinned-character (car (last (pinned-characters line)))))
    (setf (width line)
	  (+ (x last-pinned-character) (width last-pinned-character)))))


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

(defmethod initialize-instance :after ((paragraph paragraph) &key disposition)
  (with-slots (width height depth pinned-lines) paragraph
    (case disposition
      (:flush-right
       (dolist (pinned-line pinned-lines)
	 (setf (x pinned-line) (- width (width pinned-line)))))
      (:centered
       (dolist (pinned-line pinned-lines)
	 (setf (x pinned-line) (/ (- width (width pinned-line)) 2)))))
    (setf height (height (first pinned-lines))
	  depth (+ (depth (car (last pinned-lines)))
		   (* (1- (length pinned-lines)) 12)))))


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

(defun collect-elements (lineup paragraph-width algorithm)
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

(defun create-line (lineup width algorithm &aux line)
  (multiple-value-bind (elements remainder)
      (collect-elements lineup width algorithm)
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
    (list line remainder)))

(defun create-lines (lineup width algorithm)
  (loop :while lineup
	:for (line remainder) := (create-line lineup width algorithm)
	:collect line
	:do (setq lineup remainder)))

(defun create-paragraph
    (lineup width algorithm disposition
     &aux (lines (create-lines lineup width algorithm)))
  (make-instance 'paragraph
    :disposition disposition
    :width width
    :pinned-lines (loop :for line :in lines
			:for y := 0 :then (+ y 12)
			:collect (make-instance 'pinned-line
				   :y y :line line))))
