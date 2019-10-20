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


(defun lineup-width (lineup start end &optional (glue-length :natural))
  (setq glue-length (case glue-length
		      (:natural #'value)
		      (:max #'max-length)
		      (:min #'min-length)))
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:for i :from start
	:repeat (- end start)
	:for element := (aref lineup i)
	:if (typep element 'tfm::character-metrics)
	  :do (incf width (* (tfm:design-size (tfm:font element))
			     (tfm:width element)))
	:else :if (typep element 'kern)
		:do (incf width (value element))
	:else :if (typep element 'glue)
		:do (incf width (funcall glue-length element))
	:finally (return width)))

(defun next-glue-position (lineup &optional (start 0))
  (position-if (lambda (element) (typep element 'glue)) lineup :start start))

(defun line-end
    (start lineup width algorithm disposition
     &aux (glue-length (case algorithm
			 ((:fixed :best-fit) :natural)
			 (:first-fit :max)
			 (:last-fit :min))))
  (loop :for i := (next-glue-position lineup start) :then ii
	:for ii := (when i (next-glue-position lineup (1+ i)))
	:for w := (lineup-width lineup start i glue-length) :then (+ w ww)
	:for ww := (when i (lineup-width lineup i ii glue-length))
	:while (and i (<= (+ w ww) width))
	:finally (return i)))

(defun line-boundaries
    (lineup width algorithm disposition &aux (last (1- (length lineup))))
  (loop :for start := 0 :then (when end (1+ end))
	:while start
	:for end := (line-end start lineup width algorithm disposition)
	:collect (list start (if end (1- end) last))))

(defun create-line
    (lineup boundary width algorithm disposition
     &aux (glue-length (case algorithm
			 ((:fixed :best-fit) #'value)
			 (:first-fit #'max-length)
			 (:last-fit #'min-length))))
  (make-instance 'line
    :pinned-characters
    (loop :with x := 0
	  :for i :from (car boundary) :upto (cadr boundary)
	  :for element := (aref lineup i)
	  :if (typep element 'tfm::character-metrics)
	    :collect (make-instance 'pinned-character
		       :x x :character-metrics element)
	    :and :do (incf x (* (tfm:width element)
				(tfm:design-size (tfm:font element))))
	  :else :if (typep element 'kern)
		  :do (incf x (value element))
	  :else :if (typep element 'glue)
		  :do (incf x (funcall glue-length element)))))

(defun create-lines (lineup width algorithm disposition)
  (mapcar (lambda (boundary)
	    (create-line lineup boundary width algorithm disposition))
    (line-boundaries lineup width algorithm disposition)))

(defun create-paragraph
    (lineup width algorithm disposition
     &aux (lines (when lineup
		   (create-lines lineup width algorithm disposition))))
  (make-instance 'paragraph
    :disposition disposition
    :width width
    :pinned-lines
    (loop :for line :in lines
	  :for y := 0 :then (+ y 12)
	  :collect (make-instance 'pinned-line :y y :line line))))
