(in-package :etap)

(defclass pinned ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defclass pinned-character (pinned)
  ((character-metrics
    :initform nil :initarg :character-metrics :accessor character-metrics)))

(defun make-pinned-character (&rest initargs &key x y character-metrics)
  (declare (ignore x y character-metrics))
  (apply #'make-instance 'pinned-character initargs))

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
  ((pinned-characters
    :initform nil :initarg :pinned-characters :accessor pinned-characters)))

(defmethod width
    ((line line)
     &aux (last-pinned-character (car (last (pinned-characters line)))))
  (+ (x last-pinned-character) (width last-pinned-character)))

(defmethod height ((line line))
  (loop :for pinned-character :in (pinned-characters line)
	:maximize (height pinned-character)))

(defmethod depth ((line line))
  (loop :for pinned-character :in (pinned-characters line)
	:maximize (depth pinned-character)))

(defun make-line (&rest initargs &key pinned-characters)
  (declare (ignore pinned-characters))
  (apply #'make-instance 'line initargs))

(defun create-line (lineup start end &key (stretch 0) (shrink 0))
  (unless end (setq end (length lineup)))
  (make-line :pinned-characters
	     (loop :with x := 0
		   :for i :from start :upto (1- end)
		   :for element := (aref lineup i)
		   :if (typep element 'tfm::character-metrics)
		     :collect (make-pinned-character
			       :x x :character-metrics element)
		     :and :do (incf x (* (tfm:width element)
					 (tfm:design-size (tfm:font element))))
		   :else :if (kernp element)
			   :do (incf x (value element))
		   :else :if (gluep element)
			   :do (incf x (+ (value element)
					  (* stretch (stretch element))
					  (- (* shrink (shrink element))))))))
