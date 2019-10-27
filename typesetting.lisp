(in-package :etap)

(defclass pinned ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))


(defclass pinned-character (pinned)
  ((character-metrics
    :initarg :character-metrics :accessor character-metrics)))

(defmethod width ((pinned-character pinned-character))
  (width (character-metrics pinned-character)))

(defmethod height ((pinned-character pinned-character))
  (height (character-metrics pinned-character)))

(defmethod depth ((pinned-character pinned-character))
  (depth (character-metrics pinned-character)))

(defun make-pinned-character (character-metrics &rest initargs &key x y)
  (declare (ignore x y))
  (apply #'make-instance 'pinned-character
    :character-metrics character-metrics initargs))



(defclass line ()
  ((pinned-characters
    :initarg :pinned-characters :accessor pinned-characters)))

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

(defun make-line (pinned-characters &rest initargs &key x y)
  (declare (ignore x y))
  (apply #'make-instance 'line :pinned-characters pinned-characters initargs))


(defun create-line (lineup start end &key (stretch 0) (shrink 0))
  (unless end (setq end (length lineup)))
  (make-line (loop :with x := 0
		   :for i :from start :upto (1- end)
		   :for element := (lineup-aref lineup i start end)
		   :if (typep element 'tfm::character-metrics)
		     :collect (make-pinned-character element :x x)
		     :and :do (incf x (width element))
		   :else :if (kernp element)
			   :do (incf x (width element))
		   :else :if (gluep element)
			   :do (incf x (+ (width element)
					  (* stretch (stretch element))
					  (- (* shrink (shrink element))))))))
