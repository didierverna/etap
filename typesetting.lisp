(in-package :etap)

(defclass pinned ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))


(defclass pinned-character (pinned)
  ((character-metrics
    :initarg :character-metrics :accessor character-metrics)))

(defun pinned-character-p (object)
  (typep object 'pinned-character))

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


(defclass pinned-hyphenation-clue (pinned)
  ())

(defun pinned-hyphenation-clue-p (object)
  (typep object 'pinned-hyphenation-clue))

(defmethod width ((pinned-hyphenation-clue pinned-hyphenation-clue))
  0)

(defmethod height ((pinned-hyphenation-clue pinned-hyphenation-clue))
  0)

(defmethod depth ((pinned-hyphenation-clue pinned-hyphenation-clue))
  0)

(defun make-pinned-hyphenation-clue (&rest initargs &key x y)
  (declare (ignore x y))
  (apply #'make-instance 'pinned-hyphenation-clue initargs))


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


(defun create-line (lineup start end &optional (scale 0))
  (unless end (setq end (length lineup)))
  (make-line (loop :with x := 0
		   :for element :in (flatten-lineup lineup start end)
		   :if (eq element :hyphenation-clue)
		     :collect (make-pinned-hyphenation-clue :x x)
		   :else :if (typep element 'tfm:character-metrics)
		     :collect (make-pinned-character element :x x)
		     :and :do (incf x (width element))
		   :else :if (kernp element)
		     :do (incf x (width element))
		   :else :if (gluep element)
		     :do (incf x (width element))
		     :and :unless (zerop scale)
			    :do (incf x (if (> scale 0)
					  (* scale (stretch element))
					  (* scale (shrink element)))))))

(defun create-justified-line
    (lineup start stop width sloppy
     &aux (scale (lineup-scale lineup start stop width)))
  (if scale
    (create-line lineup start stop
		 (cond (sloppy scale)
		       ((zerop scale) 0)
		       ((< scale 0) (max scale -1))
		       ((> scale 0) (min scale 1))))
    (create-line lineup start stop)))
