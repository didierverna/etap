(in-package :etap)

(defclass pinned-line (pinned)
  ((line :initarg :line :accessor line)))

(defmethod width ((pinned-line pinned-line))
  (width (line pinned-line)))

(defmethod height ((pinned-line pinned-line))
  (height (line pinned-line)))

(defmethod depth ((pinned-line pinned-line))
  (depth (line pinned-line)))

(defun make-pinned-line (line &rest initargs &key x y)
  (declare (ignore x y))
  (apply #'make-instance 'pinned-line :line line initargs))

(defun create-pinned-lines (lineup width disposition algorithm)
  (loop :for line
	  :in (apply #'create-lines
		lineup width disposition (car algorithm) (cdr algorithm))
	:for x := (case (car disposition)
		    ((:flush-left :justified) 0)
		    (:centered (/ (- width (width line)) 2))
		    (:flush-right (- width (width line))))
	:for y := 0 :then (+ y 12)
	:collect (make-pinned-line line :x x :y y)))


(defclass paragraph ()
  ((width :initarg :width :accessor width)
   (pinned-lines :initform nil :initarg :pinned-lines :accessor pinned-lines)))

(defmethod height ((paragraph paragraph))
  (height (first (pinned-lines paragraph))))

(defmethod depth ((paragraph paragraph))
  (with-accessors ((pinned-lines pinned-lines)) paragraph
    (+ (* (1- (length pinned-lines)) 12)
       (depth (car (last pinned-lines))))))

(defun make-paragraph (width &rest initargs &key pinned-lines)
  (declare (ignore pinned-lines))
  (apply #'make-instance 'paragraph :width width initargs))

(defun create-paragraph (lineup width disposition algorithm)
  (make-paragraph width
   :pinned-lines (when lineup
		   (create-pinned-lines lineup width disposition algorithm))))
