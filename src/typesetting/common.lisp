(in-package :etap)

(defclass pinned ()
  ((board :documentation "The object's board."
	  :initform nil :initarg :board :reader board)
   (x :documentation "The object's X coordinate, relative to its board."
      :initform 0 :initarg :x :accessor x)
   (y :documentation "The object's Y coordinate, relative to its board."
      :initform 0 :initarg :y :accessor y))
  (:documentation "The PINNED class.
This is the base class for all pinned objects, that is, objects which have a
fixed position in 2D space, expressed relative to their board."))
