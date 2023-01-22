(in-package :etap)

(defclass pinned ()
  ((x :initform 0 :initarg :x :accessor x
      :documentation "The object's X coordinate.")
   (y :initform 0 :initarg :y :accessor y
      :documentation "The object's Y coordinate."))
  (:documentation "The PINNED class.
This is the base class for all pinned objects, that is, objects which have a
fixed position in 2D space (relative to some origin)."))
