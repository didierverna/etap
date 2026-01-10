(in-package :etap)
(in-readtable :etap)

(defclass pin ()
  ((object
    :documentation "The pinned object."
    :initarg :object :reader object)
   (board
    :documentation "The pin's board."
    :initarg :board :reader board)
   (x
    :documentation "The pin's X coordinate, relative to its board."
    :initform 0 :initarg :x :reader x)
   (y
    :documentation "The pin's Y coordinate, relative to its board."
    :initform 0 :initarg :y :reader y))
  (:documentation "The PIN class.
This class is used to pin objects at some position on a board."))

(defun make-pin (object board &rest keys &key x y)
  "Make a new pin for OBJECT at position (X, Y) on BOARD.
The default position is BOARD's origin."
  (declare (ignore x y))
  (apply #'make-instance 'pin :object object :board board keys))

(defmethod properties strnlcat ((pin pin) &key)
  "Advertise PINned object's properties."
  (properties (object pin)))

(defmethod width ((pin pin))
  "Return PINned object's width."
  (width (object pin)))

(defmethod height ((pin pin))
  "Return PINned object's height."
  (height (object pin)))

(defmethod depth ((pin pin))
  "Return PINned object's depth."
  (depth (object pin)))
