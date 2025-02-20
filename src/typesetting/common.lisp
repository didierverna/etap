(in-package :etap)

(defgeneric properties (object &key &allow-other-keys)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))


(defclass pin ()
  ((board
    :documentation "The pin board."
    :initarg :board :reader board)
   (x
    :documentation "The pin's X coordinate, relative to its board."
    :initform 0 :initarg :x :reader x)
   (y
    :documentation "The pin's Y coordinate, relative to its board."
    :initform 0 :initarg :y :reader y))
  (:documentation "The PIN class.
This class can be used as a mixin for pinning objects to a fixed 2D position,
expressed relative to a pin board."))


(defclass pinned (pin)
  ((object
    :documentation "The pinned object."
    :initarg :object :reader object))
  (:documentation "The PINNED class.
This class is used to pin any kind of object to a board."))

(defun pin-object (object board x &optional (y 0))
  "Pin OBJECT on BOARD at position (X, Y)."
  (make-instance 'pinned :object object :board board :x x :y y))
