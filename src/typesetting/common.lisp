(in-package :etap)

(defclass pinned ()
  ((object
    :documentation "The pinned object."
    :initarg :object :reader object)
   (board
    :documentation "The pinned object's board."
    :initform nil :initarg :board :reader board)
   (x
    :documentation "The pinned object's X coordinate, relative to its board."
    :initform 0 :initarg :x :reader x)
   (y
    :documentation "The pinned object's Y coordinate, relative to its board."
    :initform 0 :initarg :y :reader y))
  (:documentation "The PINNED class.
This class represents objects which are pinned to a fixed position in 2D
space, expressed relative to their board."))
