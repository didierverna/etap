(in-package :etap)

(defgeneric properties (object &key &allow-other-keys)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))


;; #### TODO: it's still not very clear to me whether we want all pinned
;; objects under the same class as it's currently done, or whether we should
;; have a pinned mixin and create subclasses for each kind of object.

(defclass pinned ()
  ((object
    :documentation "The pinned object."
    :initarg :object :reader object)
   (board
    :documentation "The pinned object's board."
    :initarg :board :reader board)
   (x
    :documentation "The pinned object's X coordinate, relative to its board."
    :initform 0 :initarg :x :reader x)
   (y
    :documentation "The pinned object's Y coordinate, relative to its board."
    :initform 0 :initarg :y :reader y))
  (:documentation "The PINNED class.
This class represents objects which are pinned to a fixed position in 2D
space, expressed relative to their board."))


(defun pin-object (object board x &optional (y 0))
  "Pin OBJECT on BOARD at position (X, Y)."
  (make-instance 'pinned :object object :board board :x x :y y))
