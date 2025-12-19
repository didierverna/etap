(in-package :etap)

(defgeneric properties (object &key)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))


;; #### NOTE: we currently provide two different ways for pinning objects.
;; - The fist one is to use the PINNED class to pin any object. It's useful
;;   when you want to reference a pinned object without actually touching it.
;;   This is what we do in lines, because line items are in fact harray items
;;   (except for whitespaces) and those items can potentially be shared
;;   across different lines.
;; - The second one is to subclass an original class, mixing it in with the
;;   PIN mixin. This comes in handy when no sharing is expected, and this is
;;   what we do with lines becoming pinned lines (by way of change-class) when
;;   a layout is rendered.
;; On the other hand, this might go away some day. Indeed, the distinction
;; between lines and pinned lines only comes from the fact that our algorithms
;; currently don't take the vertical spacing into account when they break the
;; harray. This will probably change in the future.

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

(defmethod properties strnlcat ((pinned pinned) &key)
  "Advertise PINNED object's properties."
  (properties (object pinned)))

(defun pin-object (object board x &optional (y 0))
  "Pin OBJECT on BOARD at position (X, Y)."
  (make-instance 'pinned :object object :board board :x x :y y))
