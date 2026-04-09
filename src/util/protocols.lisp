(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Geometry
;; ==========================================================================

(defgeneric width  (object) (:documentation "Return OBJECT's width."))
(defgeneric height (object) (:documentation "Return OBJECT's height."))
(defgeneric depth  (object) (:documentation "Return OBJECT's depth."))




;; ==========================================================================
;; Properties
;; ==========================================================================

(defgeneric properties (object &key)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))
