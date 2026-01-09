(in-package :etap)
(in-readtable :etap)

;; ==========================================================================
;; Geometry
;; ==========================================================================

;; #### FIXME: we need to get rid of the default methods below. That kind of
;; #### implicit behavior is relied upon in various places, but it is evil.

(defgeneric width (object)
  (:documentation "Return OBJECT's width.")
  (:method (object)
    "Return 0 by default."
    0))

(defgeneric height (object)
  (:documentation "Return OBJECT's height.")
  (:method (object)
    "Return 0 by default."
    0))

(defgeneric depth (object)
  (:documentation "Return OBJECT's depth.")
  (:method (object)
    "Return 0 by default."
    0))




;; ==========================================================================
;; Properties
;; ==========================================================================

(defgeneric properties (object &key)
  (:documentation "Return a string advertising OBJECT's properties.
Methods may return an empty string or NIL if there is nothing to advertise.")
  (:method-combination strnlcat :most-specific-last))
