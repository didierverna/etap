(in-package :etap)


;; ==========================================================================
;; Lineup Breaking
;; ==========================================================================

;; #### NOTE: each algorithm is responsible for instantiating the appropriate
;; breakup class, so we cannot short-circuit anything here in case of an empty
;; harray. ALGORITHM-TYPE is redundant with what's inside LINEUP already, but
;; it is used for generic dispatch (I'd need filtered dispatch here!).
;; Finally, this function is called with all the algorithm options (also
;; redundant with what's inside LINEUP, and also to make the dynamic dispatch
;; more convenient), without knowing in advance whether they're going to be
;; used or not, so we need to relax keyword argument checking.
(defgeneric break-lineup (lineup width algorithm-type &key &allow-other-keys)
  (:documentation
   "Break LINEUP for paragraph WIDTH with ALGORITHM-TYPE.
Return the resulting breakup."))




;; ==========================================================================
;; Breakups
;; ==========================================================================

(defclass breakup ()
  ((lineup
    :documentation "This breakup's original lineup."
    :initarg :lineup :reader lineup)
   (paragraph-width
    :documentation "The breakup's paragraph width."
    :initarg :paragraph-width :reader paragraph-width)
   (layouts
    :documentation "This breakup's layouts array, or NIL (the default).
A distinction is made between the case where there's nothing to typeset
(an empty harray) and where an algorithm couldn't find any solution (which
currently never happens because all algorithms turn to a fallback solution):
- when there's nothing to typeset, this slot remains NIL,
- when no solution could be found, an array of size 0 will be created."
    :initform nil :reader layouts))
  (:documentation "The BREAKUP class.
A breakup is the result of breaking a lineup for a specific paragraph width.
Algorithms may provide their own breakup subclass in order to store specific
global properties."))


;; Pseudo-accessors

(defmethod harray ((breakup breakup))
  "Return BREAKUP's lineup harray."
  (harray (lineup breakup)))

(defmethod disposition ((breakup breakup))
  "Return BREAKUP's lineup disposition."
  (disposition (lineup breakup)))




;; ---------
;; Rendering
;; ---------

;; #### NOTE: the call to LENGTH below will return 0 when the LAYOUTS slot is
;; nil, as well as when it's an array of size 0.
(defun layouts-# (breakup)
  "Return BREAKUP's layouts number."
  (length (layouts breakup)))

(defun get-layout (nth breakup &aux (layout (aref (layouts breakup) nth)))
  "Get the Nth BREAKUP's layout. Make sure it is rendered first."
  (if (renderedp layout) layout (render-layout layout)))



;; ----------
;; Properties
;; ----------

(defmethod properties strnlcat
    ((breakup breakup) &key layout-# &aux (layouts (layouts breakup)))
  "Return a string advertising BREAKUP's layouts number.
When LAYOUT-#, also advertise BREAKUP's LAYOUT-#th layout properties.
Care is taken to render the layout first."
  (strnlcat
   (when layouts (format nil "~A layout~:P" (length layouts)))
   (when layout-#
     (assert layouts)
     ;; Using GET-LAYOUT here makes sure that the layout is rendered.
     (properties (get-layout layout-# breakup)))))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

;; #### TODO: maybe use PROGV to avoid the need for keyword arguments to
;; BREAK-LINEUP?

(defun %make-breakup (lineup width)
  "Break LINEUP for paragraph WIDTH. Return the resulting breakup."
  (apply #'break-lineup lineup width (algorithm-type (algorithm lineup))
	 (algorithm-options (algorithm lineup))))
