(in-package :etap)

;; ==========================================================================
;; Breakups
;; ==========================================================================

(defclass breakup ()
  ((harray
    :documentation "This breakup's harray."
    :initarg :harray :reader harray)
   (disposition
    :documentation "The breakup's disposition."
    :initarg :disposition :reader disposition)
   (width
    :documentation "The breakup's paragraph width."
    :initarg :width :reader width)
   (layouts
    :documentation "This breakup's layouts array, or NIL (the default).
A distinction is made between the case where there's nothing to typeset
(an empty harray) and where an algorithm couldn't find any solution (which
currently never happens because all algorithms turn to a fallback solution):
- when there's nothing to typeset, this slot remains NIL,
- when no solution could be found, an array of size 0 will be created."
    :initform nil :reader layouts))
  (:documentation "The BREAKUP class.
A breakup is the result of running a paragraph formatting algorithm on an
harray for a specific paragraph width. Algorithms may provide their own
breakup subclass in order to store specific global properties."))

;; #### NOTE: this function is called with all the algorithm options, without
;; knowing in advance whether they're going to be used or not, so we need to
;; relax keyword argument checking. Also, each algorithm is responsible for
;; instantiating the appropriate breakup class, so we cannot short-circuit
;; anything here in case of an empty harray.
(defgeneric break-harray
    (harray disposition width algorithm &key &allow-other-keys)
  (:documentation
   "Break HARRAY as a DISPOSITION paragraph of WIDTH with ALGORITHM.
Return the resulting breakup."))

(defun %make-breakup (lineup disposition width algorithm)
  "Make a new breakup out of LINEUP for a DISPOSITION paragraph of WITH.
Use ALGORITHM to do so."
  (apply #'break-harray (harray lineup) disposition width
	 (algorithm-type algorithm) (algorithm-options algorithm)))




;; ==========================================================================
;; Rendering
;; ==========================================================================

;; #### NOTE: the call to LENGTH below will return 0 when the LAYOUTS slot is
;; nil, as well as when it's an array of size 0.
(defun layouts-# (breakup)
  "Return BREAKUP's layouts number."
  (length (layouts breakup)))

(defun get-layout (nth breakup &aux (layout (aref (layouts breakup) nth)))
  "Get the Nth BREAKUP's layout. Make sure it is rendered first."
  (if (renderedp layout) layout (render-layout layout)))




;; ==========================================================================
;; Properties
;; ==========================================================================

(defmethod properties strnlcat
    ((breakup breakup) &key layout-# &aux (layouts (layouts breakup)))
  "Return a string advertising BREAKUP's properties.
When LAYOUT-#, also advertise this layout's properties.
Note that this entails the rendering of the layout."
  (strnlcat
   (when layouts (format nil "~A layout~:P" (length layouts)))
   (when layout-#
     (assert layouts)
     ;; Using GET-LAYOUT here makes sure that the layout is rendered.
     (properties (get-layout layout-# breakup)))))
