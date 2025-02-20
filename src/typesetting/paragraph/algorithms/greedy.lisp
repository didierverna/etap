(in-package :etap)

(defclass greedy-breakup (breakup)
  ((layout
    :documentation "This breakup's layout."
    :initform nil :initarg :layout :reader layout))
  (:documentation "The Greedy Breakup class.
This class is used by greedy algorithms to store their only solution."))

(defun make-greedy-lines (harray width get-boundary make-line)
  "Make HARRAY lines for a paragraph of WIDTH.
This function processes HARRAY in a greedy way:
- (GET-BOUNDARY HARRAY BOL WIDTH) is called to get the end of line boundary
  for a line starting at BOL,
- (MAKE-LINE HARRAY BOL BOUNDARY) is called to make the line in
  question."
  (loop :for bol := *bop* :then (break-point boundary)
	:for boundary := (funcall get-boundary harray bol width)
	:while boundary
	:collect (funcall make-line harray bol boundary)))

(defun make-greedy-breakup
    (harray disposition width get-boundary make-line)
  "Make a greedy breakup of HARRAY for a DISPOSITION paragraph of WIDTH.
See `make-greedy-lines' for further information."
  (if (zerop (length harray))
    (make-instance 'greedy-breakup
      :disposition disposition
      :width width)
    (make-instance 'greedy-breakup
      :disposition disposition
      :width width
      :layout (make-instance 'layout
		:lines (make-greedy-lines harray width
					  get-boundary make-line)))))


;; #### WARNING: this method requires the layout to have been rendered
;; already!
(defmethod properties strnlcat
    ((breakup greedy-breakup) &key &aux (layout (layout breakup)))
  "Return a string advertising greedy BREAKUP's properties."
  (when layout (properties layout)))


(defmethod renditions-# ((breakup greedy-breakup))
  "Return greedy BREAKUP's renditions number (0 or 1)."
  (if (layout breakup) 1 0))

(defmethod get-rendition
    (nth (breakup greedy-breakup) &aux (layout (layout breakup)))
  "Return the only greedy BREAKUP's rendition."
  (assert (and layout (zerop nth)))
  (render-layout layout (disposition breakup) (width breakup)))
