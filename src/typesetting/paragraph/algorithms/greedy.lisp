(in-package :etap)

(defclass greedy-breakup (breakup)
  ((pinned-lines :documentation "The pinned lines."
		 :initform nil :reader pinned-lines))
  (:documentation "The Greedy Breakup class.
This class is used by greedy algorithms to store their only solution. Current
algorithms using it are Fixed, Fit, and Barnett."))

(defmethod initialize-instance :after ((breakup greedy-breakup) &key lines)
  "Pin LINES in BREAKUP."
  (when lines
    (setf (slot-value breakup 'pinned-lines)
	  (loop :with width := (width breakup)
		:with baseline-skip := (baseline-skip (harray (car lines)))
		:with x := (case (disposition-type (disposition breakup))
			     ((:flush-left :justified)
			      (lambda (line) (declare (ignore line)) 0))
			     (:centered
			      (lambda (line) (/ (- width (width line)) 2)))
			     (:flush-right
			      (lambda (line) (- width (width line)))))
		:for y := 0 :then (+ y baseline-skip)
		:for line :in lines
		:collect (pin-line line (funcall x line) y)))))

(defun greedy-get-lines (harray width beds get-boundary make-line)
  "Get HARRAY lines for a paragraph of WIDTH in a greedy way.
If HARRAY is empty, return NIL.
- (GET-BOUNDARY HARRAY BOL WIDTH) should return the next end of line boundary.
- (MAKE-LINE HARRAY BOL BOUNDARY BEDS) should return the line in question."
  (unless (zerop (length harray))
    (loop :for bol := *bop* :then (break-point boundary)
	  :for boundary := (funcall get-boundary harray bol width)
	  :while boundary
	  :collect (funcall make-line harray bol boundary beds))))
