(in-package :etap)

(defun make-greedy-lines (harray width beds get-boundary make-line)
  "Make HARRAY lines with river BEDS for a paragraph of WIDTH.
If HARRAY is empty, return NIL.
This function processes HARRAY in a greedy way:
- (GET-BOUNDARY HARRAY BOL WIDTH) is called to get the end of line boundary
  for a line starting at BOL,
- (MAKE-LINE HARRAY BOL BOUNDARY BEDS) is called to make the line in
  question."
  (unless (zerop (length harray))
    (loop :for bol := *bop* :then (break-point boundary)
	  :for boundary := (funcall get-boundary harray bol width)
	  :while boundary
	  :collect (funcall make-line harray bol boundary beds))))


(defclass greedy-breakup (breakup)
  ((pinned-lines :documentation "This breakup's pinned lines."
		 :initform nil :reader pinned-lines))
  (:documentation "The Greedy Breakup class.
This class is used by greedy algorithms to store their only solution."))

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

(defun make-greedy-breakup
    (harray disposition width beds get-boundary make-line)
  "Make a greedy breakup of HARRAY for a DISPOSITION paragraph of WIDTH.
See `make-greedy-lines' for further information."
  (make-instance 'greedy-breakup
    :disposition disposition
    :width width
    :lines (make-greedy-lines harray width beds get-boundary make-line)))
