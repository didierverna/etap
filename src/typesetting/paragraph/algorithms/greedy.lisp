(in-package :etap)
(in-readtable :etap)

(defun make-greedy-lines (harray width get-boundary make-line)
  "Break HARRAY greedily for paragraph WIDTH.
- (GET-BOUNDARY HARRAY BOL WIDTH) is called to get the end of line boundary
  for a line starting at BOL,
- (MAKE-LINE HARRAY BOL BOUNDARY) is called to make the line in question."
  (loop :for bol := *bop* :then (break-point boundary)
	:for boundary := (funcall get-boundary harray bol width)
	:while boundary
	:collect (funcall make-line harray bol boundary)))

(defun make-greedy-breakup
    (lineup width get-boundary make-line &aux (harray (harray lineup)) breakup)
  "Break LINEUP greedily for paragraph WIDTH.
See `make-greedy-lines' for information on GET-BOUNDARY and MAKE-LINE."
  (setq breakup (make-instance 'breakup :lineup lineup :paragraph-width width))
  (unless (zerop (length harray))
    (setf (slot-value breakup 'layouts)
	  (make-array 1
	    :initial-element (make-instance 'layout
			       :breakup breakup
			       :lines (make-greedy-lines harray width
					get-boundary make-line)))))
  breakup)
