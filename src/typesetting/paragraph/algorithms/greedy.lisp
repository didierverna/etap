(in-package :etap)

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
    (harray disposition width get-boundary make-line
     &aux (breakup (make-instance 'breakup
		     :harray harray
		     :disposition disposition
		     :width width)))
  "Make a greedy breakup of HARRAY for a DISPOSITION paragraph of WIDTH.
See `make-greedy-lines' for further information."
  (unless (zerop (length harray))
    (let ((layout (make-instance 'layout
		    :lines (make-greedy-lines
			    harray width get-boundary make-line))))
      (setf (slot-value breakup 'layouts)
	    (make-array 1 :initial-element layout))))
  breakup)
