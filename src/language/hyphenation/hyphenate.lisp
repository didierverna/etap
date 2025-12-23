(in-package :etap)
(in-readtable :etap)


(defparameter *lefthyphenmin* 2
  "The minimum number of characters before a potential hyphenation point.")

(defparameter *righthyphenmin* 3
  "The minimum number of characters after a potential hyphenation point.")

(defun hyphenate (word rules)
  "Hyphenate WORD (a string) based on RULES.
Return a list of hyphenation points."
  (multiple-value-bind (points found) (hyphenation-exception word rules)
    (if found
      points
      (loop :with word := (concatenate 'string '(#\.) word '(#\.))
	    :with length := (length word)
	    :with points := (list)
	    :for i :from 0 :upto (1- length)
	    :do (loop :for j :from (1+ i) :upto length
		      :for pattern
			:= (hyphenation-pattern (subseq word i j) rules)
		      :when pattern
			:do (mapc
				(lambda (elt)
				  (let* ((position (+ i (car elt) -1))
					 (value (cdr elt))
					 (point (member position points
							:key #'car)))
				    (if point
				      (when (> value (cdar point))
					(setf (cdar point) value))
				      (push (cons position value) points))))
			      pattern))
	    :finally (return
		       (sort
			   (remove-if (lambda (position)
					(or (< position *lefthyphenmin*)
					    (> position
					       ;; #### WARNING: LENGTH is too
					    ;; large by 2 because it counts
					    ;; the two additional dots!
					    (- length *righthyphenmin* 2))))
				   (mapcar #'car
				     (remove-if-not #'oddp points :key #'cdr)))
			#'<))))))
