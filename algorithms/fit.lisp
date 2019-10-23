(in-package :etap)

(defgeneric fit-line-end (start lineup width disposition variant)
  (:method (start lineup width disposition variant)
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (multiple-value-bind (natural stretch shrink)
			(lineup-width lineup start i)
		      (case variant
			(:first (+ natural stretch))
			(:best natural)
			(:last (- natural shrink))))
	    :then (+ w ww)
	  :for ww := (when i
		       (multiple-value-bind (natural stretch shrink)
			   (lineup-width lineup i ii)
			 (case variant
			   (:first (+ natural stretch))
			   (:best natural)
			   (:last (- natural shrink)))))
	  :while (and ww (<= (+ w ww) width))
	  :finally (return i)))
  (:method (start lineup width (disposition (eql :justified)) variant)
    (loop :with underfull-span
	  :with fit-spans := (list)
	  :with overfull-span
	  :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for s := (multiple-value-bind (natural stretch shrink)
			(lineup-width lineup start i)
		      (list natural (+ natural stretch) (- natural shrink)))
	    :then (mapcar #'+ s ss)
	  :for ss := (when i
		       (multiple-value-bind (natural stretch shrink)
			   (lineup-width lineup i ii)
			 (list natural (+ natural stretch) (- natural shrink))))
	  ;; #### NOTE: s becomes NIL when doing (mapcar #'+ s NIL).
	  :while (and s (not overfull-span))
	  :if (< (cadr s) width)
	    :do (setq underfull-span (cons i s))
	  :else :if (and (<= (caddr s) width) (>= (cadr s) width))
		  :do (push (cons i s) fit-spans)
	  :else :do (setq overfull-span (cons i s))
	  :finally
	     (return (case variant
		       (:first
			(cond (fit-spans (caar (last fit-spans)))
			      (underfull-span (car underfull-span))
			      (t (car overfull-span))))
		       (:last
			(cond (fit-spans (caar fit-spans))
			      (overfull-span (car overfull-span))
			      (t (car underfull-span))))
		       (:best
			(if fit-spans
			  ;; #### NOTE: two choices might be best-equals,
			  ;; when we get the same delta, once for shrink and
			  ;; once for stretch. We could offer those two
			  ;; alternatives.
			  (cdr (first (sort
				       (mapcar
					   (lambda (fit-span)
					     (cons (delta lineup start
							  (car fit-span) width)
						   (car fit-span)))
					 fit-spans)
				       #'<
				       :key (lambda (elt) (abs (car elt))))))
			  (let ((underfull-delta
				  (when underfull-span
				    (- width
				       (lineup-width
					lineup start (car underfull-span)))))
				(overfull-delta
				  (when overfull-span
				    (- (lineup-width
					lineup start (car overfull-span))
				       width))))
			    (cond ((and underfull-delta overfull-delta)
				   (if (< underfull-delta overfull-delta)
				     (car underfull-span)
				     (car overfull-span)))
				  (underfull-delta (car underfull-span))
				  (t (car overfull-span)))))))))))

(defun create-line-1 (lineup start end glue-length)
  (unless end (setq end (length lineup)))
  (make-line
   :pinned-characters
   (loop :with x := 0
	 :for i :from start :upto (1- end)
	 :for element := (aref lineup i)
	 :if (typep element 'tfm::character-metrics)
	   :collect (make-pinned-character :x x :character-metrics element)
	   :and :do (incf x (* (tfm:width element)
			       (tfm:design-size (tfm:font element))))
	 :else :if (kernp element)
		 :do (incf x (value element))
	 :else :if (gluep element)
		 :do (incf x (funcall glue-length element)))))

(defgeneric fit-create-line (lineup start end width disposition variant)
  (:method (lineup start end width disposition (variant (eql :first)))
    (create-line lineup start end :stretch 1))
  (:method (lineup start end width disposition (variant (eql :best)))
    (create-line lineup start end))
  (:method (lineup start end width disposition (variant (eql :last)))
    (create-line lineup start end :shrink 1))
  (:method (lineup start end width (disposition (eql :justified)) variant
	    &aux span glue-length)
    (declare (ignore variant))
    (setq span (multiple-value-bind (natural stretch shrink)
		   (lineup-width lineup start end)
		 (list natural (+ natural stretch) (- natural shrink)))
	  glue-length (cond ((> (caddr span) width) #'min-length)
			    ((< (cadr span) width) #'max-length)
			    (t (let ((delta (delta lineup start end width)))
				 (lambda (glue) (+ (value glue) delta))))))
    (create-line-1 lineup start end glue-length)))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :*-fit)) &key variant)
  (loop :for start := 0 :then (when end (1+ end)) ; discard glue
	:while start
	:for end := (fit-line-end start lineup width disposition variant)
	:collect (fit-create-line lineup start end width disposition variant)))
