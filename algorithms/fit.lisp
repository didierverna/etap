(in-package :etap)

(defgeneric fit-line-end (start lineup width disposition variant)
  (:method (start lineup width disposition (variant (eql :first)))
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (lineup-max-width lineup start i) :then (+ w ww)
	  :for ww := (when i (lineup-max-width lineup i ii))
	  :while (and ww (<= (+ w ww) width))
	  :finally (return i)))
  (:method (start lineup width disposition (variant (eql :best)))
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (lineup-width lineup start i) :then (+ w ww)
	  :for ww := (when i (lineup-width lineup i ii))
	  :while (and ww (<= (+ w ww) width))
	  :finally (return i)))
  (:method (start lineup width disposition (variant (eql :last)))
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (lineup-min-width lineup start i) :then (+ w ww)
	  :for ww := (when i (lineup-min-width lineup i ii))
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

(defgeneric fit-create-line
    (lineup start end width disposition variant &key &allow-other-keys)
  (:method (lineup start end width disposition (variant (eql :first)) &key)
    (create-line lineup start end :stretch 1))
  (:method (lineup start end width disposition (variant (eql :best)) &key)
    (create-line lineup start end))
  (:method (lineup start end width disposition (variant (eql :last)) &key)
    (create-line lineup start end :shrink 1))
  (:method (lineup start end width (disposition (eql :justified)) variant
	    &key sloppy)
    (multiple-value-bind (type ratio) (lineup-scale lineup start end width)
      (if type
	(create-line lineup start end type (if sloppy ratio (min ratio 1)))
	(create-line lineup start end)))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :fit))
     &key variant relax sloppy)
  (loop :for start := 0 :then (when end (1+ end)) ; discard glue
	:while start
	:for end := (fit-line-end start lineup width disposition variant)
	:collect (fit-create-line lineup start end width disposition variant
				  :sloppy sloppy)))