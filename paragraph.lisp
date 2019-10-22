(in-package :etap)

(defun lineup-width (lineup start end &optional (glue-length :natural))
  (setq glue-length (case glue-length
		      (:natural #'value)
		      (:max #'max-length)
		      (:min #'min-length)))
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:for i :from start :upto (1- end)
	:for element := (aref lineup i)
	:if (typep element 'tfm::character-metrics)
	  :do (incf width (* (tfm:design-size (tfm:font element))
			     (tfm:width element)))
	:else :if (kernp element)
		:do (incf width (value element))
	:else :if (gluep element)
		:do (incf width (funcall glue-length element))
	:finally (return width)))

(defun lineup-span (lineup start end)
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- end)
	:for element := (aref lineup i)
	:if (typep element 'tfm::character-metrics)
	  :do (incf width (* (tfm:design-size (tfm:font element))
			     (tfm:width element)))
	:else :if (kernp element)
		:do (incf width (value element))
	:else :if (gluep element)
		:do (incf width (value element))
		:and :do (incf stretch (stretch element))
		:and :do (incf shrink (shrink element))
	:finally (return (list width (+ width stretch) (- width shrink)))))

(defun delta (lineup start end width)
  (/ (- width (lineup-width lineup start end))
     (count-if #'gluep lineup :start start :end end)))

(defun next-glue-position (lineup &optional (start 0))
  (position-if #'gluep lineup :start start))

(defgeneric line-end
    (start lineup width disposition algorithm &key &allow-other-keys)
  (:method (start lineup width disposition (algorithm (eql :fixed)) &key)
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (lineup-width lineup start i) :then (+ w ww)
	  :for ww := (when i (lineup-width lineup i ii))
	  :while (and ww (<= (+ w ww) width))
	  :finally (return i)))
  (:method (start lineup width disposition (algorithm (eql :*-fit))
	    &key variant &aux glue-length)
    (setq glue-length (case variant
			(:first :max)
			(:best :natural)
			(:last :min)))
    (loop :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for w := (lineup-width lineup start i glue-length) :then (+ w ww)
	  :for ww := (when i (lineup-width lineup i ii glue-length))
	  :while (and ww (<= (+ w ww) width))
	  :finally (return i)))
  ;; #### FIXME: method for :fixed :justified
  (:method (start lineup width (disposition (eql :justified)) algorithm
	    &key variant)
    (loop :with underfull-span
	  :with fit-spans := (list)
	  :with overfull-span
	  :for i := (next-glue-position lineup start) :then ii
	  :for ii := (when i (next-glue-position lineup (1+ i)))
	  :for s := (lineup-span lineup start i) :then (mapcar #'+ s ss)
	  :for ss := (when i (lineup-span lineup i ii))
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

(defun line-boundaries
    (lineup width disposition algorithm &rest options &key &allow-other-keys)
  (when lineup
    (loop :for start := 0 :then (when end (1+ end))
	  :while start
	  :for end := (apply #'line-end
			start lineup width disposition algorithm options)
	  :collect (list start end))))

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

(defgeneric create-line
    (lineup boundary width disposition algorithm &key &allow-other-keys)
  (:method (lineup boundary width disposition (algorithm (eql :fixed))
	    &key &aux (start (car boundary)) (end (cadr boundary)))
    (create-line-1 lineup start end #'value))
  (:method (lineup boundary width disposition (algorithm (eql :*-fit))
	    &key variant
	    &aux (start (car boundary)) (end (cadr boundary)) glue-length)
    (setq glue-length (case variant
			(:first #'max-length)
			(:best #'value)
			(:last #'min-length)))
    (create-line-1 lineup start end glue-length))
  ;; #### FIXME: method for :fixed :justified
  (:method (lineup boundary width (disposition (eql :justified)) algorithm
	    &key variant
	    &aux (start (car boundary)) (end (cadr boundary)) span glue-length)
    (declare (ignore variant))
    (setq span (lineup-span lineup start end)
	  glue-length
	  (cond ((> (caddr span) width) #'min-length)
		((< (cadr span) width) #'max-length)
		(t (let ((delta (delta lineup start end width)))
		     (lambda (glue) (+ (value glue) delta))))))
    (create-line-1 lineup start end glue-length)))

(defun create-lines (lineup width disposition algorithm)
  (mapcar (lambda (boundary)
	    (apply #'create-line
	      lineup boundary width disposition
	      (car algorithm) (cdr algorithm)))
    (apply #'line-boundaries lineup width disposition
	   (car algorithm) (cdr algorithm))))

(defun create-pinned-lines (lineup width disposition algorithm)
  (loop :for line :in (create-lines lineup width disposition algorithm)
	:for x := (case disposition
		    ((:flush-left :justified) 0)
		    (:centered (/ (- width (width line)) 2))
		    (:flush-right (- width (width line))))
	:for y := 0 :then (+ y 12)
	:collect (make-pinned-line :x x :y y :line line)))


(defclass paragraph ()
  ((width :initform 0 :initarg :width :accessor width)
   (pinned-lines :initform nil :initarg :pinned-lines :accessor pinned-lines)))

(defmethod height ((paragraph paragraph))
  (height (first (pinned-lines paragraph))))

(defmethod depth ((paragraph paragraph))
  (with-accessors ((pinned-lines pinned-lines)) paragraph
    (+ (* (1- (length pinned-lines)) 12)
       (depth (car (last pinned-lines))))))

(defun make-paragraph (&rest initargs)
  (apply #'make-instance 'paragraph initargs))

(defun create-paragraph (lineup width disposition algorithm)
  (make-paragraph
   :width width
   :pinned-lines (create-pinned-lines lineup width disposition algorithm)))
