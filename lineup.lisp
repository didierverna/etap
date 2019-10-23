(in-package :etap)

(defclass kern ()
  ((value :initarg :value :reader value)))

(defun kernp (object)
  (typep object 'kern))

(defun make-kern (&rest initargs &key value)
  (declare (ignore value))
  (apply #'make-instance 'kern initargs))


(defclass glue ()
  ((value :initarg :value :reader value)
   (stretch :initarg :stretch :reader stretch)
   (shrink :initarg :shrink :reader shrink)))

(defun gluep (object)
  (typep object 'glue))

(defun make-glue (&rest initargs &key value stretch shrink)
  (declare (ignore value stretch shrink))
  (apply #'make-instance 'glue initargs))

(defun max-length (glue)
  (+ (value glue) (stretch glue)))

(defun min-length (glue)
  (- (value glue) (shrink glue)))


(defconstant +blanks+ '(#\Space #\Tab #\Newline))

(defun blankp (character)
  (member character +blanks+))

(defun lineup (text font features &aux lineup)
  (setq lineup
	(loop :with text := (string-trim +blanks+ text)
	      :with length := (length text)
	      :with glue := (let ((design-size (tfm:design-size font)))
			      (make-glue
			       :value (* (tfm:interword-space font)
					 design-size)
			       :stretch (* (tfm:interword-stretch font)
					   design-size)
			       :shrink (* (tfm:interword-shrink font)
					  design-size)))
	      :with i := 0
	      :while (< i length)
	      :for character
		:= (tfm:get-character (char-code (aref text i)) font)
	      :if (blankp (aref text i))
		:collect glue
		:and :do (setq i (position-if-not #'blankp text :start i))
	      :else :if character
		      :collect character
		      :and :do (incf i)
	      :else
		:do (incf i)))
  (when (member :ligatures features)
    (setq lineup
	  (loop :with elements := lineup
		:while elements
		:for elt1 := (car elements)
		:for elt2 := (cadr elements)
		:for lig := (when (and (typep elt1 'tfm::character-metrics)
				       (typep elt2 'tfm::character-metrics))
			      (tfm:ligature elt1 elt2))
		:if lig
		  :do (let ((composition (list)))
			(unless (tfm:delete-after lig)
			  (push elt2 composition))
			(push (tfm:composite lig) composition)
			(unless (tfm:delete-before lig)
			  (push elt1 composition))
			(setq elements (append composition (cddr elements))))
		  :and :unless (zerop (tfm:pass-over lig))
			 :append (subseq elements 0 (tfm:pass-over lig))
			 :and :do (setq elements
					(nthcdr (tfm:pass-over lig) elements))
		       :end
		:else
		  :collect elt1
		  :and :do (setq elements (cdr elements)))))
  (when (member :kerning features)
    (setq lineup
	  (loop :for elements :on lineup
		:for elt1 := (car elements)
		:for elt2 := (cadr elements)
		:for kern := (when (and (typep elt1 'tfm::character-metrics)
					(typep elt2 'tfm::character-metrics))
			       (tfm:kerning elt1 elt2))
		:collect elt1
		:when kern
		  :collect (make-kern
			    :value (* (tfm:design-size (tfm:font elt1))
				      kern)))))
  (when lineup (make-array (length lineup) :initial-contents lineup)))


(defun lineup-width (lineup start end)
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
	:finally (return (values width stretch shrink))))

(defun lineup-scale (lineup start end width)
  (multiple-value-bind (natural stretch shrink) (lineup-width lineup start end)
    (cond ((< natural width) (values :stretch (/ (- width natural) stretch)))
	  ((> natural width) (values :shrink (/ (- natural width) shrink))))))

(defun delta (lineup start end width)
  (/ (- width (lineup-width lineup start end))
     (count-if #'gluep lineup :start start :end end)))

(defun next-glue-position (lineup &optional (start 0))
  (position-if #'gluep lineup :start start))
