(in-package :etap)

(defgeneric width (object)
  (:method ((character-metrics tfm::character-metrics))
    (* (tfm:design-size (tfm:font character-metrics))
       (tfm:width character-metrics))))

(defgeneric height (object)
  (:method ((character-metrics tfm::character-metrics))
    (* (tfm:design-size (tfm:font character-metrics))
       (tfm:height character-metrics))))

(defgeneric depth (object)
  (:method ((character-metrics tfm::character-metrics))
    (* (tfm:design-size (tfm:font character-metrics))
       (tfm:depth character-metrics))))


(defclass kern ()
  ((width :initarg :width :reader width)))

(defun kernp (object)
  (typep object 'kern))

(defun make-kern (width)
  (make-instance 'kern :width width))


(defclass glue ()
  ((width :initarg :width :reader width)
   (stretch :initarg :stretch :reader stretch)
   (shrink :initarg :shrink :reader shrink)))

(defun gluep (object)
  (typep object 'glue))

(defun make-glue (width stretch shrink)
  (make-instance 'glue :width width :stretch stretch :shrink shrink))

(defun make-interword-glue
    (blank &aux (font (tfm:font blank)) (design-size (tfm:design-size font)))
  (make-glue (* (tfm:interword-space font) design-size)
	     (* (tfm:interword-stretch font) design-size)
	     (* (tfm:interword-shrink font) design-size)))


(defconstant +blanks+ '(#\Space #\Tab #\Newline))

(defun blankp (character)
  (member character +blanks+))

(defun lineup (text font features &aux lineup)
  (setq lineup
	(loop :with text := (string-trim +blanks+ text)
	      :with length := (length text)
	      :with i := 0
	      :while (< i length)
	      :for character
		:= (tfm:get-character (char-code (aref text i)) font)
	      :if (blankp (aref text i))
		:collect (make-interword-glue character)
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
		  :collect (make-kern (* (tfm:design-size (tfm:font elt1))
					 kern)))))
  (when lineup (make-array (length lineup) :initial-contents lineup)))


(defun lineup-width (lineup start end)
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- end)
	:for element := (aref lineup i)
	:do (incf width (width element))
	:when (gluep element)
	  :do (incf stretch (stretch element))
	  :and :do (incf shrink (shrink element))
	:finally (return (values width stretch shrink))))

(defun lineup-max-width (lineup start end)
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start end)
    (declare (ignore shrink))
    (+ width stretch)))

(defun lineup-min-width (lineup start end)
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start end)
    (declare (ignore stretch))
    (- width shrink)))

(defun lineup-scale (lineup start end target)
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start end)
    (cond ((= width target) (values :none 0))
	  ((< width target)
	   (unless (zerop stretch)
	     (values :stretch (/ (- target width) stretch))))
	  ((> width target)
	   (unless (zerop shrink)
	     (values :shrink (/ (- width target) shrink)))))))

(defun next-glue-position (lineup &optional (start 0))
  (position-if #'gluep lineup :start start))
