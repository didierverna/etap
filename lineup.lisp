(in-package :etap)

(defgeneric width (object)
  (:method ((null (eql nil)))
    0)
  (:method ((list list))
    (reduce #'+ (mapcar #'width list)))
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


(defclass break-point ()
  ())

(defun break-point-p (object)
  (typep object 'break-point))


(defclass discretionary (break-point)
  ((pre-break :initform nil :initarg :pre-break :accessor pre-break)
   (post-break :initform nil :initarg :post-break :accessor post-break)
   (no-break :initform nil :initarg :no-break :accessor no-break)))

(defun discretionaryp (object)
  (typep object 'discretionary))

(defun make-discretionary (&rest initargs &key pre-break post-break no-break)
  (declare (ignore pre-break post-break no-break))
  (apply #'make-instance 'discretionary initargs))


(defclass glue (break-point)
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


(defun lineup-aref (lineup i start end &aux (element (aref lineup i)))
  (if (discretionaryp element)
    (cond ((= i start) (post-break element))
	  ((= i (1- end)) (pre-break element))
	  (t (no-break element)))
    element))

(defun flatten-lineup (lineup start end)
  (loop :for i :from start :upto (1- end)
	:for element := (lineup-aref lineup i start end)
	:if (consp element)
	  :append element
	:else
	  :collect element))

(defun lineup-width (lineup start end)
  (unless end (setq end (length lineup)))
  (loop :with width := 0
	:with stretch := 0
	:with shrink := 0
	:for i :from start :upto (1- end)
	:for element := (lineup-aref lineup i start end)
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
    (cond ((= width target)
	   0)
	  ((< width target)
	   (unless (zerop stretch) (/ (- target width) stretch)))
	  ((> width target)
	   (unless (zerop shrink) (/ (- target width) shrink))))))

(defun next-break-position
    (lineup &optional (start 0)
	    &aux (length (length lineup))
		 (point (position-if #'break-point-p lineup :start start)))
  (unless (= start length)
    (if point
      (let ((next (1+ point)))
	;; (when (= next (length lineup)) (setq next nil))
	(typecase (aref lineup point)
	  (glue (list point next next))
	  (discretionary (list next point next))))
      (list length length length))))

(defun collect-word (word font)
  (loop :for char :across word
	:for character := (tfm:get-character (char-code char) font)
	:when character :collect character))

(defun hyphenate-word
    (word rules font &aux (points (hyphenation-points word rules)))
  (if points
    (loop :with hyphen := (tfm:get-character (char-code #\-) font)
	  :with elements
	    := (collect-word (subseq word 0 (car points)) font)
	  :for slices :on points
	  :do (setq elements
		    (append elements
			    (list (make-discretionary :pre-break hyphen))
			    (collect-word
			     (subseq word (car slices) (cadr slices))
			     font)))
	  :finally (return elements))
    (collect-word word font)))

;; #### NOTE: TeX's rules for hyphenating are more strict than ours here. For
;; instance, it will consider only one word betwee ntwo glues, so for instance
;; in "... foo.bar ...", bar will never be hyphenated. We do on the other
;; hand.
(defun lineup (text font features hyphenation-rules &aux lineup)
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
	      :else :if (alpha-char-p (aref text i))
		:collect (subseq text i
			   (position-if-not #'alpha-char-p text :start i))
		:and :do (setq i (position-if-not #'alpha-char-p text
				   :start i))
	      :else :if character
		:collect character
		:and :do (incf i)
	      :else
		:do (incf i)))
  (setq lineup
	(if (member :hyphenation features)
	  (loop :for element :in lineup
		:if (stringp element)
		  :append (hyphenate-word element hyphenation-rules font)
		:else
		  :collect element)
	  (loop :for element :in lineup
		:if (stringp element)
		  :append (collect-word element font)
		:else
		  :collect element)))
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
