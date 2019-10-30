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

;; #### NOTE: maybe one day we would need to process kerns and ligatures
;; within user-defined discretionaries' pre/post/no-break values!
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


(defun lineup-aref (lineup i start end &aux (element (aref lineup i)))
  (if (discretionaryp element)
    ;; #### WARNING: after all the pre-processing done on the lineup,
    ;; including ligatures / kerning management in the presence of hyphenation
    ;; points, we may end up with lineups beginning or ending with
    ;; discretionaries (or even consecutive discretionaries for that matter).
    ;; When discretionaries begin or end the lineup, we must not consider them
    ;; as post- or pre-breaks though.
    (cond ((and (= i start) (not (zerop start)))
	   (post-break element))
	  ((and (= i (1- end)) (not (= end (length lineup))))
	   (pre-break element))
	  (t (no-break element)))
    element))

(defun flatten-lineup (lineup start end)
  (loop :for i :from start :upto (1- end)
	:for element := (lineup-aref lineup i start end)
	:if (consp element) :append element :else :collect element))

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
	(typecase (aref lineup point)
	  (glue (list point next next))
	  (discretionary
	   ;; #### NOTE: a discretionary ending the lineup shouldn't be
	   ;; considered as a break point, because there's nothing afterwards.
	   ;; Hence, the behavior is that of (LENGTH LENGTH LENGTH).
	   (list next (if (= next length) next point) next))))
      (list length length length))))


(defun get-character (char font)
  ;; #### FIXME: no input encoding support yet.
  (or (tfm:get-character (char-code char) font)
      ;; #### WARNING: this one had better be available! Fall back to a null
      ;; character?
      (tfm:get-character (char-code #\?) font)))

(defun collect-word (word font)
  (map 'list (lambda (char) (get-character char font)) word))

(defun hyphen-positions+1 (word)
  (loop :for i :from 1
	:for char :across word
	;; we don't want to collect a final hyphen's position, because if a
	;; word ends with one, there's not point in inserting a discretionary
	;; there. Either the word is followed by a glue, so we will be able to
	;; break, or it's followed by, e.g. punctuation, and we don't want to
	;; break there.
	:when (and (char= char #\-) (< i (length word))) :collect i))

(defun collect-hyphenated-word
    (word rules font &aux (points (hyphen-positions+1 word)) pre-break)
  (unless points
    (setq points (hyphenation-points word rules)
	  pre-break (list (get-character #\- font))))
  (if points
    (loop :for i :from 0
	  :for char :across word
	  :for character := (get-character char font)
	  :when (member i points)
	    :collect (make-discretionary :pre-break pre-break)
	  :collect character)
    (collect-word word font)))

(defun process-words (lineup hyphenate hyphenation-rules font)
  (loop :for element :in lineup
	:if (stringp element)
	  :append (if hyphenate
		    (collect-hyphenated-word element hyphenation-rules font)
		    (collect-word element font))
	:else
	  :collect element))


;; #### FIXME: Rework to explore all possible paths across characters and
;; discretionaries branches.
(defgeneric collect-kern (elt1 elt2 elt3)
  (:method (elt1 elt2 elt3)
    nil)
  (:method ((elt1 tfm::character-metrics) (elt2 tfm::character-metrics) elt3
	    &aux (kerning (tfm:kerning elt1 elt2)))
    (when kerning (make-kern (* kerning (tfm:design-size (tfm:font elt1))))))
  (:method ((elt1 tfm::character-metrics)
	    (elt2 discretionary)
	    (elt3 tfm::character-metrics))
    (when (pre-break elt2)
      (let ((kerning (tfm:kerning elt1 (car (pre-break elt2)))))
	(when kerning
	  (push (make-kern (* kerning (tfm:design-size (tfm:font elt1))))
		(pre-break elt2)))))
    (when (post-break elt2)
      (let ((kerning (tfm:kerning (car (last (post-break elt2))) elt3)))
	(when kerning
	  (endpush (make-kern (* kerning (tfm:design-size (tfm:font elt3))))
		   (post-break elt2)))))
    (if (no-break elt2)
      (let ((kerning1 (tfm:kerning elt1 (car (no-break elt2))))
	    (kerning2 (tfm:kerning (car (last (no-break elt2))) elt3)))
	(when kerning1
	  (push (make-kern (* kerning1 (tfm:design-size (tfm:font elt1))))
		(no-break elt2)))
	(when kerning2
	  (endpush (make-kern (* kerning2 (tfm:design-size (tfm:font elt3))))
		   (no-break elt2))))
      (let ((kerning (tfm:kerning elt1 elt3)))
	(when kerning
	  (setf (no-break elt2)
		(list (make-kern (* kerning
				    (tfm:design-size (tfm:font elt1)))))))))
    nil))

(defun process-kerning (lineup)
  (loop :for elements :on lineup
	:for elt1 := (car elements)
	:for elt2 := (cadr elements)
	:for elt3 := (caddr elements)
	:for kern := (collect-kern elt1 elt2 elt3)
	:collect elt1
	:when kern :collect kern))


(defun ligature (elt1 elt2)
  (and (typep elt1 'tfm::character-metrics)
       (typep elt2 'tfm::character-metrics)
       (tfm:ligature elt1 elt2)))

(defgeneric adjacent-characters-1 (element remainder)
  (:method (element remainder)
    nil)
  (:method ((element tfm::character-metrics) remainder)
    (list element))
  (:method ((element discretionary) remainder)
    (append (adjacent-characters (pre-break element))
	    (adjacent-characters (append (no-break element) remainder)))))

(defun adjacent-characters (lineup)
  (when lineup (adjacent-characters-1 (car lineup) (cdr lineup))))

(defgeneric process-ligatures-2 (elt1 elt2 remainder)
  (:method (elt1 elt2 remainder)
    (list (list elt1) (cons elt2 remainder)))
  (:method ((elt1 tfm::character-metrics) (elt2 tfm::character-metrics)
	    remainder
	    &aux (ligature (tfm:ligature elt1 elt2)) composition)
    (cond (ligature
	   (unless (tfm:delete-after ligature) (push elt2 composition))
	   (push (tfm:composite ligature) composition)
	   (unless (tfm:delete-before ligature) (push elt1 composition))
	   (list (subseq composition 0 (tfm:pass-over ligature))
		 ;; #### NOTE: because of the way TFM ligature programs work,
		 ;; we know that there's at least one thing left in this
		 ;; remainder. Indeed, the pass over cannot exceed the number
		 ;; of retained original characters.
		 (append (nthcdr (tfm:pass-over ligature) composition)
			 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder)))))
  (:method ((elt1 tfm::character-metrics) (elt2 discretionary) remainder
	    &aux (eat-elt1 (some
			    (lambda (character) (tfm:ligature elt1 character))
			    (adjacent-characters (cons elt2 remainder)))))
    (cond (eat-elt1
	   (setf (pre-break elt2)
		 (process-ligatures (cons elt1 (pre-break elt2)))
		 (no-break elt2)
		 (process-ligatures (cons elt1 (no-break elt2))))
	   (list nil (cons elt2 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder)))))
  (:method ((elt1 discretionary) (elt2 tfm::character-metrics) remainder
	    &aux (eat-elt2 (or (ligature (car (last (post-break elt1))) elt2)
			       (ligature (car (last (no-break elt1))) elt2))))
    (cond (eat-elt2
	   (setf (no-break elt1)
		 (process-ligatures (append (no-break elt1) (list elt2)))
		 (post-break elt1)
		 (process-ligatures (append (post-break elt1) (list elt2))))
	   (list nil (cons elt1 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder))))))
;;  (:method ((elt1 discretionary) (elt2 discretionary) remainder

(defun process-ligatures-1 (element remainder)
  (if remainder
    (process-ligatures-2 element (car remainder) (cdr remainder))
    (list (list element))))

(defun process-ligatures (lineup)
  (loop :for elements := lineup :then remainder
	:for (done remainder)
	  := (process-ligatures-1 (car elements) (cdr elements))
	:while elements
	:append done))


(defconstant +blanks+ '(#\Space #\Tab #\Newline))

(defun blankp (character)
  (member character +blanks+))

(defun word-constituent-p (char)
  (or (alpha-char-p char) (char= char #\-)))

;; #### NOTE: the hyphenation process below is simple, different from what TeX
;; #### does and should certainly be improved. For instance, TeX will consider
;; #### only one word between two glues, so for instance in "... foo.bar ...",
;; #### bar will never be hyphenated. There are also other rules that prevent
;; #### hyphenation in some situations, which we do not have right now.
(defun slice-string (string font)
  (loop :with string := (string-trim +blanks+ string)
	:with length := (length string)
	:with i := 0
	:while (< i length)
	:for char := (aref string i)
	:for character := (get-character char font)
	:if (blankp char)
	  :collect (make-interword-glue character)
	  ;; i cannot be NIL here because we've trimmed any end blanks.
	  :and :do (setq i (position-if-not #'blankp string :start i))
	:else :if (alpha-char-p char)
	  :collect (subseq string i
		     (position-if-not #'word-constituent-p string :start i))
	  ;; this could happen here on the other hand.
	  :and :do (setq i (or (position-if-not #'word-constituent-p string
				 :start i)
			       length))
	:else
	  :collect character
	  :and :do (incf i)))


(defun lineup (string font features hyphenation-rules &aux lineup)
  (setq lineup (slice-string string font))
  (setq lineup (process-words
		lineup (member :hyphenation features) hyphenation-rules font))
  (when (member :ligatures features) (setq lineup (process-ligatures lineup)))
  (when (member :kerning features) (setq lineup (process-kerning lineup)))
  (when lineup (make-array (length lineup) :initial-contents lineup)))
