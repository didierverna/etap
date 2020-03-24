;; #### WARNING: there are a number of characteristics in the lineup that
;; #### affect the way algorithms are implemented. These specificities should
;; #### be kept in mind because should they change, said algorithms may become
;; #### buggy. In particular:
;; #### - the paragraph string is trimmed for spaces, so there is no glue at
;; ####   the end (we don't have TeX's last line specific treatment notably).
;; #### - the only discretionaries that we have come from the hyphenation
;; ####   step, which mean that said discretionaries only appear in the middle
;; ####   of words (we may, however, have discretionaries at the beginning or
;; ####   end of the lineup).

(in-package :etap)

(defgeneric width (object)
  (:method ((clue (eql :hyphenation-clue)))
    0)
  (:method ((null (eql nil)))
    0)
  (:method ((list list))
    (reduce #'+ (mapcar #'width list)))
  (:method ((character-metrics tfm:character-metrics))
    (tfm:width character-metrics)))

(defgeneric height (object)
  (:method ((character-metrics tfm:character-metrics))
    (tfm:height character-metrics)))

(defgeneric depth (object)
  (:method ((character-metrics tfm:character-metrics))
    (tfm:depth character-metrics)))


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

(defun make-interword-glue (blank &aux (font (tfm:font blank)))
  (make-glue (tfm:interword-space font)
	     (tfm:interword-stretch font)
	     (tfm:interword-shrink font)))


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
	;; #### FIXME: this works for now, but it is not quite right in the
	;; #### general case. When ELEMENT is a list (typically the contents
	;; #### ;; of a discretionary, there could be anything inside,
	;; #### including, e.g., glues. See also the long comment above the
	;; KERNING function.
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


(defstruct (span
	    :conc-name
	    (:constructor make-span (normal-width min-width max-width)))
  normal-width min-width max-width)

(defmethod width ((span span))
  (normal-width span))

(defun lineup-span (lineup start stop)
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start stop)
    (make-span width (- width shrink) (+ width stretch))))


(defun lineup-scale (lineup start end target)
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start end)
    (cond ((= width target)
	   0)
	  ((< width target)
	   (unless (zerop stretch) (/ (- target width) stretch)))
	  ((> width target)
	   (unless (zerop shrink) (/ (- target width) shrink))))))


(defun word-stop-p (lineup stop)
  (or (= stop (length lineup)) (gluep (aref lineup stop))))


(defstruct (boundary
	    :conc-name
	    (:constructor make-boundary (stop next-start next-search)))
  stop next-start next-search)

(defun word-boundary-p (lineup boundary)
  (word-stop-p lineup (stop boundary)))

(defun word-boundaries (lineup boundaries)
  (remove-if-not (lambda (boundary) (word-boundary-p lineup boundary))
		 boundaries))

(defun hyphen-boundaries (lineup boundaries)
  (remove-if (lambda (boundary) (word-boundary-p lineup boundary))
	     boundaries))

(defun boundary-scales (lineup start width boundaries)
  (mapcar
      (lambda (boundary)
	(cons boundary (lineup-scale lineup start (stop boundary) width)))
    boundaries))

(defun next-boundary
    (lineup &optional (start 0)
	    &aux (length (length lineup))
		 (point (position-if #'break-point-p lineup :start start)))
  (unless (= start length)
    (if point
      (let ((next (1+ point)))
	(typecase (aref lineup point)
	  (glue (make-boundary point next next))
	  (discretionary
	   ;; #### NOTE: a discretionary ending the lineup shouldn't be
	   ;; considered as a break point, because there's nothing afterwards.
	   ;; Hence, the behavior is that of (LENGTH LENGTH LENGTH).
	   (make-boundary next (if (= next length) next point) next))))
      (make-boundary length length length))))

(defun next-boundaries (lineup start width)
  (loop :with underfull-boundary
	:with fit-boundaries := (list)
	:with overfull-boundary
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-search boundary))
	:while (and boundary (not overfull-boundary))
	:for span := (lineup-span lineup start (stop boundary))
	:if (< (max-width span) width)
	  :do (setq underfull-boundary boundary)
	:else :if (and (<= (min-width span) width)
		       (>= (max-width span) width))
		:do (push boundary fit-boundaries)
	:else
	  :do (setq overfull-boundary boundary)
	:finally
	   (return
	     (values underfull-boundary fit-boundaries overfull-boundary))))


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


;; #### NOTE: the procedures handling ligatures and kerning below are aware of
;; #### discretionaries, but they are really meant for those inserted
;; #### automatically during the hyphenation process, that is,
;; #### \discretionary{-}{}{} in the middle of a word. They may be useful in
;; #### the future, for some more general cases, but combining ligatures,
;; #### kerning, and discretionaries is impossible to do statically in
;; #### general. For example, if we end up with different potential ligatures
;; #### starting from a post-break element and going to both a no-break and a
;; #### pre-break later on, we can't represent that statically. The only truly
;; #### general solution is to delay ligature and kerning processing until the
;; #### lineup is flattened. But then, this means that we also need to do that
;; #### every time we want to poll the size of various lineup chunks. This
;; #### could be rather expensive (although I haven't tried it).

(defun kerning (elt1 elt2)
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:kerning elt1 elt2)))

(defgeneric collect-kern (elt1 elt2 remainder)
  (:method (elt1 elt2 remainder)
    nil)
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
	    remainder &aux (kerning (tfm:kerning elt1 elt2)))
    (when kerning (make-kern kerning)))
  (:method ((elt1 tfm:character-metrics) (elt2 discretionary) remainder)
    (when (pre-break elt2)
      (let ((kerning (kerning elt1 (car (pre-break elt2)))))
	(when kerning (push (make-kern kerning) (pre-break elt2)))))
    (if (no-break elt2)
      (let ((kerning (kerning elt1 (car (no-break elt2)))))
	(when kerning (push (make-kern kerning) (no-break elt2))))
      (let ((kerning (kerning elt1 (car remainder))))
	(when kerning (setf (no-break elt2) (list (make-kern kerning))))))
    nil)
  (:method ((elt1 discretionary) (elt2 tfm:character-metrics) remainder)
    (when (no-break elt1)
      (let ((kerning (kerning (car (last (no-break elt1))) elt2)))
	(when kerning (endpush (make-kern kerning) (no-break elt1)))))
    (when (post-break elt1)
      (let ((kerning (kerning (car (last (post-break elt1))) elt2)))
	(when kerning (endpush (make-kern kerning) (post-break elt1)))))
    nil))

(defun process-kerning (lineup)
  (loop :for elements :on lineup
	:for elt1 := (car elements)
	:for elt2 := (cadr elements)
	:for remainder := (cddr elements)
	:for kern := (collect-kern elt1 elt2 remainder)
	:collect elt1
	:when kern :collect kern))


(defun ligature (elt1 elt2)
  (and (typep elt1 'tfm:character-metrics)
       (typep elt2 'tfm:character-metrics)
       (tfm:ligature elt1 elt2)))

(defgeneric adjacent-characters-1 (element remainder)
  (:method (element remainder)
    nil)
  (:method ((element tfm:character-metrics) remainder)
    (list element))
  (:method ((element discretionary) remainder)
    (append (adjacent-characters (pre-break element))
	    (adjacent-characters (append (no-break element) remainder)))))

(defun adjacent-characters (lineup)
  (when lineup (adjacent-characters-1 (car lineup) (cdr lineup))))


;; #### NOTE: after processing ligatures, we may end up with adjacent
;; #### discretionaries, but this is normal. For example, in the word
;; #### ef-fi-cient, what we get eventually is
;; #### e\discretionary{f-}{fi}{ffi}\discretionary{-}{}{}cient.
(defgeneric process-ligatures-2 (elt1 elt2 remainder)
  (:method (elt1 elt2 remainder)
    (list (list elt1) (cons elt2 remainder)))
  (:method ((elt1 tfm:character-metrics) (elt2 tfm:character-metrics)
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
  (:method ((elt1 tfm:character-metrics) (elt2 discretionary) remainder
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
  (:method ((elt1 discretionary) (elt2 tfm:character-metrics) remainder
	    &aux (eat-elt2 (or (ligature (car (last (no-break elt1))) elt2)
			       (ligature (car (last (post-break elt1))) elt2))))
    (cond (eat-elt2
	   (setf (no-break elt1)
		 (process-ligatures (append (no-break elt1) (list elt2)))
		 (post-break elt1)
		 (process-ligatures (append (post-break elt1) (list elt2))))
	   (list nil (cons elt1 remainder)))
	  (t
	   (list (list elt1) (cons elt2 remainder))))))

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


(define-constant +blanks+ '(#\Space #\Tab #\Newline))

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


(defun lineup (string font hyphenation-rules
	       &key kerning ligatures hyphenation
	       &aux lineup)
  (setq lineup (slice-string string font))
  (setq lineup (process-words lineup hyphenation hyphenation-rules font))
  (when ligatures (setq lineup (process-ligatures lineup)))
  (when kerning (setq lineup (process-kerning lineup)))
  (when (and lineup hyphenation)
    (mapc (lambda (element)
	    (when (discretionaryp element)
	      (push :hyphenation-clue (no-break element))))
      lineup))
  (when lineup (make-array (length lineup) :initial-contents lineup)))
