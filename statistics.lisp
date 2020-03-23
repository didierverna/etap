(in-package :etap)


(defstruct (node (:constructor make-node (boundary children)))
  boundary children)

(defun create-node (lineup boundary width)
  (when boundary
    (if (= (stop boundary) (length lineup))
      (make-node boundary nil)
      (multiple-value-bind (underfull-boundary fit-boundaries overfull-boundary)
	  (next-boundaries lineup (next-start boundary) width)
	(when underfull-boundary (push underfull-boundary fit-boundaries))
	(when overfull-boundary (push overfull-boundary fit-boundaries))
	(make-node
	 boundary
	 (mapcar (lambda (boundary) (create-node lineup boundary width))
	   fit-boundaries))))))

(defun root-node (lineup width)
  (multiple-value-bind (underfull-boundary fit-boundaries overfull-boundary)
      (next-boundaries lineup 0 width)
    (when underfull-boundary (push underfull-boundary fit-boundaries))
    (when overfull-boundary (push overfull-boundary fit-boundaries))
    (make-node
     nil
     (mapcar (lambda (boundary) (create-node lineup boundary width))
       fit-boundaries))))


(defun node-lines (node)
  (mapcan (lambda (child)
	    (let ((head-line (cons (next-start (node-boundary node))
				   (stop (node-boundary child))))
		  (next-lines (node-lines child)))
	      (if next-lines
		(mapcar (lambda (lines) (cons head-line lines))
		  next-lines)
		(list (list head-line)))))
    (node-children node)))

(defun root-node-lines (node)
  (mapcan (lambda (child)
	    (let ((head-line (cons 0 (stop (node-boundary child))))
		  (next-lines (node-lines child)))
	      (if next-lines
		(mapcar (lambda (lines) (cons head-line lines))
		  next-lines)
		(list (list head-line)))))
    (node-children node)))


(defstruct (solution
	    (:constructor make-solution (lines hyphens underfulls overfulls)))
  lines hyphens underfulls overfulls)

(defun create-solution (lineup width lines)
  (loop :with hyphens := 0
	:with underfulls := 0
	:with overfulls := 0
	:for line :in lines
	:unless (word-stop-p lineup (cdr line))
	  :do (incf hyphens)
	;; #### WARNING: dirty trick to not count the last line as underfull!
	:when (and (< (cdr line) (length lineup))
		   (< (lineup-max-width lineup (car line) (cdr line)) width))
	  :do (incf underfulls)
	:when (> (lineup-min-width lineup (car line) (cdr line)) width)
	  :do (incf overfulls)
	:finally
	   (return
	     (make-solution lines hyphens underfulls overfulls))))

(defun print-solutions
    (state
     &key (width (paragraph-width state))
	  (text (text state))
	  (kerning (cadr (member :kerning (features state))))
	  (ligatures (cadr (member :ligatures (features state))))
	  (hyphenation (cadr (member :hyphenation (features state)))))
  (let* ((lineup
	   (lineup text (font state) (hyphenation-rules state)
	     :kerning kerning :ligatures ligatures :hyphenation hyphenation))
	 (solutions
	   (mapcar (lambda (lines) (make-solution lineup width lines))
	     (root-node-lines (root-node lineup width)))))
    solution))


#|(paragraph (or (find-if
			 (lambda (paragraph)
			   (and (zerop (duncan-paragraph-hyphens paragraph))
				(zerop (duncan-paragraph-underfulls paragraph))
				(zerop (duncan-paragraph-overfulls paragraph))))
			 paragraphs)
    (if selection
      (mapcar (lambda (line)
		(duncan-create-line lineup (car line) (cdr line)
				    width sloppy))
	(duncan-paragraph-lines (car selection)))

	  )))
|#
