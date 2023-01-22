;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

(defparameter *kp-variants*
  '(:graph :dynamic))

(defparameter *kp-variants-help-keys*
  '(:kp-variant-graph :kp-variant-dynamic))


(defmacro define-kp-caliber (name min default max)
  "Define a NAMEd Knuth-Plass caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber kp ,name ,min ,default ,max))

(defmacro calibrate-kp (name &optional infinity)
  "Calibrate NAMEd Knuth-Plass variable."
  `(calibrate kp ,name ,infinity))

(define-kp-caliber line-penalty 0 10 100)
(define-kp-caliber hyphen-penalty -10000 50 10000)
(define-kp-caliber explicit-hyphen-penalty -10000 50 10000)
(define-kp-caliber adjacent-demerits 0 10000 10000)
(define-kp-caliber double-hyphen-demerits 0 10000 10000)
(define-kp-caliber final-hyphen-demerits 0 5000 10000)
(define-kp-caliber pre-tolerance -1 100 10000)
(define-kp-caliber tolerance 0 200 10000)
(define-kp-caliber emergency-stretch 0 0 20)
(define-kp-caliber looseness -10 0 10)

(defparameter *kp-tooltips*
  '(:kp-variant-graph "Graph-based implementation."
    :kp-variant-dynamic "Dynamic programming implementation."))

(defmacro kp-default (variable)
  `(default-variable ,variable kp))


(defun scale-fitness-class (scale)
  (cond ((or (null scale) (< scale -1/2)) 3)
	((<= -1/2 scale 1/2) 2)
	((<= 1/2 scale 1) 1)
	((< 1 scale) 0)))

(defun local-demerits (badness penalty line-penalty)
  (cond ((and (numberp penalty) (<= 0 penalty))
	 (!+ (!expt (!+ line-penalty badness) 2) (expt penalty 2)))
	((and (numberp penalty) (< penalty 0))
	 (!+ (!expt (!+ line-penalty badness) 2) (- (expt penalty 2))))
	(t ;; :-infinity
	 (!expt (!+ line-penalty badness) 2))))

(defclass kp-edge (paragraph-edge)
  ((hyphenp :accessor hyphenp)
   (fitness-class :accessor fitness-class)
   ;; #### NOTE: these are only line-local demerits.
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start line-penalty hyphen-penalty explicit-hyphen-penalty
     &allow-other-keys
     &aux (stop (stop (boundary (node edge))))
	  (hyphenp (not (word-stop-p lineup stop)))
	  (scale (lineup-scale lineup start stop width))
	  (badness (badness lineup start stop width))
	  (penalty (if hyphenp
		     (if (pre-break (aref lineup (1- stop)))
		       hyphen-penalty
		       explicit-hyphen-penalty)
		       0)))
  (assert (not (eq penalty :+infinity)))
  ;; #### WARNING: it is possible to get a rigid line here (scale = NIL), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf (hyphenp edge) hyphenp)
  (setf (fitness-class edge) (scale-fitness-class scale))
  (setf (demerits edge) (local-demerits badness penalty line-penalty)))

(defmethod next-boundaries
    (lineup start width (algorithm (eql :kp))
     &key pass threshold
	  hyphen-penalty explicit-hyphen-penalty
	  emergency-stretch)
  (loop :with boundaries :with overfull :with emergency-boundary
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop boundary))
	:while (and boundary (not overfull))
	:for boundary-type
	  := (cond ((word-boundary-p lineup boundary)
		    :word)
		   ((pre-break (aref lineup (1- (stop boundary))))
		    :hyphen)
		   (t
		    :explicit-hyphen))
	:for min-width := (lineup-min-width lineup start (stop boundary))
	:when (or (eq boundary-type :word)
		  (and (> pass 1)
		       (or (and (eq boundary-type :hyphen)
				(!< hyphen-penalty :+infinity))
			   (and (eq boundary-type :explicit-hyphen)
				(!< explicit-hyphen-penalty :+infinity)))))
	  :if (> min-width width)
	    :do (setq overfull boundary)
	  :else :if (or (and (eq boundary-type :hyphen)
			     (eq hyphen-penalty :-infinity))
			(and (eq boundary-type :explicit-hyphen)
			     (eq explicit-hyphen-penalty :-infinity)))
	    :do (return (list boundary))
	  :else :if (!<= (badness lineup start (stop boundary) width
				  emergency-stretch)
			 threshold)
	    :do (push boundary boundaries)
	  :else
	    :do (setq emergency-boundary boundary)
	:finally (return (if boundaries
			   boundaries
			   ;; #### NOTE: we absolutely need to return
			   ;; something here even on pass 2, because the
			   ;; emergency stretch could be 0.
			   (when (> pass 1)
			     (if overfull (list overfull)
				 (list emergency-boundary)))))))


(defclass kp-layout (paragraph-layout)
  ((size :accessor size)
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((layout kp-layout)  &key &aux (edge (car (edges layout))))
  (setf (size layout) 1
	(demerits layout) (demerits edge)))

(defmethod update-paragraph-layout ((layout kp-layout) (edge kp-edge))
  (incf (size layout))
  (setf (demerits layout) (!+ (demerits layout) (demerits edge))))

(defun kp-postprocess-layout
    (layout adjacent-demerits double-hyphen-demerits final-hyphen-demerits)
  (when (> (length (edges layout)) 1)
    (loop :for edge1 :in (edges layout)
	  :for edge2 :in (cdr (edges layout))
	  :when (and (hyphenp edge1) (hyphenp edge2))
	    :do (setf (demerits layout)
		      (!+ (demerits layout) double-hyphen-demerits))
	  :when (> (abs (- (fitness-class edge1) (fitness-class edge2))) 1)
	    :do (setf (demerits layout)
		      (!+ (demerits layout) adjacent-demerits))))
  (when (hyphenp (nth (- (size layout) 2) (edges layout)))
    (setf (demerits layout) (!+ final-hyphen-demerits (demerits layout)))))

(defun kp-create-layout-lines
    (lineup width disposition layout
     &aux (justified (eq (disposition-type disposition) :justified))
	  (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  (loop :for edge :in (edges layout)
	:and start := 0 :then (next-start (boundary (node edge)))
	:for stop := (stop (boundary (node edge)))
	:if justified
	  :collect (create-justified-line lineup start stop width sloppy)
	:else
	  :collect (create-line lineup start stop)))

(defun kp-graph-create-lines
    (lineup width disposition
     line-penalty hyphen-penalty explicit-hyphen-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness)
  (let* ((graph (or (when (!<= 0 pre-tolerance)
		      (paragraph-graph lineup width :kp
			:pass 1 :threshold pre-tolerance
			:line-penalty line-penalty))
		    (paragraph-graph lineup width :kp
		      :pass 2 :threshold tolerance
		      :line-penalty line-penalty
		      :hyphen-penalty hyphen-penalty
		      :explicit-hyphen-penalty explicit-hyphen-penalty)))
	 (layouts (paragraph-layouts graph :kp)))
    (mapc (lambda (layout)
	    (kp-postprocess-layout layout
	      adjacent-demerits double-hyphen-demerits
	      final-hyphen-demerits))
      layouts)
    (setq layouts (sort layouts #'!< :key #'demerits))
    (when (and (not (zerop emergency-stretch))
	       (eql (demerits (car layouts)) :+infinity))
      (setq graph (paragraph-graph lineup width :kp
		    :pass 3 :threshold tolerance
		    :line-penalty line-penalty
		    :hyphen-penalty hyphen-penalty
		    :explicit-hyphen-penalty explicit-hyphen-penalty
		    :emergency-stretch emergency-stretch))
      (setq layouts (paragraph-layouts graph :kp))
      (mapc (lambda (layout)
	      (kp-postprocess-layout layout
		adjacent-demerits double-hyphen-demerits
		final-hyphen-demerits))
	layouts)
      (setq layouts (sort layouts #'!< :key #'demerits)))
    (unless (zerop looseness)
      (let ((ideal-size (+ (size (car layouts)) looseness)))
	(setq layouts (sort layouts (lambda (size1 size2)
				      (< (abs (- size1 ideal-size))
					 (abs (- size2 ideal-size))))
			    :key #'size))))
    (kp-create-layout-lines lineup width disposition (car layouts))))


(defstruct (kp-node (:constructor kp-make-node))
  boundary demerits previous)

(defun kp-try-boundary (boundary nodes
			lineup width pass threshold line-penalty break-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits emergency-stretch)
  (let (last-deactivated-node new-nodes)
    (maphash
     (lambda (key node
	      &aux (previous-boundary (kp-node-boundary node))
		   (previous-line (car key))
		   (previous-fitness-class (cdr key)))
       (let ((scale (lineup-scale lineup (next-start previous-boundary)
				  (stop boundary) width emergency-stretch)))
	 (when (or (!< scale -1)
		   (eq break-penalty :-infinity)
		   ;; #### WARNING: we must deactivate all nodes when we reach
		   ;; the paragraph's end. TeX does this by adding a forced
		   ;; break at the end.
		   (= (stop boundary) (length lineup)))
	   (setq last-deactivated-node (cons key node))
	   (remhash key nodes))
	 (when (!<= -1 scale)
	   (let ((badness (scale-badness scale)))
	     (when (!<= badness threshold)
	       (let ((fitness-class (scale-fitness-class scale))
		     (demerits
		       (local-demerits badness break-penalty line-penalty)))
		 (when (> (abs (- fitness-class previous-fitness-class)) 1)
		   (setq demerits (!+ demerits adjacent-demerits)))
		 (when (not (word-boundary-p lineup previous-boundary))
		   (if (not (word-boundary-p lineup boundary))
		     (setq demerits (!+ demerits double-hyphen-demerits))
		     ;; #### WARNING: final hyphen demerits are added here
		     ;; although they really concern the previous node. TeX
		     ;; does it like this but I don't really understand how
		     ;; that's conformant with the dynamic programming
		     ;; principle. The code is at #859, but it is confusing
		     ;; because TeX considers the end of a paragraph as
		     ;; hyphenated, which is explained at #829 :-/.
		     (when (= (stop boundary) (length lineup))
		       (setq demerits (!+ demerits final-hyphen-demerits)))))
		 (setq demerits (!+ demerits (kp-node-demerits node)))
		 (let ((previous
			 (find-if (lambda (key)
				    (and (= (car key) (1+ previous-line))
					 (= (cdr key) fitness-class)))
				  new-nodes
				  :key #'car)))
		   (if previous
		     (when (!<= demerits (kp-node-demerits (cdr previous)))
		       (setf (kp-node-demerits (cdr previous)) demerits
			     (kp-node-previous (cdr previous)) node))
		     (push (cons (cons (1+ previous-line) fitness-class)
				 (kp-make-node :boundary boundary
					       :demerits demerits
					       :previous node))
			   new-nodes)))))))))
     nodes)
    (when (and (> pass 1) (zerop (hash-table-count nodes)) (null new-nodes))
      (setq new-nodes
	    (list
	     (cons (cons (1+ (caar last-deactivated-node))
			 (cdar last-deactivated-node))
		   (kp-make-node :boundary boundary
				 :demerits (kp-node-demerits
					    (cdr last-deactivated-node))
				 :previous (cdr last-deactivated-node))))))
    (mapc (lambda (new-node)
	    (setf (gethash (car new-node) nodes) (cdr new-node)))
      new-nodes)))

(defun kp-create-nodes (lineup width pass threshold
			line-penalty hyphen-penalty explicit-hyphen-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits &optional emergency-stretch)
  (let ((nodes (make-hash-table :test #'equal))) ;; key = (line . fitness)
    (setf (gethash '(0 . 1) nodes)
	  (kp-make-node :boundary (make-boundary 0 0) :demerits 0))
    (loop :for boundary := (next-boundary lineup)
	    :then (next-boundary lineup (stop boundary))
	  :while boundary
	  :for boundary-type
	    := (cond ((word-boundary-p lineup boundary) :word)
		     ((pre-break (aref lineup (1- (stop boundary)))) :hyphen)
		     (t :explicit-hyphen))
	  :for break-penalty := (ecase boundary-type
				  (:word 0)
				  (:hyphen hyphen-penalty)
				  (:explicit-hyphen explicit-hyphen-penalty))
	  :when (or (eq boundary-type :word)
		    (and (> pass 1) (!< break-penalty :+infinity)))
	    :do (kp-try-boundary boundary nodes
		  lineup width pass threshold line-penalty break-penalty
		  adjacent-demerits double-hyphen-demerits
		  final-hyphen-demerits emergency-stretch))
    (unless (zerop (hash-table-count nodes)) nodes)))

(defun kp-dynamic-create-lines
    (lineup width disposition
     line-penalty hyphen-penalty explicit-hyphen-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness
     &aux (justified (eq (disposition-type disposition) :justified))
	  (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  (let ((nodes (or (when (!<= 0 pre-tolerance)
		     (kp-create-nodes lineup width 1 pre-tolerance
		       line-penalty hyphen-penalty explicit-hyphen-penalty
		       adjacent-demerits double-hyphen-demerits
		       final-hyphen-demerits))
		   (kp-create-nodes lineup width 2 tolerance
		     line-penalty hyphen-penalty explicit-hyphen-penalty
		     adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits))))
    (when (and (not (zerop emergency-stretch))
	       (loop :for node :being :the :hash-values :in nodes
		     :when (numberp (kp-node-demerits node))
		       :do (return nil)
		     :finally (return t)))
      (setq nodes (kp-create-nodes lineup width 3 tolerance
		     line-penalty hyphen-penalty explicit-hyphen-penalty
		     adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits emergency-stretch)))
    (let ((best (loop :with demerits := :+infinity :with best :with last
		      :for node :being :the :hash-values :in nodes
			:using (hash-key key)
		      :do (setq last (cons node (car key)))
		      :when (!< (kp-node-demerits node) demerits)
			:do (setq best (cons node (car key))
				  demerits (kp-node-demerits node))
		      :finally (return (or best last)))))
      (if (zerop looseness)
	(setq best (car best))
	(setq best (loop :with ideal-size := (+ (cdr best) looseness)
			 :with closer := (car best)
			 :with delta := (abs (- ideal-size (cdr best)))
			 :for node :being :the :hash-values :in nodes
			   :using (hash-key key)
			 :if (< (abs (- ideal-size (car key))) delta)
			   :do (setq closer node
				     delta (abs (- ideal-size (car key))))
			 :else :if (and (= (abs (- ideal-size (car key)))
					   delta)
					(< (kp-node-demerits node)
					   (kp-node-demerits closer)))
			   :do (setq closer node)
			 :finally (return closer))))
      (loop :with lines
	    :for end := best :then (kp-node-previous end)
	    :for beg := (kp-node-previous end)
	    :while beg
	    :for start := (next-start (kp-node-boundary beg))
	    :for stop := (stop (kp-node-boundary end))
	    :do (push
		 (if justified
		   (create-justified-line lineup start stop width sloppy)
		   (create-line lineup start stop))
		 lines)
	    :finally (return lines)))))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass))
     &key variant
	  line-penalty hyphen-penalty explicit-hyphen-penalty
	  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
	  pre-tolerance tolerance emergency-stretch looseness)
  (kp-default variant)
  (calibrate-kp line-penalty)
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  ;; #### FIXME: why are these treated in a special way ??
  (when (>= pre-tolerance (caliber-max *kp-pre-tolerance*))
    (setq pre-tolerance :+infinity))
  (cond ((>= tolerance (caliber-max *kp-tolerance*))
	 (setq tolerance :+infinity))
	((< tolerance (caliber-min *kp-tolerance*))
	 (setq tolerance (caliber-min *kp-tolerance*))))
  (calibrate-kp emergency-stretch)
  (calibrate-kp looseness)
  (ecase variant
    (:graph
     (kp-graph-create-lines lineup width disposition
       line-penalty hyphen-penalty explicit-hyphen-penalty
       adjacent-demerits double-hyphen-demerits final-hyphen-demerits
       pre-tolerance tolerance emergency-stretch looseness))
    (:dynamic
     (kp-dynamic-create-lines lineup width disposition
       line-penalty hyphen-penalty explicit-hyphen-penalty
       adjacent-demerits double-hyphen-demerits final-hyphen-demerits
       pre-tolerance tolerance emergency-stretch looseness))))
