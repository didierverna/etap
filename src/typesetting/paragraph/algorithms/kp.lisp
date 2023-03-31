;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

;; =============
;; Specification
;; =============

(defparameter *kp-variants*
  '(:graph :dynamic))

(defparameter *kp-variants-help-keys*
  '(:kp-variant-graph :kp-variant-dynamic))


(defmacro define-kp-caliber (name min default max)
  "Define a NAMEd Knuth-Plass caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber kp ,name ,min ,default ,max))

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



;; =========
;; Utilities
;; =========

(defun scale-fitness-class (scale)
  (cond ((or (<< scale -1/2) (== scale +∞)) 3)
	((<= -1/2 scale 1/2) 2)
	((<= 1/2 scale 1) 1)
	((< 1 scale) 0)))

(defun local-demerits (badness penalty line-penalty)
  (cond ((and (numberp penalty) (<= 0 penalty))
	 (++ (^^ (++ line-penalty badness) 2) (expt penalty 2)))
	((and (numberp penalty) (< penalty 0))
	 (++ (^^ (++ line-penalty badness) 2) (- (expt penalty 2))))
	(t ;; -∞
	 (^^ (++ line-penalty badness) 2))))



;; ====================
;; Graph Implementation
;; ====================

;; -----
;; Edges
;; -----

(defclass kp-edge (edge)
  ((hyphenp :accessor hyphenp)
   (scale :accessor scale)
   (fitness-class :accessor fitness-class)
   ;; #### NOTE: these are only line-local demerits.
   (demerits :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup start width line-penalty
     &aux (stop (stop-idx (boundary (destination edge)))))
  ;; #### WARNING: it is possible to get a rigid line here (scale = NIL), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf (hyphenp edge)
	(hyphenation-point-p (item (boundary (destination edge)))))
  (setf (scale edge) (lineup-scale lineup start stop width))
  (setf (fitness-class edge) (scale-fitness-class (scale edge)))
  (setf (demerits edge)
	(local-demerits (scale-badness (scale edge))
			(penalty (item (boundary (destination edge))))
			line-penalty)))


;; -------
;; Layouts
;; -------

(defclass kp-layout (layout)
  ((size :accessor size)
   (demerits :accessor demerits)))

(defmethod initialize-instance :after ((layout kp-layout)  &key edge)
  (setf (size layout) 1
	(demerits layout) (demerits edge)))

(defmethod push-edge :after (edge (layout kp-layout))
  (incf (size layout))
  (setf (demerits layout) (++ (demerits layout) (demerits edge))))

(defun kp-postprocess-layout
    (layout adjacent-demerits double-hyphen-demerits final-hyphen-demerits)
  (when (> (length (edges layout)) 1)
    (loop :for edge1 :in (edges layout)
	  :for edge2 :in (cdr (edges layout))
	  :when (and (hyphenp edge1) (hyphenp edge2))
	    :do (setf (demerits layout)
		      (++ (demerits layout) double-hyphen-demerits))
	  :when (> (abs (- (fitness-class edge1) (fitness-class edge2))) 1)
	    :do (setf (demerits layout)
		      (++ (demerits layout) adjacent-demerits)))
    (when (hyphenp (nth (- (size layout) 2) (edges layout)))
      (setf (demerits layout) (++ final-hyphen-demerits (demerits layout))))))


;; ---------
;; Algorithm
;; ---------

(defun kp-make-layout-lines
    (lineup disposition layout
     &aux (justified (eq (disposition-type disposition) :justified))
	  (overstretch
	   (cadr (member :overstretch (disposition-options disposition))))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  (loop :for edge :in (edges layout)
	:and start := 0 :then (start-idx (boundary (destination edge)))
	:for stop := (stop-idx (boundary (destination edge)))
	:if justified
	  :collect (make-scaled-line lineup start stop (scale edge)
				     overshrink overstretch)
	:else
	  :collect (make-line lineup start stop)))

(defun kp-next-boundaries
    (lineup start width
     &key pass threshold emergency-stretch)
  (loop :with boundaries :with overfull :with emergency-boundary
	:with continue := t
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	:while continue
	:for min-width := (lineup-min-width lineup start (stop-idx boundary))
	:do (when (and (<< (penalty (item boundary)) +∞)
		       (or (not (hyphenation-point-p (item boundary)))
			   (> pass 1) ))
	      (when (eq (penalty (item boundary)) -∞) (setq continue nil))
	      (cond ((> min-width width)
		     (setq overfull boundary continue nil))
		    ((<== (badness lineup start (stop-idx boundary) width
				   emergency-stretch)
			  threshold)
		     (push boundary boundaries))
		    (t
		     (setq emergency-boundary boundary))))
	:finally (return (if boundaries
			   boundaries
			   ;; #### NOTE: we absolutely need to return
			   ;; something here even on pass 2, because the
			   ;; emergency stretch could be 0.
			   (when (> pass 1)
			     (if overfull (list overfull)
				 (list emergency-boundary)))))))

(defun kp-graph-make-lines
    (lineup disposition width line-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness)
  (let* ((graph (or (when (<== 0 pre-tolerance)
		      (make-graph lineup width
			:edge-type `(kp-edge :line-penalty ,line-penalty)
			:next-boundaries #'kp-next-boundaries
			:pass 1 :threshold pre-tolerance))
		    (make-graph lineup width
		      :edge-type `(kp-edge :line-penalty ,line-penalty)
		      :next-boundaries #'kp-next-boundaries
		      :pass 2 :threshold tolerance)))
	 (layouts (layouts graph 'kp-layout)))
    (mapc (lambda (layout)
	    (kp-postprocess-layout layout
	      adjacent-demerits double-hyphen-demerits
	      final-hyphen-demerits))
      layouts)
    (setq layouts (sort layouts #'<< :key #'demerits))
    (when (and (not (zerop emergency-stretch))
	       (eql (demerits (car layouts)) +∞))
      (setq graph (make-graph lineup width
		    :edge-type `(kp-edge :line-penalty ,line-penalty)
		    :next-boundaries #'kp-next-boundaries
		    :pass 3 :threshold tolerance
		    :emergency-stretch emergency-stretch))
      (setq layouts (layouts graph 'kp-layout))
      (mapc (lambda (layout)
	      (kp-postprocess-layout layout
		adjacent-demerits double-hyphen-demerits
		final-hyphen-demerits))
	layouts)
      (setq layouts (sort layouts #'<< :key #'demerits)))
    (unless (zerop looseness)
      (let ((ideal-size (+ (size (car layouts)) looseness)))
	(setq layouts (sort layouts (lambda (size1 size2)
				      (< (abs (- size1 ideal-size))
					 (abs (- size2 ideal-size))))
			:key #'size))))
    (kp-make-layout-lines lineup disposition (car layouts))))



;; ==================================
;; Dynamic Programming Implementation
;; ==================================

(defstruct (kp-node (:constructor kp-make-node))
  boundary demerits previous)

;; The active nodes hash table is accessed by
;; key = (boundary line-number fitness-class)
(defun make-key (boundary line fitness) (list boundary line fitness))
(defun key-boundary (key) (first key))
(defun key-line (key) (second key))
(defun key-fitness (key) (third key))

(defun kp-try-boundary (boundary nodes
			lineup width pass threshold line-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits emergency-stretch)
  (let (last-deactivated-node new-nodes)
    (maphash
     (lambda (key node
	      &aux (previous-boundary (key-boundary key))
		   (previous-line (key-line key))
		   (previous-fitness (key-fitness key))
		   (scale (lineup-scale lineup
					(start-idx previous-boundary)
					(stop-idx boundary)
					width emergency-stretch)))
       (when (or (<< scale -1)
		 (eq (penalty (item boundary)) -∞)
		 ;; #### WARNING: we must deactivate all nodes when we reach
		 ;; the paragraph's end. TeX does this by adding a forced
		 ;; break at the end.
		 (last-boundary-p boundary))
	 (setq last-deactivated-node (cons key node))
	 (remhash key nodes))
       (when (<== -1 scale)
	 (let ((badness (scale-badness scale)))
	   (when (<== badness threshold)
	     (let ((fitness (scale-fitness-class scale))
		   (demerits (local-demerits badness (penalty (item boundary))
					     line-penalty)))
	       (when (and (>= previous-fitness 0) ;; not first line
			  (> (abs (- fitness previous-fitness)) 1))
		 (setq demerits (++ demerits adjacent-demerits)))
	       (when (discretionaryp (item previous-boundary))
		 (if (discretionaryp (item boundary))
		   (setq demerits (++ demerits double-hyphen-demerits))
		   ;; #### WARNING: final hyphen demerits are added here
		   ;; although they really concern the previous node. TeX does
		   ;; it like this but I don't really understand how that's
		   ;; conformant with the dynamic programming principle. The
		   ;; code is at #859, but it is confusing because TeX
		   ;; considers the end of a paragraph as hyphenated, which is
		   ;; explained at #829 :-/.
		   (when (last-boundary-p boundary)
		     (setq demerits (++ demerits final-hyphen-demerits)))))
	       (setq demerits (++ demerits (kp-node-demerits node)))
	       (let* ((new-key (make-key boundary (1+ previous-line) fitness))
		      (previous (find new-key new-nodes
				  :test #'equal :key #'car)))
		 (if previous
		   (when (<== demerits (kp-node-demerits (cdr previous)))
		     (setf (kp-node-demerits (cdr previous)) demerits
			   (kp-node-previous (cdr previous)) node))
		   (push (cons new-key
			       (kp-make-node :boundary boundary
					     :demerits demerits
					     :previous node))
			 new-nodes))))))))
     nodes)
    ;; #### FIXME: review this carefully wrt what TeX does.
    (when (and (> pass 1) (zerop (hash-table-count nodes)) (null new-nodes))
      (setq new-nodes
	    (list
	     (cons (make-key boundary
			     (1+ (key-line (car last-deactivated-node)))
			     (key-fitness (car last-deactivated-node)))
		   (kp-make-node :boundary boundary
				 :demerits (kp-node-demerits
					    (cdr last-deactivated-node))
				 :previous (cdr last-deactivated-node))))))
    (mapc (lambda (new-node)
	    (setf (gethash (car new-node) nodes) (cdr new-node)))
      new-nodes)))

(defun kp-create-nodes (lineup width pass threshold line-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits &optional emergency-stretch)
  ;; #### WARNING: the root node / boundary are fake because they don't really
  ;; represent a line ending, but there are some requirements on them in order
  ;; to KP-TRY-BOUNDARY above to work correctly when trying out the very first
  ;; line.
  ;; - The fake root boundary has a null ITEM (making DISCRETIONARYP return
  ;;   NIL, essentially telling that we don't have previous hyphenation; so no
  ;;   double hyphen demerits), and a START-IDX of 0, which is indeed the
  ;;   case.
  ;; - The fake root node has a (previous) line number of 0, which is correct,
  ;;   and a fitness class of -1, meaning no previous fitness class; so no
  ;;   adjacent demerits.
  (let ((root-boundary (make-instance 'boundary :item nil :start-idx 0))
	(nodes (make-hash-table :test #'equal)))
    (setf (gethash (make-key root-boundary 0 -1) nodes)
	  (kp-make-node :boundary root-boundary :demerits 0))
    (loop :for boundary := (next-boundary lineup 0)
	    :then (next-boundary lineup (stop-idx boundary))
	  :while boundary
	  :when (and (<< (penalty (item boundary)) +∞)
		     (or (not (hyphenation-point-p (item boundary)))
			 (> pass 1)))
	    :do (kp-try-boundary boundary nodes
		  lineup width pass threshold line-penalty
		  adjacent-demerits double-hyphen-demerits
		  final-hyphen-demerits emergency-stretch))
    (unless (zerop (hash-table-count nodes)) nodes)))

;; #### FIXME: this function probably needs to go away.
(defun make-wide-line
    (lineup start stop width &optional overshrink overstretch)
  "Make a line of WIDTH from LINEUP chunk between START and STOP.
If no elasticity is available, the line will remain at its normal width.
If some elasticity is available, get as close as possible to WIDTH within the
limits of the available elasticity.
If OVERSTRETCH, disregard the limit and stretch as much needed.
If OVERSHRINK, disregard the limit and shrink as much needed."
  (make-scaled-line lineup start stop (lineup-scale lineup start stop width)
		    overshrink overstretch))

(defun kp-dynamic-make-lines
    (lineup disposition width line-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness
     &aux (justified (eq (disposition-type disposition) :justified))
	  (overstretch
	   (cadr (member :overstretch (disposition-options disposition))))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  (let ((nodes (or (when (<== 0 pre-tolerance)
		     (kp-create-nodes lineup width 1 pre-tolerance
		       line-penalty adjacent-demerits double-hyphen-demerits
		       final-hyphen-demerits))
		   (kp-create-nodes lineup width 2 tolerance
		     line-penalty adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits))))
    (when (and (not (zerop emergency-stretch))
	       (loop :for node :being :the :hash-values :in nodes
		     :when (numberp (kp-node-demerits node))
		       :do (return nil)
		     :finally (return t)))
      (setq nodes (kp-create-nodes lineup width 3 tolerance
		     line-penalty adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits emergency-stretch)))
    (let ((best (loop :with demerits := +∞ :with best :with last
		      :for node :being :the :hash-values :in nodes
			:using (hash-key key)
		      :do (setq last (cons node (car key)))
		      :when (<< (kp-node-demerits node) demerits)
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
	    :for start := (start-idx (kp-node-boundary beg))
	    :for stop := (stop-idx (kp-node-boundary end))
	    :do (push
		 (if justified
		   (make-wide-line lineup start stop width
				   overshrink overstretch)
		   (make-line lineup start stop))
		 lines)
	    :finally (return lines)))))



;; ===========
;; Entry Point
;; ===========

(defmacro calibrate-kp (name &optional infinity)
  "Calibrate NAMEd Knuth-Plass variable."
  `(calibrate kp ,name ,infinity))

(defmacro default-kp (name)
  "Default Knuth-Plass NAMEd variable."
  `(default kp ,name))

(defmethod make-lines :around
    (lineup disposition width (algorithm (eql :knuth-plass))
     &key hyphen-penalty explicit-hyphen-penalty)
  "Apply hyphen penalties and add a final glue to the lineup."
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
  (mapc (lambda (item)
	  (when (hyphenation-point-p item)
	    (setf (penalty item)
		  (if (explicitp item)
		    explicit-hyphen-penalty
		    hyphen-penalty))))
    lineup)
  (endpush (make-glue :stretch +∞ :penalty +∞) lineup)
  (call-next-method))

(defmethod make-lines
    (lineup disposition width (algorithm (eql :knuth-plass))
     &key variant line-penalty
	  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
	  pre-tolerance tolerance emergency-stretch looseness)
  (default-kp variant)
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  ;; #### FIXME: I can't remember why these are treated in a special way
  ;; (maybe because we handle infinity only on the positive side or
  ;; something). This needs to be reviewed.
  (when (null pre-tolerance)
    (setq pre-tolerance (caliber-default *kp-pre-tolerance*)))
  (when (>= pre-tolerance (caliber-max *kp-pre-tolerance*))
    (setq pre-tolerance +∞))
  (when (null tolerance)
    (setq tolerance (caliber-default *kp-tolerance*)))
  (cond ((>= tolerance (caliber-max *kp-tolerance*))
	 (setq tolerance +∞))
	((< tolerance (caliber-min *kp-tolerance*))
	 (setq tolerance (caliber-min *kp-tolerance*))))
  (calibrate-kp emergency-stretch)
  (calibrate-kp looseness)
  (ecase variant
    (:graph
     (kp-graph-make-lines lineup disposition width line-penalty
       adjacent-demerits double-hyphen-demerits final-hyphen-demerits
       pre-tolerance tolerance emergency-stretch looseness))
    (:dynamic
     (kp-dynamic-make-lines lineup disposition width line-penalty
       adjacent-demerits double-hyphen-demerits final-hyphen-demerits
       pre-tolerance tolerance emergency-stretch looseness))))
