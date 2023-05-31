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
  (cond ((<< scale -1/2) 3)
	((<< 1 scale) 0)
	((<= -1/2 scale 1/2) 2)
	(t 1)))

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
  ;; See warning in KP-CREATE-NODES about that.
  (when (= (fitness-class (first (edges layout))) 0)
    (setf (demerits layout) (++ (demerits layout) adjacent-demerits)))
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
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  (loop :for edge :in (edges layout)
	:and start := 0 :then (start-idx (boundary (destination edge)))
	:for stop := (stop-idx (boundary (destination edge)))
	:if justified
	  :collect (make-scaled-line lineup start stop (scale edge)
				     ;; #### NOTE: the overstretch option is
				     ;; always T because TeX has its own
				     ;; tolerance settings which are not
				     ;; necessarily 1.
				     overshrink t)
	:else
	  :collect (make-line lineup start stop)))

(defun kp-next-boundaries
    (lineup start width
     &key hyphenate threshold final
     &aux (emergency-stretch (when (numberp final) final)))
  "Knuth-Plass graph implementation version of `next-boundaries'.
See `kp-create-nodes' for the semantics of HYPHENATE and FINAL."
  (loop :with boundaries :with overfull :with emergency-boundary
	:with continue := t
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	:while continue
	:for min-width := (lineup-min-width lineup start (stop-idx boundary))
	:do (when (and (<< (penalty (item boundary)) +∞)
		       (or (not (hyphenation-point-p (item boundary)))
			   hyphenate ))
	      (when (eq (penalty (item boundary)) -∞) (setq continue nil))
	      (cond ((> min-width width)
		     (setq overfull boundary continue nil))
		    ((<== (scale-badness
			   (lineup-scale lineup start (stop-idx boundary)
					 width emergency-stretch))
			  threshold)
		     (push boundary boundaries))
		    (t
		     (setq emergency-boundary boundary))))
	:finally (return (or boundaries
			     (when final
			       (if overfull
				 (list overfull)
				 (list emergency-boundary)))))))

(defun kp-graph-make-lines
    (lineup disposition width line-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness)
  "Typeset LINEUP with the Knuth-Plass algorithm, graph version."
  (let* ((graph (or (when (<== 0 pre-tolerance)
		      (make-graph lineup width
			:edge-type `(kp-edge :line-penalty ,line-penalty)
			:next-boundaries #'kp-next-boundaries
			:threshold pre-tolerance))
		    (make-graph lineup width
		      :edge-type `(kp-edge :line-penalty ,line-penalty)
		      :next-boundaries #'kp-next-boundaries
		      :hyphenate t :threshold tolerance
		      :final (zerop emergency-stretch))
		    (make-graph lineup width
		      :edge-type `(kp-edge :line-penalty ,line-penalty)
		      :next-boundaries #'kp-next-boundaries
		      :hyphenate t :threshold tolerance
		      :final emergency-stretch)))
	 (layouts (layouts graph 'kp-layout)))
    (mapc (lambda (layout)
	    (kp-postprocess-layout layout
	      adjacent-demerits double-hyphen-demerits
	      final-hyphen-demerits))
      layouts)
    (setq layouts (sort layouts #'<< :key #'demerits))
    (unless (zerop looseness)
      (let ((ideal-size (+ (size (car layouts)) looseness)))
	(setq layouts (sort layouts (lambda (size1 size2)
				      (< (abs (- size1 ideal-size))
					 (abs (- size2 ideal-size))))
			:key #'size))))
    ;; #### WARNING: by choosing the first layout here, we're doing the
    ;; opposite of what TeX does in case of total demerits equality. We could
    ;; instead check for multiple such layouts and take the last one. On the
    ;; other hand, while we're using a hash table in the dynamic programming
    ;; implementation, we're not doing exactly what TeX does either, so
    ;; there's no rush. It's still important to keep that in mind however,
    ;; because that explains while we may end up with different solutions
    ;; between the graph and the dynamic versions.
    (kp-make-layout-lines lineup disposition (car layouts))))



;; ==================================
;; Dynamic Programming Implementation
;; ==================================

(defstruct (kp-node (:constructor kp-make-node))
  boundary scale demerits previous)

;; The active nodes hash table is accessed by
;; key = (boundary line-number fitness-class)
(defun make-key (boundary line fitness) (list boundary line fitness))
(defun key-boundary (key) (first key))
(defun key-line (key) (second key))
(defun key-fitness (key) (third key))

(defun kp-try-boundary (boundary nodes
			lineup width threshold line-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits final
			&aux (emergency-stretch (when (numberp final) final)))
  "Examine BOUNDARY and update active NODES accordingly."
  (let (last-deactivated-node new-nodes)
    (maphash
     (lambda (key node
	      &aux (previous-boundary (key-boundary key))
		   (previous-line (key-line key))
		   (previous-fitness (key-fitness key))
		   (scale (lineup-scale lineup
					(start-idx previous-boundary)
					(stop-idx boundary)
					width)))
       (when (or (<< scale -1)
		 (eq (penalty (item boundary)) -∞)
		 ;; #### WARNING: we must deactivate all nodes when we reach
		 ;; the paragraph's end. TeX does this by adding a forced
		 ;; break at the end.
		 (last-boundary-p boundary))
	 (setq last-deactivated-node (cons key node))
	 (remhash key nodes))
       (when (<== -1 scale)
	 (let ((badness (scale-badness
			 (if emergency-stretch
			   (lineup-scale lineup
					 (start-idx previous-boundary)
					 (stop-idx boundary)
					 width emergency-stretch)
			   scale))))
	   (when (<== badness threshold)
	     (let ((fitness (scale-fitness-class scale))
		   (demerits (local-demerits badness (penalty (item boundary))
					     line-penalty)))
	       (when (> (abs (- fitness previous-fitness)) 1)
		 (setq demerits (++ demerits adjacent-demerits)))
	       ;; #### NOTE: according to #859, TeX doesn't consider the
	       ;; admittedly very rare and weird case where a paragraph would
	       ;; end with an explicit hyphen. As stipulated in #829, for the
	       ;; purpose of computing demerits, the end of the paragraph is
	       ;; always regarded as virtually hyphenated, and in case the
	       ;; previous line (really) is hyphenated, the value of
	       ;; final-hyphen-demerits is used instead of
	       ;; double-hyphen-demerits. One could consider using
	       ;; double-hyphen-demerits when there actually is a final
	       ;; hyphen, but on the other hand, the final line is rarely
	       ;; justified so the two hyphens are very unlikely to create a
	       ;; ladder.
	       (when (discretionaryp (item previous-boundary))
		 (if (last-boundary-p boundary)
		   (setq demerits (++ demerits final-hyphen-demerits))
		   (when (discretionaryp (item boundary))
		     (setq demerits (++ demerits double-hyphen-demerits)))))
	       (setq demerits (++ demerits (kp-node-demerits node)))
	       (let* ((new-key (make-key boundary (1+ previous-line) fitness))
		      (previous (find new-key new-nodes
				  :test #'equal :key #'car)))
		 (if previous
		   ;; #### NOTE: the inclusive inequality below is conformant
		   ;; with what TeX does in #855. Concretely, it makes the KP
		   ;; algorithm greedy in some sense: in case of demerits
		   ;; equality, TeX keeps the last envisioned solution. On the
		   ;; other hand, we're in fact not doing exactly the same
		   ;; thing because we're using MAPHASH and the order of the
		   ;; nodes in the hash table is not deterministic.
		   (when (<== demerits (kp-node-demerits (cdr previous)))
		     (setf (kp-node-scale (cdr previous)) scale
			   (kp-node-demerits (cdr previous)) demerits
			   (kp-node-previous (cdr previous)) node))
		   (push (cons new-key
			       (kp-make-node :boundary boundary
					     :scale scale
					     :demerits demerits
					     :previous node))
			 new-nodes))))))))
     nodes)
    (when (and final (zerop (hash-table-count nodes)) (null new-nodes))
      (setq new-nodes
	    (list
	     (let ((scale (lineup-scale lineup
					(start-idx
					 (key-boundary
					  (car last-deactivated-node)))
					(stop-idx boundary)
					width)))
	       (cons (make-key boundary
			       (1+ (key-line (car last-deactivated-node)))
			       (scale-fitness-class scale))
		     (kp-make-node :boundary boundary
				   :scale scale
				   ;; #### NOTE: in this situation, TeX sets
				   ;; the local demerits to 0 (#855) by
				   ;; checking the artificial_demerits flag.
				   ;; So we just re-use the previous total.
				   :demerits (kp-node-demerits
					      (cdr last-deactivated-node))
				   :previous (cdr last-deactivated-node)))))))
    (mapc (lambda (new-node)
	    (setf (gethash (car new-node) nodes) (cdr new-node)))
      new-nodes)))

(defun kp-create-nodes (lineup width hyphenate threshold line-penalty
			adjacent-demerits double-hyphen-demerits
			final-hyphen-demerits &optional final)
  "Compute the best sequences of breaks for LINEUP in the Knuth-Plass sense.
This function may be called up to three times (corresponding to \"passes\"
through the algorithm in the TeX jargon).
- HYPHENATE means consider hyphenation points as potential breaks. It is NIL
  for pass 1, and T for passes 2 and 3.
- FINAL means this is the final pass (in which case we can't allow to loose
  all nodes). If FINAL is NIL, we're in pass 1. If FINAL is T, we're in pass 2
  and there is no emergency stretch. Otherwise, FINAL is a non-zero value (the
  emergency stretch) and we're in pass 3."
  ;; #### WARNING: the root node / boundary are fake because they don't really
  ;; represent a line ending, but there are some requirements on them in order
  ;; to KP-TRY-BOUNDARY above to work correctly when trying out the very first
  ;; line. The fake root boundary has:
  ;; - a null ITEM (making DISCRETIONARYP return NIL), essentially telling
  ;;   that we don't have previous hyphenation, so no double hyphen demerits.
  ;; - a START-IDX of 0, which is correct.
  ;; - a (previous) line number of 0, which is correct.
  ;; - a fitness class of 2 (decent). TeX computes adjacent demerits even for
  ;;   the first line which doesn't really have a previous line. This has the
  ;;   effect of negatively weighting very loose first lines. See
  ;;   https://tug.org/pipermail/texhax/2023-May/026091.html
  (let ((root-boundary (make-instance 'boundary :item nil :start-idx 0))
	(nodes (make-hash-table :test #'equal)))
    (setf (gethash (make-key root-boundary 0 -1) nodes)
	  (kp-make-node :boundary root-boundary :demerits 0))
    (loop :for boundary := (next-boundary lineup 0)
	    :then (next-boundary lineup (stop-idx boundary))
	  :while boundary
	  :when (and (<< (penalty (item boundary)) +∞)
		     (or (not (hyphenation-point-p (item boundary)))
			 hyphenate))
	    :do (kp-try-boundary boundary nodes
		  lineup width threshold line-penalty
		  adjacent-demerits double-hyphen-demerits
		  final-hyphen-demerits final))
    (unless (zerop (hash-table-count nodes)) nodes)))

(defun kp-dynamic-make-lines
    (lineup disposition width line-penalty
     adjacent-demerits double-hyphen-demerits final-hyphen-demerits
     pre-tolerance tolerance emergency-stretch looseness
     &aux (justified (eq (disposition-type disposition) :justified))
	  (overshrink
	   (cadr (member :overshrink (disposition-options disposition)))))
  "Typeset LINEUP with the Knuth-Plass algorithm, dynamic programming version."
  (let ((nodes (or (when (<== 0 pre-tolerance)
		     (kp-create-nodes lineup width nil pre-tolerance
		       line-penalty adjacent-demerits double-hyphen-demerits
		       final-hyphen-demerits))
		   (kp-create-nodes lineup width t tolerance
		     line-penalty adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits (zerop emergency-stretch))
		   (kp-create-nodes lineup width t tolerance
		     line-penalty adjacent-demerits double-hyphen-demerits
		     final-hyphen-demerits emergency-stretch))))
    (let ((best (loop :with demerits := +∞ :with best :with last
		      :for node :being :the :hash-values :in nodes
			:using (hash-key key)
		      :do (setq last (cons (key-line key) node))
		      :when (<< (kp-node-demerits node) demerits)
			:do (setq best (cons (key-line key) node)
				  demerits (kp-node-demerits node))
		      :finally (return (or best last)))))
      (if (zerop looseness)
	(setq best (cdr best))
	(setq best
	      (loop :with ideal-size := (+ (car best) looseness)
		    :with closer := (cdr best)
		    :with delta := (abs looseness)
		    :for node :being :the :hash-values :in nodes
		      :using (hash-key key)
		    :do (cond ((< (abs (- ideal-size (key-line key))) delta)
			       (setq closer node
				     delta (abs (- ideal-size (key-line key)))))
			      ((and (= (abs (- ideal-size (key-line key)))
				       delta)
				    (< (kp-node-demerits node)
				       (kp-node-demerits closer)))
			       (setq closer node)))
		    :finally (return closer))))
      (loop :with lines
	    :for end := best :then (kp-node-previous end)
	    :for beg := (kp-node-previous end)
	    :while beg
	    :for start := (start-idx (kp-node-boundary beg))
	    :for stop := (stop-idx (kp-node-boundary end))
	    :do (push (if justified
			(make-scaled-line lineup start stop (kp-node-scale end)
					  ;; #### NOTE: the overstretch option
					  ;; is always T because TeX has its
					  ;; own tolerance settings which are
					  ;; not necessarily 1.
					  overshrink t)
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
  "Typeset LINEUP with the Knuth-Plass algorithm."
  (default-kp variant)
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  (calibrate-kp pre-tolerance :positive)
  (calibrate-kp tolerance :positive)
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
