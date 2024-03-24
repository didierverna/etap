;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

;; #### FIXME: my implementation of the 3 passes is probably not correct. It
;; seems that TeX goes into the 2nd pass, not only when the first fails, but
;; also when it succeeds with a non-zero looseness, and the number of lines
;; doesn't match. I need to check this.

(in-package :etap)

;; ==========================================================================
;; Specification
;; ==========================================================================

(defparameter *kp-variants*
  '(:graph :dynamic))

(defparameter *kp-variants-help-keys*
  '(:kp-variant-graph :kp-variant-dynamic))

(defparameter *kp-tooltips*
  '(:kp-variant-graph "Graph-based implementation."
    :kp-variant-dynamic "Dynamic programming implementation."))


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


(define-global-variables variant hyphen-penalty explicit-hyphen-penalty
  line-penalty adjacent-demerits double-hyphen-demerits final-hyphen-demerits
  pre-tolerance tolerance emergency-stretch looseness)


(defmacro calibrate-kp (name &optional infinity)
  "Calibrate NAMEd Knuth-Plass variable."
  `(calibrate kp ,name ,infinity))

(defmacro default-kp (name)
  "Default Knuth-Plass NAMEd variable."
  `(default kp ,name))




;; ==========================================================================
;; HList
;; ==========================================================================

(defmethod process-hlist
    (hlist disposition (algorithm (eql :knuth-plass))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Adjust hyphen penalties in HLIST, and append the final glue to it."
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
  (mapc (lambda (item)
	  (when (hyphenation-point-p item)
	    (setf (penalty item)
		  (if (explicitp item)
		    *explicit-hyphen-penalty*
		    *hyphen-penalty*))))
    hlist)
  (endpush (make-glue :stretch +∞ :penalty +∞) hlist)
  hlist)




;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### WARNING: the logic is ACTUAL-SCALES is to establish scaling
;; tolerances, whereas TeX uses badness tolerances. Hence I need to convert it
;; back (from a float to a ratio), which is not very nice.
(defun stretch-tolerance (badness-tolerance)
  "Return the stretch tolerance corresponding to BADNESS-TOLERANCE."
  ;; #### NOTE: we don't get a negative pre-tolerance here because pass 1
  ;; would be skipped. So we only need to handle the $>= 0 case.
  (if ($= badness-tolerance +∞)
    +∞
    (rationalize (expt (/ badness-tolerance 100) 1/3))))

(defun scale-fitness-class (scale)
  "Return SCALE's fitness class.
This is an integer ranging from 0 (very loose) to 3 (tight)."
  (cond (($< scale -1/2) 3)
	(($< 1 scale) 0)
	((<= -1/2 scale 1/2) 2)
	(t 1)))

(defun fitness-class-name (fitness-class)
  "Return FITNESS-CLASS's name (a string)."
  (ecase fitness-class (3 "tight") (2 "decent") (1 "loose") (0 "very loose")))


;; -------------------
;; Line specialization
;; -------------------

(defclass kp-line (line)
  ((fitness-class :documentation "This line's fitness class."
		  :initarg :fitness-class
		  :reader fitness-class)
   (badness :documentation "This line's badness."
	    :initarg :badness
	    :reader badness)
   (demerits :documentation "This line's local demerits."
	     :initarg :demerits
	     :reader demerits))
  (:documentation "The Knuth-Plass line class."))

(defmethod properties strnlcat ((line kp-line))
  "Advertise LINE's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A.~%Badness: ~A.~%Demerits: ~A."
    (fitness-class-name (fitness-class line))
    ($float (badness line))
    ($float (demerits line))))


;; ----------------------
;; Breakup specialization
;; ----------------------

(defclass kp-breakup-mixin ()
  ((pass :documentation "Which of the 3 passes produced this breakup."
	 :initform 0 :initarg :pass :reader pass))
  (:documentation "The KP-BREAKUP-MIXIN class.
This class is mixed in both the graph and dynamic breakup classes."))

;; #### NOTE: the Knuth-Plass algorithm never refuses to typeset, so a pass of
;; 0 means that the harray was empty.
(defmethod properties strnlcat
    ((mixin kp-breakup-mixin) &aux (pass (pass mixin)))
  "Advertise TeX's algorithm pass number and total demerits."
  (unless (zerop pass) (format nil "Pass: ~A." pass)))




;; ==========================================================================
;; Graph Variant
;; ==========================================================================

;; -----
;; Edges
;; -----

(defclass kp-edge (edge)
  ((scale :documentation "This edge's scale."
	  :reader scale)
   (fitness-class :documentation "This edge's fitness class."
		  :reader fitness-class)
   (badness :documentation "This edge's badness."
	    :reader badness)
   (demerits :documentation "This edge's local demerits."
	     :reader demerits))
  (:documentation "The KP-EDGE class."))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key harray start width
     &aux (stop (stop-idx (boundary (destination edge)))))
  "Initialize EDGE's scale, fitness class, badness, and local demerits."
  ;; #### WARNING: it is possible to get a rigid line here (scale = +/-∞), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf (slot-value edge 'scale) (harray-scale harray start stop width))
  (setf (slot-value edge 'fitness-class) (scale-fitness-class (scale edge)))
  (setf (slot-value edge 'badness) (scale-badness (scale edge)))
  (setf (slot-value edge 'demerits)
	(local-demerits (badness edge)
			(penalty (item (boundary (destination edge))))
			*line-penalty*)))


;; -------
;; Layouts
;; -------

(defclass kp-layout (layout)
  ((size :documentation "This layout's size (i.e. the number of lines)."
	 :accessor size)
   (demerits :documentation "This layout's total demerits."
	     :accessor demerits))
  (:documentation "The KP-LAYOUT class."))

(defmethod initialize-instance :after ((layout kp-layout)  &key edge)
  "Initialize LAYOUT's size to 1 and demerits to its last EDGE's."
  (setf (size layout) 1 (demerits layout) (demerits edge)))

(defmethod properties strnlcat ((layout kp-layout))
  "Advertise Knuth-Plass LAYOUT's demerits."
  (format nil "Demerits: ~A." ($float (demerits layout))))

(defmethod push-edge :after (edge (layout kp-layout))
  "Increase LAYOUT size by 1 and demerits with EDGE's."
  (incf (size layout))
  (setf (demerits layout) ($+ (demerits layout) (demerits edge))))

(defun kp-postprocess-layout (layout)
  "Finish computing LAYOUT's total demerits.
When this function is called, LAYOUT's demerits contain only the sum of each
line's local ones. This function handles the remaining contextual information,
such as hyphen adjacency and fitness class differences between lines."
  ;; See warning in KP-CREATE-NODES about that.
  (when (= (fitness-class (first (edges layout))) 0)
    (setf (demerits layout) ($+ (demerits layout) *adjacent-demerits*)))
  (when (> (length (edges layout)) 1)
    (loop :for edge1 :in (edges layout)
	  :for edge2 :in (cdr (edges layout))
	  :when (and (hyphenated edge1) (hyphenated edge2))
	    :do (setf (demerits layout)
		      ($+ (demerits layout) *double-hyphen-demerits*))
	  :when (> (abs (- (fitness-class edge1) (fitness-class edge2))) 1)
	    :do (setf (demerits layout)
		      ($+ (demerits layout) *adjacent-demerits*)))
    (when (hyphenated (nth (- (size layout) 2) (edges layout)))
      (setf (demerits layout) ($+ *final-hyphen-demerits* (demerits layout))))))


;; ---------------
;; Boundary lookup
;; ---------------

(defun kp-next-boundaries
    (harray start width
     &key hyphenate threshold final
     &aux (emergency-stretch (when (numberp final) final)))
  "Knuth-Plass graph implementation version of `next-boundaries'.
See `kp-create-nodes' for the semantics of HYPHENATE and FINAL."
  (loop :with boundaries :with overfull :with emergency-boundary
	:with continue := t
	:for boundary := (next-boundary harray start)
	  :then (next-boundary harray (stop-idx boundary))
	:while continue
	:for min-width := (harray-min-width harray start (stop-idx boundary))
	:do (when (and ($< (penalty (item boundary)) +∞)
		       (or hyphenate
			   (not (hyphenation-point-p (item boundary)))))
	      (when (eq (penalty (item boundary)) -∞) (setq continue nil))
	      (cond ((> min-width width)
		     (setq overfull boundary continue nil))
		    (($<= (scale-badness
			   (harray-scale harray start (stop-idx boundary)
					 width emergency-stretch))
			  threshold)
		     (push boundary boundaries))
		    (t
		     (setq emergency-boundary boundary))))
	:finally (return (or boundaries
			     (when final
			       (list (or overfull emergency-boundary)))))))


;; -----------------
;; Lines computation
;; -----------------

(defun kp-pin-layout (harray disposition width beds layout pass)
  "Pin Knuth-Plass LAYOUT from HARRAY for a DISPOSITION paragraph."
  (when layout
    (loop :with disposition-options := (disposition-options disposition)
	  ;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce
	  ;; elastic underfulls (in case of an impossible layout, it falls
	  ;; back to overfull boxes). This means that the overstretch option
	  ;; has no effect, but it allows for a nice trick: we can indicate
	  ;; lines exceeding the tolerance thanks to an emergency stretch as
	  ;; overstretched, regardless of the option. This is done by setting
	  ;; the overstretched parameter to T and not counting emergency
	  ;; stretch in the stretch-tolerance one.
	  :with overshrink := (getf disposition-options :overshrink)
	  :with disposition := (disposition-type disposition)
	  :with stretch-tolerance
	    := (stretch-tolerance (if (> pass 1) *tolerance* *pre-tolerance*))
	  :with baseline-skip := (baseline-skip harray)
	  :for y := 0 :then (+ y baseline-skip)
	  :for edge :in (edges layout)
	  :and start := 0 :then (start-idx (boundary (destination edge)))
	  :for stop := (stop-idx (boundary (destination edge)))
	  :for line := (case disposition
			 (:justified
			  (multiple-value-bind (theoretical effective)
			      (actual-scales (scale edge)
				:stretch-tolerance stretch-tolerance
				:overshrink overshrink
				:overstretch t)
			    (make-instance 'kp-line
			      :harray harray :start-idx start :stop-idx stop
			      :beds beds
			      :scale theoretical :effective-scale effective
			      :fitness-class (fitness-class edge)
			      :badness (badness edge)
			      :demerits (demerits edge))))
			 (t ;; just switch back to normal spacing.
			  (make-instance 'line
			    :harray harray :start-idx start :stop-idx stop
			    :beds beds)))
	  :for x := (case disposition
		      ((:flush-left :justified) 0)
		      (:centered (/ (- width (width line)) 2))
		      (:flush-right (- width (width line))))
	  :collect (pin-line line x y))))


;; ----------------------
;; Breakup specialization
;; ----------------------

(defun kp-graph-break-harray (harray disposition width beds)
  "Break HARRAY with the Knuth-Plass algorithm, graph version."
  (if (zerop (length harray))
    (make-instance 'kp-graph-breakup)
    (let ((threshold *pre-tolerance*)
	  (pass 1)
	  graph layouts breakup)
      (when ($<= 0 threshold)
	(setq graph (make-graph harray width
		      :edge-type 'kp-edge
		      :next-boundaries `(kp-next-boundaries
					 :threshold ,threshold))))
      (unless (edges graph)
	(incf pass)
	(setq threshold *tolerance*)
	(setq graph (make-graph harray width
		      :edge-type 'kp-edge
		      :next-boundaries `(kp-next-boundaries
					 :hyphenate t
					 :threshold ,threshold
					 :final ,(zerop *emergency-stretch*)))))
      (unless (edges graph)
	(incf pass)
	(setq graph (make-graph harray width
		      :edge-type 'kp-edge
		      :next-boundaries `(kp-next-boundaries
					 :hyphenate t
					 :threshold ,threshold
					 :final ,*emergency-stretch*))))
      (setq layouts (graph-layouts graph 'kp-layout))
      (mapc #'kp-postprocess-layout layouts)
      (setq layouts (sort layouts #'$< :key #'demerits))
      (unless (zerop *looseness*)
	(let ((ideal-size (+ (size (car layouts)) *looseness*)))
	  (setq layouts (sort layouts (lambda (size1 size2)
					(< (abs (- size1 ideal-size))
					   (abs (- size2 ideal-size))))
			  :key #'size))))
      (setq breakup (make-instance 'kp-graph-breakup
		      :pass pass :graph graph :layouts layouts))
      ;; #### WARNING: by choosing the first layout here, we're doing the
      ;; opposite of what TeX does in case of total demerits equality. We
      ;; could instead check for multiple such layouts and take the last one.
      ;; On the other hand, while we're using a hash table in the dynamic
      ;; programming implementation, we're not doing exactly what TeX does
      ;; either, so there's no rush. It's still important to keep that in mind
      ;; however, because that explains while we may end up with different
      ;; solutions between the graph and the dynamic versions.
      (setf (aref (renditions breakup) 0)
	    (kp-pin-layout harray disposition width beds (first layouts)
			   pass))
      breakup)))




;; ==========================================================================
;; Dynamic Variant
;; ==========================================================================

(defstruct (kp-node (:constructor kp-make-node))
  boundary scale fitness-class badness demerits total-demerits previous)

;; The active nodes hash table is accessed by
;; key = (boundary line-number fitness-class)
(defun make-key (boundary line fitness) (list boundary line fitness))
(defun key-boundary (key) (first key))
(defun key-line (key) (second key))
(defun key-fitness (key) (third key))


;; ---------------
;; Boundary lookup
;; ---------------

(defun kp-try-boundary
    (boundary nodes harray width threshold final
     &aux (emergency-stretch (when (numberp final) final))
	  last-deactivated-node new-nodes)
  "Examine BOUNDARY and update active NODES accordingly."
  (maphash
   (lambda (key node
	    &aux (previous-boundary (key-boundary key))
		 (previous-line (key-line key))
		 (previous-fitness (key-fitness key))
		 (scale (harray-scale harray
				      (start-idx previous-boundary)
				      (stop-idx boundary)
				      width)))
     (when (or ($< scale -1)
	       (eq (penalty (item boundary)) -∞)
	       ;; #### WARNING: we must deactivate all nodes when we reach
	       ;; the paragraph's end. TeX does this by adding a forced
	       ;; break at the end.
	       (last-boundary-p boundary))
       (setq last-deactivated-node (cons key node))
       (remhash key nodes))
     (when ($<= -1 scale)
       (let ((badness (scale-badness
		       (if emergency-stretch
			 (harray-scale harray
				       (start-idx previous-boundary)
				       (stop-idx boundary)
				       width emergency-stretch)
			 scale))))
	 (when ($<= badness threshold)
	   (let* ((fitness (scale-fitness-class scale))
		  (demerits (local-demerits badness (penalty (item boundary))
					    *line-penalty*))
		  (total-demerits ($+ (kp-node-total-demerits node)
				      demerits)))
	     ;; #### FIXME: for now, all contextual penalties affect the total
	     ;; demerits only below. This is to remain consistent with the
	     ;; graph implementation, and is due to a limitation in the
	     ;; layouts design. See the related comment in the layouts section
	     ;; of graph.lisp.
	     (when (> (abs (- fitness previous-fitness)) 1)
	       (setq total-demerits ($+ total-demerits *adjacent-demerits*)))
	     ;; #### NOTE: according to #859, TeX doesn't consider the
	     ;; admittedly very rare and weird case where a paragraph would
	     ;; end with an explicit hyphen. As stipulated in #829, for the
	     ;; purpose of computing demerits, the end of the paragraph is
	     ;; always regarded as virtually hyphenated, and in case the
	     ;; previous line (really) is hyphenated, the value of
	     ;; final-hyphen-demerits is used instead of
	     ;; double-hyphen-demerits. One could consider using
	     ;; double-hyphen-demerits when there actually is a final hyphen,
	     ;; but on the other hand, the final line is rarely justified so
	     ;; the two hyphens are very unlikely to create a ladder.
	     (when (discretionaryp (item previous-boundary))
	       (if (last-boundary-p boundary)
		 (setq total-demerits
		       ($+ total-demerits *final-hyphen-demerits*))
		 (when (discretionaryp (item boundary))
		   (setq total-demerits
			 ($+ total-demerits *double-hyphen-demerits*)))))
	     (let* ((new-key (make-key boundary (1+ previous-line) fitness))
		    (previous (find new-key new-nodes
				:test #'equal :key #'car)))
	       (if previous
		 ;; #### NOTE: the inclusive inequality below is conformant
		 ;; with what TeX does in #855. Concretely, it makes the KP
		 ;; algorithm greedy in some sense: in case of demerits
		 ;; equality, TeX keeps the last envisioned solution. On the
		 ;; other hand, we're in fact not doing exactly the same thing
		 ;; because we're using MAPHASH and the order of the nodes in
		 ;; the hash table is not deterministic.
		 (when ($<= total-demerits
			    (kp-node-total-demerits (cdr previous)))
		   (setf (kp-node-scale (cdr previous)) scale
			 (kp-node-badness (cdr previous)) badness
			 (kp-node-demerits (cdr previous)) demerits
			 (kp-node-total-demerits (cdr previous))
			 total-demerits
			 (kp-node-previous (cdr previous)) node))
		 (push (cons new-key
			     (kp-make-node :boundary boundary
					   :scale scale
					   :fitness-class fitness
					   :badness badness
					   :demerits demerits
					   :total-demerits total-demerits
					   :previous node))
		       new-nodes))))))))
   nodes)
  (when (and final (zerop (hash-table-count nodes)) (null new-nodes))
    (setq new-nodes
	  (list
	   (let* ((scale (harray-scale harray
				       (start-idx
					(key-boundary
					 (car last-deactivated-node)))
				       (stop-idx boundary)
				       width))
		  (fitness-class (scale-fitness-class scale)))
	     (cons (make-key boundary
			     (1+ (key-line (car last-deactivated-node)))
			     fitness-class)
		   (kp-make-node :boundary boundary
				 :scale scale
				 :fitness-class fitness-class
				 ;; #### NOTE: in this situation, TeX sets the
				 ;; local demerits to 0 (#855) by checking the
				 ;; artificial_demerits flag. So we just
				 ;; re-use the previous total.
				 :badness 0
				 :demerits 0
				 :total-demerits (kp-node-total-demerits
						  (cdr last-deactivated-node))
				 :previous (cdr last-deactivated-node)))))))
  (mapc (lambda (new-node)
	  (setf (gethash (car new-node) nodes) (cdr new-node)))
    new-nodes))

(defun kp-create-nodes (harray width pass)
  "Compute the best sequences of breaks for HARRAY in the Knuth-Plass sense.
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
  ;; line.
  ;;
  ;; The fake root boundary has:
  ;; - a null ITEM (making DISCRETIONARYP return NIL), essentially telling
  ;;   that we don't have previous hyphenation, so no double hyphen demerits.
  ;; - a START-IDX of 0, which is correct.
  ;;
  ;; The fake root key is a list of:
  ;; - the fake root boundary,
  ;; - a (previous) line number of 0,
  ;; - a fitness class of 2 (decent). TeX computes adjacent demerits even for
  ;;   the first line which doesn't really have a previous line. This has the
  ;;   effect of negatively weighting very loose first lines. See
  ;;   https:i/tug.org/pipermail/texhax/2023-May/026091.html
  ;;
  ;; The fake root node has:
  ;; - the fake root boundary, unused because we use the key to get it,
  ;; - a fitness class of 2 (see above), here for consistency but unused
  ;;   because we use the key to get it,
  ;; - an initial total demerits of 0.
  ;; The other slots (scale, badness, and demerits are not initialized).
  (let ((hyphenate (> pass 1))
	(threshold (if (> pass 1) *tolerance* *pre-tolerance*))
	(final (case pass
		 (1 nil)
		 (2 (zerop *emergency-stretch*))
		 (3 *emergency-stretch*)))
	(root-boundary (make-instance 'boundary :item nil :start-idx 0))
	(nodes (make-hash-table :test #'equal)))
    (setf (gethash (make-key root-boundary 0 2) nodes)
	  (kp-make-node :boundary root-boundary :fitness-class 2
			:total-demerits 0))
    (loop :for boundary := (next-boundary harray 0)
	    :then (next-boundary harray (stop-idx boundary))
	  :while boundary
	  :when (and ($< (penalty (item boundary)) +∞)
		     (or hyphenate
			 (not (hyphenation-point-p (item boundary)))))
	    :do (kp-try-boundary boundary nodes harray width threshold final))
    (unless (zerop (hash-table-count nodes)) nodes)))


;; -----------------
;; Lines computation
;; -----------------

(defun kp-dynamic-pin-node (harray disposition width beds node pass)
  "Pin Knuth-Plass NODE from HARRAY for a DISPOSITION paragraph of WIDTH."
  (loop :with disposition-options := (disposition-options disposition)
	;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce
	;; elastic underfulls (in case of an impossible layout, it falls back
	;; to overfull boxes). This means that the overstretch option has no
	;; effect, but it allows for a nice trick: we can indicate lines
	;; exceeding the tolerance thanks to an emergency stretch as
	;; overstretched, regardless of the option. This is done by setting
	;; the overstretched parameter to T and not counting emergency stretch
	;; in the stretch-tolerance one.
	:with overshrink := (getf disposition-options :overshrink)
	:with disposition := (disposition-type disposition)
	:with stretch-tolerance
	  := (stretch-tolerance (if (> pass 1) *tolerance* *pre-tolerance*))
	:with lines
	:for end := node :then (kp-node-previous end)
	:for beg := (kp-node-previous end)
	:while beg
	:for start := (start-idx (kp-node-boundary beg))
	:for stop := (stop-idx (kp-node-boundary end))
	:do (push (if (eq disposition :justified)
		    ;; #### NOTE: I think that the Knuth-Plass algorithm
		    ;; cannot produce elastic underfulls (in case of an
		    ;; impossible layout, it falls back to overfull boxes).
		    ;; This means that the overstretch option has no effect,
		    ;; but it allows for a nice trick: we can indicate lines
		    ;; exceeding the tolerance thanks to an emergency stretch
		    ;; as overstretched, regardless of the option. This is
		    ;; done by setting the overstretched parameter to T and
		    ;; not counting emergency stretch in the stretch-tolerance
		    ;; one.
		    (multiple-value-bind (theoretical effective)
			(actual-scales (kp-node-scale end)
			  :stretch-tolerance stretch-tolerance
			  :overshrink overshrink :overstretch t)
		      (make-instance 'kp-line
			:harray harray
			:start-idx start :stop-idx stop
			:beds beds
			:scale theoretical
			:effective-scale effective
			:fitness-class (kp-node-fitness-class end)
			:badness (kp-node-badness end)
			:demerits (kp-node-demerits end)))
		    (make-instance 'line
		      :harray harray :start-idx start :stop-idx stop
		      :beds beds))
		  lines)
	:finally
	   (return (loop :with baseline-skip := (baseline-skip harray)
			 :for y := 0 :then (+ y baseline-skip)
			 :for line :in lines
			 :for x := (case disposition
				     ((:flush-left :justified) 0)
				     (:centered (/ (- width (width line)) 2))
				     (:flush-right (- width (width line))))
			 :collect (pin-line line x y)))))


;; ------------------------
;; Breakup specialization
;; ------------------------

;; #### FIXME: this is good enough for now, but we want to keep around all
;; possible solutions for the remaining active nodes, just like what we do for
;; graph based breakups. In fact, since the dynamic optimization is
;; essentially just a way to keep only pruned graphs in memory, we should
;; still get layouts in the end.
(defclass kp-dynamic-breakup (kp-breakup-mixin simple-breakup)
  ((nodes-# :documentation "The number of remaining active nodes."
	    :initform 0 :initarg :nodes-# :reader nodes-#)
   (demerits :documentation "The total demerits."
	     :initform 0 :initarg :demerits :reader demerits))
  (:documentation "The KP-DYNAMIC-BREAKUP class."))

;; #### NOTE: the Knuth-Plass algorithm never refuses to typeset, so a nodes-#
;; of 0 means that the harray was empty.
(defmethod properties strnlcat
    ((breakup kp-dynamic-breakup) &aux (nodes-# (nodes-# breakup)))
  "Advertise the number of remaining active nodes."
  (unless (zerop nodes-#)
    (format nil "Demerits: ~A~@
	       From ~A remaining active node~:P."
      ($float (demerits breakup))
      nodes-#)))

(defun kp-dynamic-break-harray
    (harray disposition width beds &aux (pass 1))
  "Break HARRAY with the Knuth-Plass algorithm, dynamic programming version."
  (if (zerop (length harray))
    (make-instance 'kp-dynamic-breakup)
    (let* ((nodes (or (when ($>= *pre-tolerance* 0)
			(kp-create-nodes harray width pass))
		      (kp-create-nodes harray width (incf pass))
		      (kp-create-nodes harray width (incf pass))))
	   (best (loop :with total-demerits := +∞ :with best :with last
		       :for node :being :the :hash-values :in nodes
			 :using (hash-key key)
		       :do (setq last (cons (key-line key) node))
		       :when ($< (kp-node-total-demerits node) total-demerits)
			 :do (setq best (cons (key-line key) node)
				   total-demerits (kp-node-total-demerits node))
		       :finally (return (or best last)))))
      (if (zerop *looseness*)
	(setq best (cdr best))
	(setq best
	      (loop :with ideal-size := (+ (car best) *looseness*)
		    :with closer := (cdr best)
		    :with delta := (abs *looseness*)
		    :for node :being :the :hash-values :in nodes
		      :using (hash-key key)
		    :do (cond ((< (abs (- ideal-size (key-line key))) delta)
			       (setq closer node
				     delta (abs (- ideal-size (key-line key)))))
			      ((and (= (abs (- ideal-size (key-line key)))
				       delta)
				    (< (kp-node-total-demerits node)
				       (kp-node-total-demerits closer)))
			       (setq closer node)))
		    :finally (return closer))))
      (make-instance 'kp-dynamic-breakup
	:nodes-# (hash-table-count nodes)
	:demerits (kp-node-total-demerits best)
	:pass pass
	:pinned-lines (kp-dynamic-pin-node
		       harray disposition width beds best pass)))))




;; ==========================================================================
;; Variant Dispatch
;; ==========================================================================

(defmethod break-harray
    (harray disposition width beds (algorithm (eql :knuth-plass))
     &key ((:variant *variant*))
	  ((:line-penalty *line-penalty*))
	  ((:adjacent-demerits *adjacent-demerits*))
	  ((:double-hyphen-demerits *double-hyphen-demerits*))
	  ((:final-hyphen-demerits *final-hyphen-demerits*))
	  ((:pre-tolerance *pre-tolerance*))
	  ((:tolerance *tolerance*))
	  ((:emergency-stretch *emergency-stretch*))
	  ((:looseness *looseness*)))
  "Break HARRAY with the Knuth-Plass algorithm."
  (default-kp variant)
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  (calibrate-kp pre-tolerance :positive)
  (calibrate-kp tolerance :positive)
  (calibrate-kp emergency-stretch)
  (calibrate-kp looseness)
  (funcall (ecase *variant*
	     (:graph #'kp-graph-break-harray)
	     (:dynamic #'kp-dynamic-break-harray))
    harray disposition width beds))
