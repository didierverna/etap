;; This is my Knuth-Plass Extended, aka, KPX algorithm

;; #### FIXME: my implementation of the 3 passes is probably not correct. It
;; seems that TeX goes into the 2nd pass, not only when the first fails, but
;; also when it succeeds with a non-zero looseness, and the number of lines
;; doesn't match. I need to check this.

(in-package :etap)

;; ==========================================================================
;; Specification
;; ==========================================================================

;; #### NOTE: in theory, when only the looseness changes, we don't have to run
;; the algorithm again: we only need to sort the solutions found previously in
;; a different order and select another one. We currently don't go that far,
;; and the GUI doesn't know that anyway.

;; #### TODO: maybe we could introduce a basic value for similar demerits and
;; multiply it by the size of the similarity, rather than using a fixed value.

(defparameter *kpx-variants*
  '(:graph :dynamic))

(defparameter *kpx-variants-help-keys*
  '(:kpx-variant-graph :kpx-variant-dynamic))

(defparameter *kpx-tooltips*
  '(:kpx-variant-graph "Graph-based implementation."
    :kpx-variant-dynamic "Dynamic programming implementation."))


(defmacro define-kpx-caliber (name min default max)
  "Define a NAMEd KPX caliber with MIN, DEFAULT, and MAX values."
  `(define-caliber kpx ,name ,min ,default ,max))

(define-kpx-caliber line-penalty 0 10 100)
(define-kpx-caliber hyphen-penalty -10000 50 10000)
(define-kpx-caliber explicit-hyphen-penalty -10000 50 10000)
(define-kpx-caliber adjacent-demerits 0 10000 10000)
(define-kpx-caliber double-hyphen-demerits 0 10000 10000)
(define-kpx-caliber final-hyphen-demerits 0 5000 10000)
(define-kpx-caliber similar-demerits 0 5000 10000)
(define-kpx-caliber pre-tolerance -1 100 10000)
(define-kpx-caliber tolerance 0 200 10000)
(define-kpx-caliber emergency-stretch 0 0 20)
(define-kpx-caliber looseness -10 0 10)


(define-global-variables variant
  line-penalty hyphen-penalty explicit-hyphen-penalty
  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
  similar-demerits
  pre-tolerance tolerance emergency-stretch looseness)


(defmacro calibrate-kpx (name &optional infinity)
  "Calibrate NAMEd KPX variable."
  `(calibrate kpx ,name ,infinity))

(defmacro default-kpx (name)
  "Default KPX NAMEd variable."
  `(default kpx ,name))




;; ==========================================================================
;; HList
;; ==========================================================================

(defmethod process-hlist
    (hlist disposition (algorithm (eql :kpx))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Adjust hyphen penalties in HLIST, and append the final glue to it."
  (calibrate-kpx hyphen-penalty t)
  (calibrate-kpx explicit-hyphen-penalty t)
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

(defclass kpx-line (line)
  ((fitness-class :documentation "This line's fitness class."
		  :initarg :fitness-class
		  :reader fitness-class)
   (badness :documentation "This line's badness."
	    :initarg :badness
	    :reader badness)
   (demerits :documentation "This line's local demerits."
	     :initarg :demerits
	     :reader demerits))
  (:documentation "The KPX line class."))

(defmethod properties strnlcat ((line kpx-line))
  "Advertise LINE's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A.~%Badness: ~A.~%Demerits: ~A."
    (fitness-class-name (fitness-class line))
    ($float (badness line))
    ($float (demerits line))))


;; ----------------------
;; Breakup specialization
;; ----------------------

(defclass kpx-breakup-mixin ()
  ((pass :documentation "Which of the 3 passes produced this breakup."
	 :initform 0 :initarg :pass :reader pass))
  (:documentation "The KPX-BREAKUP-MIXIN class.
This class is mixed in both the graph and dynamic breakup classes."))

;; #### NOTE: the KPX algorithm never refuses to typeset, so a pass of
;; 0 means that the harray was empty.
(defmethod properties strnlcat
    ((mixin kpx-breakup-mixin) &aux (pass (pass mixin)))
  "Advertise TeX's algorithm pass number and total demerits."
  (unless (zerop pass) (format nil "Pass: ~A." pass)))




;; ==========================================================================
;; Graph Variant
;; ==========================================================================

;; -----
;; Edges
;; -----

(defclass kpx-edge (edge)
  ((scale :documentation "This edge's scale."
	  :reader scale)
   (fitness-class :documentation "This edge's fitness class."
		  :reader fitness-class)
   (badness :documentation "This edge's badness."
	    :reader badness)
   (demerits :documentation "This edge's local demerits."
	     :reader demerits))
  (:documentation "The KPX-EDGE class."))

(defmethod initialize-instance :after
    ((edge kpx-edge)
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

(defclass kpx-layout (layout)
  ((size :documentation "This layout's size (i.e. the number of lines)."
	 :accessor size)
   (demerits :documentation "This layout's total demerits."
	     :accessor demerits))
  (:documentation "The KPX-LAYOUT class."))

(defmethod initialize-instance :after ((layout kpx-layout)  &key edge)
  "Initialize LAYOUT's size to 1 and demerits to its last EDGE's."
  (setf (size layout) 1 (demerits layout) (demerits edge)))

(defmethod properties strnlcat ((layout kpx-layout))
  "Advertise KPX LAYOUT's demerits."
  (format nil "Demerits: ~A." ($float (demerits layout))))

(defmethod push-edge :after (edge (layout kpx-layout))
  "Increase LAYOUT size by 1 and demerits with EDGE's."
  (incf (size layout))
  (setf (demerits layout) ($+ (demerits layout) (demerits edge))))

(defun kpx-postprocess-layout (layout)
  "Finish computing LAYOUT's total demerits.
When this function is called, LAYOUT's demerits contain only the sum of each
line's local ones. This function handles the remaining contextual information,
such as hyphen adjacency and fitness class differences between lines."
  ;; See warning in KPX-CREATE-NODES about that.
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

(defun kpx-next-boundaries
    (harray start width
     &key hyphenate threshold final
     &aux (emergency-stretch (when (numberp final) final)))
  "KPX graph implementation version of `next-boundaries'.
See `kpx-create-nodes' for the semantics of HYPHENATE and FINAL."
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

(defun kpx-pin-layout (harray disposition width beds layout pass)
  "Pin KPX LAYOUT from HARRAY for a DISPOSITION paragraph."
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
			    (make-instance 'kpx-line
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

(defclass kpx-graph-breakup (kpx-breakup-mixin graph-breakup)
  ()
  (:documentation "The Knuth-Plass Graph Breakup class."))

(defun kpx-graph-break-harray (harray disposition width beds)
  "Break HARRAY with the KPX algorithm, graph version."
  (if (zerop (length harray))
    (make-instance 'kpx-graph-breakup)
    (let ((threshold *pre-tolerance*)
	  (pass 1)
	  graph nodes layouts breakup)
      (when ($<= 0 threshold)
	(multiple-value-setq (graph nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kpx-next-boundaries :threshold ,threshold))))
      (unless (edges graph)
	(incf pass)
	(setq threshold *tolerance*)
	(multiple-value-setq (graph nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kpx-next-boundaries
			       :hyphenate t
			       :threshold ,threshold
			       :final ,(zerop *emergency-stretch*)))))
      (unless (edges graph)
	(incf pass)
	(multiple-value-setq (graph nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kpx-next-boundaries
			       :hyphenate t
			       :threshold ,threshold
			       :final ,*emergency-stretch*))))
      (setq layouts (graph-layouts graph 'kpx-layout))
      (mapc #'kpx-postprocess-layout layouts)
      (setq layouts (sort layouts #'$< :key #'demerits))
      (unless (zerop *looseness*)
	(let ((ideal-size (+ (size (car layouts)) *looseness*)))
	  (setq layouts (stable-sort layouts (lambda (size1 size2)
					       (< (abs (- size1 ideal-size))
						  (abs (- size2 ideal-size))))
				     :key #'size))))
      (setq breakup (make-instance 'kpx-graph-breakup
		      :pass pass :graph graph :nodes nodes :layouts layouts))
      ;; #### WARNING: by choosing the first layout here, we're doing the
      ;; opposite of what TeX does in case of total demerits equality. We
      ;; could instead check for multiple such layouts and take the last one.
      ;; On the other hand, while we're using a hash table in the dynamic
      ;; programming implementation, we're not doing exactly what TeX does
      ;; either, so there's no rush. It's still important to keep that in mind
      ;; however, because that explains while we may end up with different
      ;; solutions between the graph and the dynamic versions.
      (setf (aref (renditions breakup) 0)
	    (kpx-pin-layout harray disposition width beds (first layouts)
			   pass))
      breakup)))




;; ==========================================================================
;; Dynamic Variant
;; ==========================================================================

(defstruct (kpx-node (:constructor kpx-make-node))
  eol boundary scale fitness-class badness demerits total-demerits previous)

;; The active nodes hash table is accessed by
;; key = (boundary line-number fitness-class)
(defun make-key (boundary line fitness) (list boundary line fitness))
(defun key-boundary (key) (first key))
(defun key-line (key) (second key))
(defun key-fitness (key) (third key))


;; #### NOTE: the dynamic variant cannot check for beginning-of-line
;; similarities, unless we make different nodes for different bol's, just like
;; we make different nodes for different fitness classes. This would probably
;; ruin the optimization.

;; #### WARNING: this is good enough for now, but there are many limitations
;; to this approach to similarity.
;; 1. Comparing character metrics works only because we use a single font.
;; 2. Discarding kerns is probably not such a big deal, especially since we
;;    have a single font: the same sequence of characters would entail the
;;    same sequence of kerns.
;; 3. We stop at the first potential break point back because we can't
;;    remember whether it's been used by the previous line or not (although
;;    it's very unlikely); see comment above about the dynamic optimization
;;    constraint. This means that at least in theory, we might miss a longer
;;    similarity sequence containing hyphenation points.
;; 4. The only exception to this is hyphenation points with an infinite
;;    penalty, because these are not potential break points anymore.
;; 5. On the other hand, we also stop at blanks, including unbreakable ones.
;;    That is because the only completely correct solution to similarity would
;;    be to compare vertical alignments as well, and this can only be done on
;;    lines (pinned objects) when scaling has been applied.
;; 6. In particular, this means that similarity doesn't currently work on the
;;    last two lines (which, at least in theory, could be both completely
;;    justified), because of the final infinite and unbreakable glue.
;;    #### TODO: in fact, I'm likely to remove this hack and treat the last
;;    line in a special way, so this might render this point obsolete.
;; 7. Finally, this approach works only on rectangular paragraphs.

(defun eol (harray boundary &aux idx eol)
  "Return the end-of-line items for an HARRAY line ending at BOUNDARY.
This is the list of the last visible characters (including a final hyphen if
the line is hyphenated) that lie between BOUNDARY and the previous break
point, in reverse order."
  (cond ((hyphenation-point-p (item boundary))
	 (setq idx (1- (start-idx boundary)))
	 (setq eol (retain 'tfm:character-metrics (pre-break (item boundary))
			   :key #'type-of)))
	(t
	 (setq idx (1- (stop-idx boundary)))))
  (loop :for i :from idx :downto 0 ; probably terminated sooner by :until
	:for item := (aref harray i)
	:until (or (and (hyphenation-point-p item) ($< (penalty item) +∞))
		   (break-point-p item))
	:when (eq (type-of item) 'tfm:character-metrics) :do (push item eol))
  (nreverse eol))

(defun eol-eq (eol1 eol2)
  "Return the number of consecutive identical elements in EOL1 and EOL2."
  (loop :with i := 0
	:for elt1 :in eol1
	:for elt2 :in eol2
	:when (eq elt1 elt2)
	  :do (incf i)
	:finally (return i)))


;; ---------------
;; Boundary lookup
;; ---------------

(defun kpx-try-boundary
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
	   (let* ((eol (eol harray boundary))
		  (fitness (scale-fitness-class scale))
		  (demerits (local-demerits badness (penalty (item boundary))
					    *line-penalty*))
		  (total-demerits ($+ (kpx-node-total-demerits node)
				      demerits)))
	     ;; #### FIXME: for now, all contextual penalties affect the total
	     ;; demerits only below. This is to remain consistent with the
	     ;; graph implementation, and is due to a limitation in the
	     ;; layouts design. See the related comment in the layouts section
	     ;; of graph.lisp.
	     (when (> (abs (- fitness previous-fitness)) 1)
	       (setq total-demerits ($+ total-demerits *adjacent-demerits*)))
	     ;; #### NOTE: for now, I'm considering that hyphenated
	     ;; similarities are even worse than regular ones, so we will
	     ;; apply both similar and double-hyphen demerits.
	     ;; #### FIXME: see with Thomas whether 2 is acceptable.
	     (when (> (eol-eq eol (kpx-node-eol node)) 2)
	       (setq total-demerits ($+ total-demerits *similar-demerits*)))
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
			    (kpx-node-total-demerits (cdr previous)))
		   (setf (kpx-node-scale (cdr previous)) scale
			 (kpx-node-badness (cdr previous)) badness
			 (kpx-node-demerits (cdr previous)) demerits
			 (kpx-node-total-demerits (cdr previous))
			 total-demerits
			 (kpx-node-previous (cdr previous)) node))
		 (push (cons new-key
			     (kpx-make-node :eol eol
					    :boundary boundary
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
		   (kpx-make-node :eol (eol harray boundary)
				  :boundary boundary
				  :scale scale
				  :fitness-class fitness-class
				  ;; #### NOTE: in this situation, TeX sets
				  ;; the local demerits to 0 (#855) by
				  ;; checking the artificial_demerits flag. So
				  ;; we just re-use the previous total.
				  :badness 0
				  :demerits 0
				  :total-demerits (kpx-node-total-demerits
						   (cdr last-deactivated-node))
				  :previous (cdr last-deactivated-node)))))))
  (mapc (lambda (new-node)
	  (setf (gethash (car new-node) nodes) (cdr new-node)))
    new-nodes))

(defun kpx-create-nodes (harray width pass)
  "Compute the best sequences of breaks for HARRAY in the KPX sense.
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
  ;; to KPX-TRY-BOUNDARY above to work correctly when trying out the very first
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
	  (kpx-make-node :boundary root-boundary :fitness-class 2
			:total-demerits 0))
    (loop :for boundary := (next-boundary harray 0)
	    :then (next-boundary harray (stop-idx boundary))
	  :while boundary
	  :when (and ($< (penalty (item boundary)) +∞)
		     (or hyphenate
			 (not (hyphenation-point-p (item boundary)))))
	    :do (kpx-try-boundary boundary nodes harray width threshold final))
    (unless (zerop (hash-table-count nodes)) nodes)))


;; -----------------
;; Lines computation
;; -----------------

(defun kpx-dynamic-pin-node (harray disposition width beds node pass)
  "Pin KPX NODE from HARRAY for a DISPOSITION paragraph of WIDTH."
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
	:for end := node :then (kpx-node-previous end)
	:for beg := (kpx-node-previous end)
	:while beg
	:for start := (start-idx (kpx-node-boundary beg))
	:for stop := (stop-idx (kpx-node-boundary end))
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
			(actual-scales (kpx-node-scale end)
			  :stretch-tolerance stretch-tolerance
			  :overshrink overshrink :overstretch t)
		      (make-instance 'kpx-line
			:harray harray
			:start-idx start :stop-idx stop
			:beds beds
			:scale theoretical
			:effective-scale effective
			:fitness-class (kpx-node-fitness-class end)
			:badness (kpx-node-badness end)
			:demerits (kpx-node-demerits end)))
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

;; #### FIXME: since the dynamic optimization is essentially just a way to
;; keep only pruned graphs in memory, we should arrange to use a graph breakup
;; here.
(defclass kpx-dynamic-breakup (kpx-breakup-mixin breakup)
  ((nodes :documentation "The breakup's sorted nodes array."
	  :initform nil :reader nodes)
   (renditions :documentation "The breakups' sorted renditions array."
	       :initform nil :reader renditions))
  (:documentation "The KPX-DYNAMIC-BREAKUP class."))

(defmethod initialize-instance :after
    ((breakup kpx-dynamic-breakup) &key nodes)
  "Convert the ndoes list to an array and create the renditions array."
  (setf (slot-value breakup 'nodes)
	(make-array (length nodes) :initial-contents nodes))
  (setf (slot-value breakup 'renditions)
	(make-array (length nodes) :initial-element nil)))

(defmethod pinned-lines
    ((breakup kpx-dynamic-breakup) &aux (renditions (renditions breakup)))
  (when (and renditions (not (zerop (length renditions))))
    (aref renditions 0)))

;; #### NOTE: the Knuth-Plass algorithm never refuses to typeset, so a nodes-#
;; of 0 means that the harray was empty.
(defmethod properties strnlcat
    ((breakup kpx-dynamic-breakup)
     &aux (nodes (nodes breakup)) (nodes-# (length nodes)))
  "Advertise the number of remaining active nodes."
  (unless (zerop nodes-#)
    (format nil "Demerits: ~A~@
	       From ~A remaining active node~:P."
      ($float (kpx-node-total-demerits (aref nodes 0)))
      nodes-#)))

(defun kpx-dynamic-break-harray
    (harray disposition width beds &aux (pass 1))
  "Break HARRAY with the KPX algorithm, dynamic programming version."
  (if (zerop (length harray))
    (make-instance 'kpx-dynamic-breakup)
    (let* ((nodes (or (when ($>= *pre-tolerance* 0)
			(kpx-create-nodes harray width pass))
		      (kpx-create-nodes harray width (incf pass))
		      (kpx-create-nodes harray width (incf pass))))
	   nodes-list
	   breakup)
      (maphash (lambda (key node)
		 (push (cons (key-line key) node) nodes-list))
	       nodes)
      (setq nodes-list
	    (sort nodes-list #'$<
	      :key (lambda (elt) (kpx-node-total-demerits (cdr elt)))))
      (unless (zerop *looseness*)
	(let ((ideal-size (+ (car (first nodes-list)) *looseness*)))
	  (setq nodes-list
		(stable-sort nodes-list (lambda (elt1 elt2)
					  (< (abs (- elt1 ideal-size))
					     (abs (- elt2 ideal-size))))
			     :key #'car))))
      (setq breakup (make-instance 'kpx-dynamic-breakup
		      :pass pass :nodes (mapcar #'cdr nodes-list)))
      (setf (aref (renditions breakup) 0)
	    (kpx-dynamic-pin-node
	     harray disposition width beds (aref (nodes breakup) 0) pass))
      breakup)))




;; ==========================================================================
;; Variant Dispatch
;; ==========================================================================

(defmethod break-harray
    (harray disposition width beds (algorithm (eql :kpx))
     &key ((:variant *variant*))
	  ((:line-penalty *line-penalty*))
	  ((:adjacent-demerits *adjacent-demerits*))
	  ((:double-hyphen-demerits *double-hyphen-demerits*))
	  ((:final-hyphen-demerits *final-hyphen-demerits*))
	  ((:similar-demerits *similar-demerits*))
	  ((:pre-tolerance *pre-tolerance*))
	  ((:tolerance *tolerance*))
	  ((:emergency-stretch *emergency-stretch*))
	  ((:looseness *looseness*)))
  "Break HARRAY with the KPX algorithm."
  (default-kpx variant)
  (calibrate-kpx line-penalty)
  (calibrate-kpx adjacent-demerits)
  (calibrate-kpx double-hyphen-demerits)
  (calibrate-kpx final-hyphen-demerits)
  (calibrate-kpx similar-demerits)
  (calibrate-kpx pre-tolerance :positive)
  (calibrate-kpx tolerance :positive)
  (calibrate-kpx emergency-stretch)
  (calibrate-kpx looseness)
  (funcall (ecase *variant*
	     (:graph #'kpx-graph-break-harray)
	     (:dynamic #'kpx-dynamic-break-harray))
    harray disposition width beds))
