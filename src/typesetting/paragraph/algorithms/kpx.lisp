;; This is my Knuth-Plass Extended, aka, KPX algorithm.

;; #### FIXME: this code is way too much redundant with the Knuth-Plass one.

(in-package :etap)

;; ==========================================================================
;; Specification
;; ==========================================================================

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
(define-kpx-caliber adjacent-demerits 0 10000 20000)
(define-kpx-caliber double-hyphen-demerits 0 10000 20000)
(define-kpx-caliber final-hyphen-demerits 0 5000 20000)
(define-kpx-caliber similar-demerits 0 5000 20000)
(define-kpx-caliber pre-tolerance -1 100 10000)
(define-kpx-caliber tolerance 0 200 10000)
(define-kpx-caliber emergency-stretch 0 0 20)
(define-kpx-caliber looseness -10 0 10)


(define-global-variables variant line-penalty
  hyphen-penalty explicit-hyphen-penalty
  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
  similar-demerits
  pre-tolerance tolerance emergency-stretch looseness)


(defmacro calibrate-kpx (name &optional infinity)
  "Calibrate NAMEd KPX variable."
  `(calibrate kpx ,name :infinity ,infinity))

(defmacro default-kpx (name)
  "Default KPX NAMEd variable."
  `(default kpx ,name))




;; ==========================================================================
;; HList
;; ==========================================================================

;; #### WARNING: although we have a specific hierarchy for hyphenation points,
;; the Knuth-Plass applies hyphen penalties to all discretionaries, so we do
;; the same here.

(defmethod process-hlist
    (hlist disposition (algorithm (eql :kpx))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Adjust hyphen penalties in HLIST, and append the final glue to it."
  (calibrate-kpx hyphen-penalty t)
  (calibrate-kpx explicit-hyphen-penalty t)
  (mapc (lambda (item)
	  (when (discretionaryp item)
	    (setf (penalty item)
		  (if (pre-break item)
		    *hyphen-penalty*
		    *explicit-hyphen-penalty*))))
    hlist)
  (endpush (make-glue :stretch +∞ :penalty +∞) hlist)
  hlist)




;; ==========================================================================
;; Similarity (homeoteleutons and homeoarchies)
;; ==========================================================================

;; #### WARNING: this is good enough for now, but there are many limitations
;; to our approach to similarity.
;; 1. Comparing character metrics works only because we use a single font.
;; 2. Discarding kerns is probably not such a big deal, especially since we
;;    have a single font: the same sequence of characters would entail the
;;    same sequence of kerns.
;; 3. We stop at the first potential break point for several reasons (see
;;    below). This means that at least in theory, we might miss longer
;;    similarity sequences containing hyphenation points for instance. The
;;    reasons are the following:
;;    a. in the end-of-line case dynamic version, we can't remember whether
;;       it's been used by the previous line or not (although it's very
;;       unlikely). One exception would be hyphenation points with an infinite
;;       penalty, because these are not potential break points anymore.
;;       However,
;;    b. going through unused breakpoints requires recursively digging into
;;       the no-break constituents, hence some form of flattening. This is not
;;       currently implemented, but should probably be.
;; 5. We also stop at blanks, including unbreakable ones. That is because the
;;    only completely correct solution to similarity would be to compare
;;    vertical alignments as well, and this can only be done on lines (pinned
;;    objects) when scaling has been applied.
;; 6. In particular, this means that the end-of-line similarity doesn't
;;    currently work on the last two lines (which, at least in theory, could
;;    be both completely justified), because of the final infinite and
;;    unbreakable glue.
;;    #### TODO: in fact, I'm likely to remove this hack and treat the last
;;    line in a special way, so this might render this point obsolete.
;; 7. Finally, this approach works only on rectangular paragraphs.

;; #### FIXME: I don't like the asymmetry between BOL and EOL here. Perhaps
;; edges should be instantiated with a start boundary rather than just an
;; index.
(defun harray-bol (start harray &aux (item (aref harray start)) idx bol)
  "Return the beginning-of-line items for an HARRAY line starting at START.
This is the list of the first visible characters that lie between START and
the next break point."
  (cond ((discretionaryp item)
	 (setq idx (1+ start))
	 (setq bol (retain 'tfm:character-metrics (post-break item)
			   :key #'type-of)))
	(t
	 (setq idx start)))
  (append bol
	  (loop :for i :from idx :upto (1- (length harray))
		;;                ╰► probably terminated sooner by :until
		:for item := (aref harray i)
		:until (break-point-p item)
		:when (eq (type-of item) 'tfm:character-metrics)
		  :collect item)))

(defun boundary-eol (boundary harray &aux idx eol)
  "Return the end-of-line items for an HARRAY line ending at BOUNDARY.
This is the list of the last visible characters (including a final hyphen if
the line is hyphenated) that lie between BOUNDARY and the previous break
point, in reverse order."
  (cond ((discretionaryp (item boundary))
	 (setq idx (- (stop-idx boundary) 2))
	 (setq eol (retain 'tfm:character-metrics (pre-break (item boundary))
			   :key #'type-of)))
	(t ; glue
	 (setq idx (1- (stop-idx boundary)))))
  (loop :for i :from idx :downto 0 ; probably terminated sooner by :until
	:for item := (aref harray i)
	:until (break-point-p item)
	:when (eq (type-of item) 'tfm:character-metrics) :do (push item eol))
  (nreverse eol))




;; ==========================================================================
;; Graph Variant
;; ==========================================================================

;; -----
;; Edges
;; -----

(defclass kpx-edge (kp-edge)
  ((bol :documentation "This edge's beginning-of-line items."
	:reader bol)
   (eol :documentation "This edge's end-of-line items."
	:reader eol))
  (:documentation "The KPX-EDGE class."))

(defmethod initialize-instance :after
    ((edge kpx-edge)
     &key harray start width
     &aux (boundary (boundary (destination edge))))
  "Initialize KPX EDGE's beginning and end of line items, adjust the scale."
  (setf (slot-value edge 'bol) (harray-bol start harray)
	(slot-value edge 'eol) (boundary-eol boundary harray))
  ;; #### HACK ALERT: at that point, the last edge is initialized normally for
  ;; a last line, that is, without the need for justification. The scaling
  ;; will thus be <= 0, and the other properties computed accordingly. If we
  ;; consider that adjusting the scaling of the last line a-posteriori
  ;; improves the quality, we don't want to also consider that as a local
  ;; deterioration of the quality (for example: slightly stretching an
  ;; underfull last line shouldn't be considered "bad"). Because of that, we
  ;; just leave the badness and local demerits as they are now. On the other
  ;; hand, we need to remember the scaling required to properly justify the
  ;; line now, because this value will serve as the maximum authorized scaling
  ;; for later adjustment (we can shrink as much as we want, but if we want to
  ;; stretch, we still don't want to make the last line overfull. In order to
  ;; do that (and this is where the hack is), we re-use the SCALE slot to push
  ;; that new value on top of the original one, by calling HARRAY-SCALE again,
  ;; but not counting the final (infinitely stretchable) glue. Another, less
  ;; hackish possibility would be to have a KPX-LAST-EDGE class with an
  ;; additional slot, but we can't CHANGE-CLASS here because we're within a
  ;; method accessing the instance's slots... so that would require additional
  ;; modifications elsewhere.
  (when (last-boundary-p boundary)
    (setf (slot-value edge 'scale)
	  (cons (harray-scale harray start (1- (stop-idx boundary)) width)
		(slot-value edge 'scale)))))

;; Last line ledge
(defclass kpx-last-ledge (kp-ledge)
  ((scale :documentation "The adjusted last line scale."
	  :reader scale)
   (fitness-class :documentation "The adjusted last line fitness class."
		  :reader fitness-class))
  (:documentation "The KPX Last LEDGE class.
This class allows to override the last line scale and fitness-class on a
per-layout basis, in order to make it as close as possible to that of the the
one-before-last."))

;; #### NOTE: technically, this is not required, but it makes the code in
;; KPX-POSTPROCESS-LAYOUT more pleasant to read.
(defmethod update-instance-for-different-class :after
    ((old kp-ledge) (new kpx-last-ledge) &key)
  "Initialize the scale slot to the edge's one."
  (setf (slot-value new 'scale) (scale (edge old))))

;; #### FIXME: no need to advertise the adjusted scale here because that's
;; what the advertised line properties contain (hence, the design of the LINE
;; hierarchy is broken; see comment atop GRAPH-LINE). Also, the adjusted
;; fitness class is likely to not be very different from the original one.
(defmethod properties strnlcat ((ledge kpx-last-ledge))
  "Advertise KPX last LEDGE's adjusted fitness class."
  (format nil "Adjusted fitness class: ~A."
    (fitness-class-name (fitness-class ledge))))


;; -------
;; Layouts
;; -------

(defun kpx-postprocess-layout
    (layout &aux (total-demerits (demerits (edge (first (ledges layout))))))
  "Compute LAYOUT's properties."
  ;; #### NOTE: in order to stay close to the original philosophy about
  ;; adjacency of the first line (see warning in KPX-CREATE-NODES about that),
  ;; we do it by comparison with a scale of 0. This originally made sense in
  ;; TeX because the last line of previous paragraph would most often not be
  ;; scaled. On the other hand, with our last line adjustment feature in KPX,
  ;; this is not a validd assumption anymore. In theory, we should remember if
  ;; there's a previous paragraph, if so, what's the scaling of the last line,
  ;; and eventually compare to that.
  ;; #### WARNING: with a continuous function for adjacency demerits, we get
  ;; the risk of doing +∞ * 0 twice below, with infinite scaling differences
  ;; and no adjacent demerits. Fortunately, we have a way to protect ourselves
  ;; against that: without adjacent demerits, we don't even need to compute a
  ;; product at all (in other words, +∞ * 0 = 0 in this context).
  #+()(unless (zerop *adjacent-demerits*)
	(setq total-demerits ($+ total-demerits
				 ($* ($abs (scale (edge (first (ledges layout)))))
				     *adjacent-demerits*))))
  (when (= (fitness-class (edge (first (ledges layout)))) 0)
    (setf total-demerits ($+ total-demerits *adjacent-demerits*)))
  (setf (slot-value (first (ledges layout)) 'demerits) total-demerits)
  (unless (numberp (badness (edge (first (ledges layout)))))
    (incf (slot-value layout 'bads)))
  (when (> (length (ledges layout)) 1)
    (loop :for ledge1 :in (ledges layout)
	  :for ledge2 :in (cdr (ledges layout))
	  :for edge1 := (edge ledge1)
	  :for edge2 := (edge ledge2)
	  :for finalp := (last-boundary-p (boundary (destination edge2)))
	  :when finalp
	    :do (change-class ledge2 'kpx-last-ledge)
	    :and :do (setf (slot-value ledge2 'scale)
			   (if ($< (scale ledge1) (car (scale ledge2)))
			     ;; we can always shrink the last line as much as
			     ;; we want.
			     (scale ledge1)
			     ;; otherwise, we don't want to stretch more than
			     ;; required to justify
			     ($min (scale ledge1) (car (scale ledge2)))))
	    :and :do (setf (slot-value ledge2 'fitness-class)
			   (scale-fitness-class (scale ledge2)))
	  :unless (numberp (badness edge2))
	    :do (incf (slot-value layout 'bads))
	  :do (setq total-demerits ($+ total-demerits (demerits edge2)))
	  :when (>= (compare (bol edge1) (bol edge2)) 2)
	    :do (setq total-demerits ($+ total-demerits *similar-demerits*))
	  :when (and (not finalp) (>= (compare (eol edge1) (eol edge2)) 2))
	    :do (setq total-demerits ($+ total-demerits *similar-demerits*))
	  ;; See comment in dynamic version. Do not consider very rare case
	  ;; where the paragraph ends with an explicit hyphen.
	  :when (and (not finalp) (hyphenated ledge1) (hyphenated ledge2))
	    :do (setq total-demerits
		      ($+ total-demerits *double-hyphen-demerits*))
	  :when (and finalp (hyphenated ledge1))
	    :do (setq total-demerits
		      ($+ total-demerits *final-hyphen-demerits*))
	  :when (> (abs (- (fitness-class edge1) (fitness-class edge2))) 1)
	    :do (setf total-demerits ($+ total-demerits *adjacent-demerits*))
	  #|
	  :unless (zerop *adjacent-demerits*)
	    :do (setq total-demerits
		      ($+ total-demerits
			  ;; #### WARNING: we access the scale through the
			  ;; ledge, not the edge, because of the last line
			  ;; override.
			  ($* ($abs ($- (scale ledge1) (scale ledge2)))
	  *adjacent-demerits*)))
	  |#
	  :do (setf (slot-value ledge2 'demerits) total-demerits)))
  (setf (slot-value layout 'size) (length (ledges layout))))


;; -----------------
;; Lines computation
;; -----------------

;; #### NOTE: I'm keeping this for now, in anticipation for last line
;; adjustment changes (probably need to modify this function).
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
	  :for ledge :in (ledges layout)
	  :for boundary := (boundary (destination (edge ledge)))
	  :and start := 0 :then (start-idx boundary)
	  :for stop := (stop-idx boundary)
	  :for line := (case disposition
			 (:justified
			  (multiple-value-bind (theoretical effective)
			      ;; #### WARNING: we access the scale through the
			      ;; ledge, not the edge, because of the last line
			      ;; override.
			      (actual-scales (scale ledge)
				:stretch-tolerance stretch-tolerance
				:overshrink overshrink
				:overstretch t)
			    (make-instance 'graph-line
			      :harray harray :start-idx start
			      ;; #### HACK ALERT: don't count the final glue
			      ;; for last lines with non zero scaling !
			      :stop-idx (if (last-boundary-p boundary)
					  (1- stop)
					  stop)
			      :beds beds
			      :scale theoretical :effective-scale effective
			      :ledge ledge)))
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

(defun kpx-graph-break-harray (harray disposition width beds)
  "Break HARRAY with the KPX algorithm, graph version."
  (if (zerop (length harray))
    (make-instance 'kp-graph-breakup)
    (let ((threshold *pre-tolerance*)
	  (pass 1)
	  root nodes layouts breakup)
      (when ($<= 0 threshold)
	(multiple-value-setq (root nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kp-next-boundaries :threshold ,threshold))))
      (unless (and root (edges root))
	(incf pass)
	(setq threshold *tolerance*)
	(multiple-value-setq (root nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kp-next-boundaries
			       :hyphenate t
			       :threshold ,threshold
			       :final ,(zerop *emergency-stretch*)))))
      (unless (edges root)
	(incf pass)
	(multiple-value-setq (root nodes)
	  (make-graph harray width
	    :edge-type 'kpx-edge
	    :next-boundaries `(kp-next-boundaries
			       :hyphenate t
			       :threshold ,threshold
			       :final ,*emergency-stretch*))))
      (setq layouts
	    (make-layouts root :layout-type 'kp-layout :ledge-type 'kp-ledge))
      (mapc #'kpx-postprocess-layout layouts)
      ;; #### WARNING: in order to remain consistent with TeX, and as in the
      ;; dynamic version, an unfit line will have its demerits set to 0.
      ;; Contrary to the dynamic version however, the final pass may offer a
      ;; graph in which there are both fit and unfit possibilities, and it may
      ;; even turn out that an unfit one has fewer demerits than a fit one
      ;; (because of the zero'ed lines). Consequently, the layouts must be
      ;; sorted by number of bads first, and demerits next.
      (setq layouts
	    (sort layouts (lambda (l1 l2)
			    (or (< (bads l1) (bads l2))
				(and (= (bads l1) (bads l2))
				     (< (demerits l1) (demerits l2)))))))
      (unless (zerop *looseness*)
	(let ((ideal-size (+ (size (car layouts)) *looseness*)))
	  (setq layouts (stable-sort layouts (lambda (size1 size2)
					       (< (abs (- size1 ideal-size))
						  (abs (- size2 ideal-size))))
				     :key #'size))))
      (setq breakup (make-instance 'kp-graph-breakup
		      :root root :nodes nodes :layouts layouts :pass pass))
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

(defstruct (kpx-node (:constructor kpx-make-node) (:include kp-node)) bol eol)

(defstruct (kpx-last-node (:constructor kpx-make-last-node)
			  (:include kpx-node))
  original-scale original-fitness-class)


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
	   (let* ((eol (boundary-eol boundary harray))
		  (bol (unless (last-boundary-p boundary)
			 (harray-bol (start-idx boundary) harray)))
		  (fitness (scale-fitness-class scale))
		  (demerits (local-demerits badness (penalty (item boundary))
					    *line-penalty*))
		  (total-demerits ($+ (kpx-node-total-demerits node)
				      demerits))
		  original-scale original-fitness)
	     ;; Last line adjustment: see comments in the graph variant.
	     (when (last-boundary-p boundary)
	       (let ((max-scale (harray-scale harray
					      (start-idx previous-boundary)
					      (1- (stop-idx boundary))
					      width)))
		 (setq original-scale scale
		       original-fitness fitness
		       scale (if ($< (kpx-node-scale node) scale)
			       (kpx-node-scale node)
			       ($min (kpx-node-scale node) max-scale))
		       fitness (scale-fitness-class scale))))
	     (when (> (abs (- fitness previous-fitness)) 1)
	       (setq total-demerits ($+ total-demerits *adjacent-demerits*)))
	     ;; #### NOTE: for now, I'm considering that hyphenated
	     ;; similarities are even worse than regular ones, so we will
	     ;; apply both similar and double-hyphen demerits.
	     ;; #### FIXME: see with Thomas whether 2 is acceptable.
	     (when (>= (compare bol (kpx-node-bol node)) 2)
	       (setq total-demerits ($+ total-demerits *similar-demerits*)))
	     (when (>= (compare eol (kpx-node-eol node)) 2)
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
			 (kpx-node-previous (cdr previous)) node)
		   (when (last-boundary-p boundary)
		     (setf (kpx-last-node-original-scale (cdr previous))
			   original-scale
			   (kpx-last-node-original-fitness-class (cdr previous))
			   original-fitness)))
		 (push (cons new-key
			     (if (last-boundary-p boundary)
			       (kpx-make-last-node
				:eol eol
				:boundary boundary
				:scale scale
				:original-scale original-scale
				:fitness-class fitness
				:original-fitness-class original-fitness
				:badness badness
				:demerits demerits
				:total-demerits total-demerits
				:previous node)
			       (kpx-make-node
				:bol bol
				:eol eol
				:boundary boundary
				:scale scale
				:fitness-class fitness
				:badness badness
				:demerits demerits
				:total-demerits total-demerits
				:previous node)))
		       new-nodes))))))))
   nodes)
  (when (and final (zerop (hash-table-count nodes)) (null new-nodes))
    (setq new-nodes
	  (list
	   (let* ((node (cdr last-deactivated-node))
		  (previous-boundary (key-boundary (car last-deactivated-node)))
		  (scale (harray-scale harray
				       (start-idx previous-boundary)
				       (stop-idx boundary)
				       width))
		  (badness (scale-badness scale))
		  (fitness (scale-fitness-class scale))
		  original-scale original-fitness)
	     ;; Last line adjustment.
	     (when (last-boundary-p boundary)
	       (let ((max-scale (harray-scale harray
					      (start-idx previous-boundary)
					      (1- (stop-idx boundary))
					      width)))
		 (setq original-scale scale
		       original-fitness fitness
		       scale (if ($< (kpx-node-scale node) scale)
			       (kpx-node-scale node)
			       ($min (kpx-node-scale node) max-scale))
		       fitness (scale-fitness-class scale))))
	     (cons (make-key boundary
			     (1+ (key-line (car last-deactivated-node)))
			     fitness)
		   (if (last-boundary-p boundary)
		     (kpx-make-last-node
		      :eol (boundary-eol boundary harray)
		      :boundary boundary
		      :scale scale
		      :original-scale original-scale
		      :fitness-class fitness
		      :original-fitness-class original-fitness
		      :badness badness
		      ;; #### NOTE: in this situation, TeX sets the local
		      ;; demerits to 0 (#855) by checking the
		      ;; artificial_demerits flag. So we just re-use the
		      ;; previous total.
		      :demerits 0
		      :total-demerits
		      (kpx-node-total-demerits node)
		      :previous node)
		     (kpx-make-node
		      :bol (harray-bol (if (discretionaryp (item boundary))
					 (start-idx boundary)
					 (1+ (start-idx boundary)))
				       harray)
		      :eol (boundary-eol boundary harray)
		      :boundary boundary
		      :scale scale
		      :fitness-class fitness
		      :badness badness
		      ;; #### NOTE: in this situation, TeX sets the local
		      ;; demerits to 0 (#855) by checking the
		      ;; artificial_demerits flag. So we just re-use the
		      ;; previous total.
		      :demerits 0
		      :total-demerits
		      (kpx-node-total-demerits node)
		      :previous node)))))))
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
  ;; - a null eol and the correct bol so as to compute similar demerits,
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
			 :bol (harray-bol 0 harray)
			:total-demerits 0))
    (loop :for boundary := (next-boundary harray 0)
	    :then (next-boundary harray (idx boundary))
	  :while boundary
	  :when (and ($< (penalty (item boundary)) +∞)
		     (or hyphenate
			 (not (hyphenation-point-p (item boundary)))))
	    :do (kpx-try-boundary boundary nodes harray width threshold final))
    (unless (zerop (hash-table-count nodes)) nodes)))


;; -----------------
;; Lines computation
;; -----------------

(defclass kpx-last-dynamic-line (kp-dynamic-line)
  ((original-scale :documentation "The last line's original scale."
		   :initarg :original-scale
		   :reader original-scale)
   (original-fitness-class
    :documentation "The last line's original fitness class."
    :initarg :original-fitness-class
    :reader original-fitness-class))
  (:documentation "The KPX Last Dynamic Line class."))

(defmethod properties strnlcat ((line kpx-last-dynamic-line))
  "Advertise KPX last dynamic LINE's original scale and fitness class."
  (format nil "Original scale: ~A.~@
	       Original fitness class: ~A."
    ($float (original-scale line))
    (fitness-class-name (original-fitness-class line))))

;; #### NOTE: I'm keeping this for now, in anticipation for last line
;; adjustment changes (probably need to modify this function).
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
		      (if (last-boundary-p (kp-node-boundary end))
			(make-instance 'kpx-last-dynamic-line
			  :harray harray
			  :start-idx start
			  ;; #### HACK ALERT: don't count the final glue for
			  ;; last lines with non zero scaling !
			  :stop-idx (1- stop)
			  :beds beds
			  :scale theoretical
			  :original-scale (kpx-last-node-original-scale end)
			  :effective-scale effective
			  :fitness-class (kpx-node-fitness-class end)
			  :original-fitness-class
			  (kpx-last-node-original-fitness-class end)
			  :badness (kpx-node-badness end)
			  :demerits (kpx-node-demerits end)
			  :total-demerits (kpx-node-total-demerits end))
			(make-instance 'kp-dynamic-line
			  :harray harray
			  :start-idx start
			  :stop-idx stop
			  :beds beds
			  :scale theoretical
			  :effective-scale effective
			  :fitness-class (kpx-node-fitness-class end)
			  :badness (kpx-node-badness end)
			  :demerits (kpx-node-demerits end)
			  :total-demerits (kpx-node-total-demerits end))))
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

(defun kpx-dynamic-break-harray
    (harray disposition width beds &aux (pass 1))
  "Break HARRAY with the KPX algorithm, dynamic programming version."
  (if (zerop (length harray))
    (make-instance 'kp-dynamic-breakup)
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
      (setq breakup (make-instance 'kp-dynamic-breakup
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
