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

;; #### NOTE: in theory, when only the looseness changes, we don't have to run
;; the algorithm again: we only need to sort the solutions found previously in
;; a different order and select another one. We currently don't go that far,
;; and the GUI doesn't know that anyway.

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
(define-kp-caliber adjacent-demerits 0 10000 20000)
(define-kp-caliber double-hyphen-demerits 0 10000 20000)
(define-kp-caliber final-hyphen-demerits 0 5000 20000)
(define-kp-caliber pre-tolerance -1 100 10000)
(define-kp-caliber tolerance 0 200 10000)
(define-kp-caliber emergency-stretch 0 0 20)
(define-kp-caliber looseness -10 0 10)


(define-global-variables variant line-penalty
  hyphen-penalty explicit-hyphen-penalty
  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
  pre-tolerance tolerance emergency-stretch looseness)


(defmacro calibrate-kp (name &optional infinity)
  "Calibrate NAMEd Knuth-Plass variable."
  `(calibrate kp ,name :infinity ,infinity))

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


;; ----------------------
;; Breakup specialization
;; ----------------------

(defclass kp-breakup-mixin ()
  ((pre-tolerance
    :documentation "This breakup's pre-tolerance."
    :initarg :pre-tolerance :reader pre-tolerance)
   (tolerance
    :documentation "This breakup's tolerance."
    :initarg :tolerance :reader tolerance)
   (pass
    :documentation "Which of the 3 passes produced this breakup."
    :initform 0 :initarg :pass :reader pass))
  (:documentation "The KP-BREAKUP-MIXIN class.
This class is mixed in both the graph and dynamic breakup classes."))

;; #### NOTE: the Knuth-Plass algorithm never refuses to typeset, so a pass of
;; 0 means that the harray was empty.
(defmethod properties strnlcat
    ((mixin kp-breakup-mixin) &key &aux (pass (pass mixin)))
  "Advertise Knuth-Plass breakup MIXIN's pass number."
  (unless (zerop pass) (format nil "Pass: ~A." pass)))




;; ==========================================================================
;; Graph Variant
;; ==========================================================================

;; ----------
;; Boundaries
;; ----------

(defclass kp-boundary (fit-boundary)
  ((fitness-class
    :documentation "This boundary's fitness class."
    :reader fitness-class)
   (badness
    :documentation "This boundary's badness."
    :reader badness)
   (demerits
    :documentation "This boundary's local demerits."
    :reader demerits))
  (:documentation "The KP-boundary class."))

(defmethod initialize-instance :after ((boundary kp-boundary) &key)
  "Initialize BOUNDARY's fitness class, badness, and local demerits."
  ;; #### WARNING: it is possible to get a rigid line here (scale = +/-∞), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf
   (slot-value boundary 'fitness-class) (scale-fitness-class (scale boundary))
   (slot-value boundary 'badness)       (scale-badness (scale boundary)))
  (setf (slot-value boundary 'demerits)
	;; See comment in the dynamic version about this.
	(if (numberp (badness boundary))
	  (local-demerits (badness boundary) (penalty boundary) *line-penalty*)
	  0)))


;; Boundaries lookup

;; #### FIXME: the docstring is wrong, so is the one for kp-create-nodes.
;; Also, maybe I should pass a PASS argument here, as in kn-create-nodes now.
(defun kp-get-boundaries
    (harray bol width
     &key hyphenate threshold final
     &aux (emergency-stretch (when (numberp final) final)))
  "Get boundaries for an HARRAY line of WIDTH beginning at BOL.
This is the Knuth-Plass version.

See `kp-create-nodes' for the semantics of HYPHENATE and FINAL."
  (loop :with boundaries :with overfull :with emergency-boundary
	:with continue := t
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol continue)
	:when (and ($< (penalty eol) +∞)
		   (or hyphenate (not (hyphenation-point-p eol))))
	  :do (let ((boundary (make-instance 'kp-boundary
				:harray harray :bol bol :break-point eol
				:width width
				:extra emergency-stretch)))
		(when (eq (penalty eol) -∞) (setq continue nil))
		(cond ((> (min-width boundary) width)
		       (setq overfull boundary continue nil))
		      (($<= (scale-badness (scale boundary)) threshold)
		       (push boundary boundaries))
		      (t
		       (setq emergency-boundary boundary))))
	:finally (return (or boundaries
			     (when final
			       (list (or overfull emergency-boundary)))))))


;; -------
;; Layouts
;; -------

;; Ledges

(defclass kp-ledge (ledge)
  ((demerits :documentation "The cumulative demerits so far in the layout."
	     :initarg :demerits :reader demerits))
  (:documentation "The Knuth-Plass Ledge class."))

(defmethod properties strnlcat ((ledge kp-ledge) &key)
  "Advertise Knuth-Plass LEDGE's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A.~@
	       Badness: ~A.~@
	       Demerits: ~A (local), ~A (cumulative)."
    (fitness-class-name (fitness-class (boundary ledge)))
    ($float (badness (boundary ledge)))
    ($float (demerits (boundary ledge)))
    ($float (demerits ledge))))


;; Layouts

;; #### NOTE: the layout's demerits is in fact the demerits of the last ledge.
(defclass kp-layout (layout)
  ((demerits
    :documentation "This layout's total demerits."
    :initarg :demerits :reader demerits)
   (bads
    :documentation "The number of bad lines in this layout."
    :initarg :bads :reader bads)
   (size
    :documentation "This layout's size (i.e. the number of lines)."
    :initarg :size :reader size))
  (:documentation "The KP-LAYOUT class."))

;; #### NOTE: we only advertise the layout's demerits for now. The other
;; properties are here for sorting the layouts from best to worse.
(defmethod properties strnlcat ((layout kp-layout) &key)
  "Advertise Knuth-Plass LAYOUT's demerits."
  (format nil "Demerits: ~A." ($float (demerits layout))))

(defun kp-postprocess-layout (layout &aux (length (length (ledges layout))))
  "Compute LAYOUT's properties."
  (change-class (first (ledges layout)) 'kp-ledge
    :demerits (demerits (boundary (first (ledges layout)))))
  ;; See warning in KP-CREATE-NODES about that.
  (when (= (fitness-class (boundary (first (ledges layout)))) 0)
    (setf (slot-value (first (ledges layout)) 'demerits)
	  ($+ (demerits (first (ledges layout))) *adjacent-demerits*)))
  (change-class layout 'kp-layout
    :demerits (demerits (first (ledges layout)))
    :bads (if (numberp (badness (boundary (first (ledges layout))))) 0  1)
    :size length)
  (when (> length 1)
    (loop :for ledge1 :in (ledges layout)
	  :for ledge2 :in (cdr (ledges layout))
	  :for finalp := (eopp (break-point (boundary ledge2)))
	  :unless (numberp (badness (boundary ledge2)))
	    :do (incf (slot-value layout 'bads))
	  :do (setf (slot-value layout 'demerits)
		    ($+ (demerits layout) (demerits (boundary ledge2))))
	  ;; See comment in dynamic version. Do not consider the very rare
	  ;; case where the paragraph ends with an explicit hyphen.
	  :when (and (not finalp) (hyphenated ledge1) (hyphenated ledge2))
	    :do (setf (slot-value layout 'demerits)
		      ($+ (demerits layout) *double-hyphen-demerits*))
	  :when (and finalp (hyphenated ledge1))
	    :do (setf (slot-value layout 'demerits)
		      ($+ (demerits layout) *final-hyphen-demerits*))
	  :when (> (abs (- (fitness-class (boundary ledge1))
			   (fitness-class (boundary ledge2))))
		   1)
	    :do (setf (slot-value layout 'demerits)
		      ($+ (demerits layout) *adjacent-demerits*))
	  :do (change-class ledge2 'kp-ledge :demerits (demerits layout))))
  layout)


;; -------
;; Breakup
;; -------

;; #### NOTE: the KP Breakup Mixin comes first for proper ordering of the
;; displayed properties.
(defclass kp-graph-breakup (kp-breakup-mixin graph-breakup)
  ()
  (:documentation "The Knuth-Plass Graph Breakup class."))

(defun kp-graph-break-harray (harray disposition width)
  "Break HARRAY with the Knuth-Plass algorithm, graph version."
  (if (zerop (length harray))
    (make-instance 'kp-graph-breakup
      :disposition disposition :width width :harray harray
      :pre-tolerance *pre-tolerance* :tolerance *tolerance*)
    (let ((pass 1) graph layouts)
      (when ($<= 0 *pre-tolerance*)
	(setq graph
	      (make-graph harray width
			  (lambda (harray bol width)
			    (kp-get-boundaries harray bol width
			      :threshold *pre-tolerance*)))))
      (unless (and graph (gethash *bop* graph))
	(incf pass)
	(setq graph
	      (make-graph harray width
			  (lambda (harray bol width)
			    (kp-get-boundaries harray bol width
			      :hyphenate t
			      :threshold *tolerance*
			      :final (zerop *emergency-stretch*))))))
      (unless (gethash *bop* graph)
	(incf pass)
	(setq graph
	      (make-graph harray width
			  (lambda (harray bol width)
			    (kp-get-boundaries  harray bol width
			      :hyphenate t
			      :threshold *tolerance*
			      :final *emergency-stretch*)))))
      (setq layouts (mapc #'kp-postprocess-layout (make-layouts graph)))
      ;; #### WARNING: in order to remain consistent with TeX, and as in the
      ;; dynamic version, an unfit line will have its demerits set to 0.
      ;; Contrary to the dynamic version however, the final pass may offer a
      ;; graph in which there are both fit and unfit possibilities, and it may
      ;; even turn out that an unfit one has fewer demerits than a fit one
      ;; (because of the zero'ed lines). Consequently, the layouts must be
      ;; sorted by number of bads first, and demerits next.

      ;; #### WARNING: by choosing the first layout here, we're doing the
      ;; opposite of what TeX does in case of total demerits equality
      ;; (extremely rare), or when there's no solution and we resort to
      ;; overfulls, because TeX restores the last deactivated node (so the
      ;; last seen (im)possibility. We could instead check for multiple
      ;; equivalent layouts and take the last one. On the other hand, while
      ;; we're using a hash table in the dynamic programming implementation,
      ;; we're not doing exactly what TeX does either, so there's no rush.
      ;; It's still important to keep that in mind however, because that
      ;; explains while we may end up with different solutions between the
      ;; graph and the dynamic versions.
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
      (make-instance 'kp-graph-breakup
	:disposition disposition :width width
	:harray harray :graph graph :layouts layouts
	:pre-tolerance *pre-tolerance* :tolerance *tolerance* :pass pass))))


;; -----
;; Lines
;; -----

;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce elastic
;; underfulls (in case of an impossible layout, it falls back to overfull
;; boxes). This means that the overstretch option has no effect, but it allows
;; for a nice trick: we can indicate lines exceeding the tolerance thanks to
;; an emergency stretch as overstretched, regardless of the option. This is
;; done by setting the overstretch parameter to T and not counting emergency
;; stretch in the stretch tolerance below.

(defun kp-make-justified-line
    (harray bol ledge stretch-tolerance overshrink)
  "KP version of `make-ledge-line' for justified lines."
  (multiple-value-bind (theoretical effective)
      (actual-scales (scale (boundary ledge))
	:stretch-tolerance stretch-tolerance
	:overshrink overshrink
	:overstretch t)
    (make-ledge-line harray bol ledge
      :scale theoretical
      :effective-scale effective)))


;; ----------
;; Renditions
;; ----------

(defmethod make-rendition
    (nth (breakup kp-graph-breakup)
     &aux (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (overshrink (getf (disposition-options disposition) :overshrink))
	  (stretch-tolerance (if (> (pass breakup) 1)
			       (tolerance breakup)
			       (pre-tolerance breakup))))
  ;; #### NOTE: no emergency stretch counted here. See comment on top of
  ;; KP-MAKE-JUSTIFIED-LINE.
  (pin-lines
   (make-layout-lines (harray breakup)
		      (aref (layouts breakup) nth)
		      (case disposition-type
			(:justified
			 (lambda (harray bol ledge)
			   (kp-make-justified-line
			    harray bol ledge stretch-tolerance overshrink)))
			(t ;; just switch back to normal spacing.
			 (lambda (harray bol ledge)
			   (make-line harray bol (boundary ledge))))))
   disposition-type
   (width breakup)))




;; ==========================================================================
;; Dynamic Variant
;; ==========================================================================

(defstruct (kp-node (:constructor kp-make-node))
  boundary scale fitness-class badness demerits total-demerits previous)

(defmethod properties strnlcat ((node kp-node) &key)
  "Advertise KP dynamic NODE's properties."
  (format nil "Demerits: ~A" ($float (kp-node-total-demerits node))))


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
				      (bol-idx (break-point previous-boundary))
				      (eol-idx (break-point boundary))
				      width)))
     (when (or ($< scale -1)
	       (eq (penalty boundary) -∞)
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
				       (bol-idx (break-point previous-boundary))
				       (eol-idx (break-point boundary))
				       width emergency-stretch)
			 scale))))
	 (when ($<= badness threshold)
	   (let* ((fitness (scale-fitness-class scale))
		  (demerits (local-demerits badness (penalty boundary)
					    *line-penalty*))
		  (total-demerits ($+ (kp-node-total-demerits node)
				      demerits)))
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
	     (when (discretionaryp (break-point previous-boundary))
	       (if (last-boundary-p boundary)
		 (setq total-demerits
		       ($+ total-demerits *final-hyphen-demerits*))
		 (when (discretionaryp (break-point boundary))
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
				       (bol-idx
					(break-point
					 (key-boundary
					  (car last-deactivated-node))))
				       (eol-idx (break-point boundary))
				       width))
		  (badness (scale-badness scale))
		  (fitness-class (scale-fitness-class scale)))
	     (cons (make-key boundary
			     (1+ (key-line (car last-deactivated-node)))
			     fitness-class)
		   (kp-make-node :boundary boundary
				 :scale scale
				 :fitness-class fitness-class
				 :badness badness
				 ;; #### NOTE: in this situation, TeX sets the
				 ;; local demerits to 0 (#855) by checking the
				 ;; artificial_demerits flag. So we just
				 ;; re-use the previous total.
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
	(root-boundary (make-instance 'boundary :break-point *bop*))
	(nodes (make-hash-table :test #'equal)))
    (setf (gethash (make-key root-boundary 0 2) nodes)
	  (kp-make-node :boundary root-boundary :fitness-class 2
			:total-demerits 0))
    (loop :for boundary := (next-boundary harray 0)
	    :then (next-boundary harray (idx (break-point boundary)))
	  :while boundary
	  :when (and ($< (penalty boundary) +∞)
		     (or hyphenate
			 (not (hyphenated boundary))))
	    :do (kp-try-boundary boundary nodes harray width threshold final))
    (unless (zerop (hash-table-count nodes)) nodes)))


;; -------
;; Breakup
;; -------

(defclass kp-dynamic-breakup (kp-breakup-mixin breakup)
  ((harray
    :documentation "This breakup's harray."
    :initarg :harray :reader harray)
   (nodes
    :documentation "The breakup's sorted nodes array."
    :initform nil :reader nodes)
   (renditions
    :documentation "The breakups' sorted renditions array."
    :initform nil :reader renditions))
  (:documentation "The KP-DYNAMIC-BREAKUP class."))

(defmethod initialize-instance :after
    ((breakup kp-dynamic-breakup) &key nodes)
  "Convert the nodes list to an array and create the renditions array."
  (when nodes
    (setf (slot-value breakup 'nodes)
	  (make-array (length nodes) :initial-contents nodes))
    (setf (slot-value breakup 'renditions)
	  (make-array (length nodes) :initial-element nil))))


(defmethod properties strnlcat
    ((breakup kp-dynamic-breakup) &key rendition &aux (nodes (nodes breakup)))
  "Advertise Knuth-Plass dynamic BREAKUP's demerits and remaining active nodes."
  (when nodes
    (strnlcat
     (format nil "Remaining active nodes: ~A." (length nodes))
     (when rendition (properties (aref nodes rendition)))
     (when rendition (properties (get-rendition rendition breakup))))))


(defun kp-dynamic-break-harray (harray disposition width &aux (pass 1))
  "Break HARRAY with the Knuth-Plass algorithm, dynamic programming version."
  (if (zerop (length harray))
    (make-instance 'kp-dynamic-breakup
      :disposition disposition :width width :harray harray
      :pre-tolerance *pre-tolerance* :tolerance *tolerance*)
    (let* ((nodes (or (when ($>= *pre-tolerance* 0)
			(kp-create-nodes harray width pass))
		      (kp-create-nodes harray width (incf pass))
		      (kp-create-nodes harray width (incf pass))))
	   nodes-list)
      (maphash (lambda (key node)
		 (push (cons (key-line key) node) nodes-list))
	       nodes)
      (setq nodes-list
	    (sort nodes-list #'$<
	      :key (lambda (elt) (kp-node-total-demerits (cdr elt)))))
      (unless (zerop *looseness*)
	(let ((ideal-size (+ (car (first nodes-list)) *looseness*)))
	  (setq nodes-list
		(stable-sort nodes-list (lambda (elt1 elt2)
					  (< (abs (- elt1 ideal-size))
					     (abs (- elt2 ideal-size))))
			     :key #'car))))
      (make-instance 'kp-dynamic-breakup
	:disposition disposition :width width :harray harray
	:pre-tolerance *pre-tolerance* :tolerance *tolerance*
	:pass pass :nodes (mapcar #'cdr nodes-list)))))


;; -----
;; Lines
;; -----

(defclass kp-dynamic-line (line)
  ((fitness-class :documentation "This line's fitness class."
		  :initarg :fitness-class
		  :reader fitness-class)
   (badness :documentation "This line's badness."
	    :initarg :badness
	    :reader badness)
   (demerits :documentation "This line's local demerits."
	     :initarg :demerits
	     :reader demerits)
   (total-demerits :documentation "The total demerits so far."
		   :initarg :total-demerits
		   :reader total-demerits))
  (:documentation "The Knuth-Plass Dynamic Line class."))

(defmethod properties strnlcat ((line kp-dynamic-line) &key)
  "Advertise Knuth-Plass dynamic LINE's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A.~@
	       Badness: ~A.~@
	       Demerits: ~A (local), ~A (cunulative)."
    (fitness-class-name (fitness-class line))
    ($float (badness line))
    ($float (demerits line))
    ($float (total-demerits line))))


;; ----------
;; Renditions
;; ----------

;; #### FIXME: the renditions logic below is a duplicate of the graph one,
;; essentially because we should have a super-class for multiple-renditions
;; breakups, split apart from the graph one.

;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce elastic
;; underfulls (in case of an impossible layout, it falls back to overfull
;; boxes). This means that the overstretch option has no effect, but it allows
;; for a nice trick: we can indicate lines exceeding the tolerance thanks to
;; an emergency stretch as overstretched, regardless of the option. This is
;; done by setting the overstretched parameter to T and not counting emergency
;; stretch in the stretch-tolerance one.
(defmethod make-rendition
    (nth (breakup kp-dynamic-breakup)
     &aux (harray (harray breakup))
	  (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (disposition-options (disposition-options disposition))
	  (overshrink (getf disposition-options :overshrink))
	  (stretch-tolerance (stretch-tolerance
			      (if (> (pass breakup) 1)
				(tolerance breakup)
				(pre-tolerance breakup)))))
  "Render Nth node from KP Dynamic BREAKUP."
  (pin-lines
   (loop :with lines
	 :for end := (aref (nodes breakup) nth) :then (kp-node-previous end)
	 :for beg := (kp-node-previous end)
	 :while beg
	 :for start := (bol-idx (break-point (kp-node-boundary beg)))
	 :for stop  := (eol-idx (break-point (kp-node-boundary end)))
	 :do (push (if (eq disposition-type :justified)
		     ;; #### NOTE: I think that the Knuth-Plass algorithm
		     ;; cannot produce elastic underfulls (in case of an
		     ;; impossible layout, it falls back to overfull boxes).
		     ;; This means that the overstretch option has no effect,
		     ;; but it allows for a nice trick: we can indicate lines
		     ;; exceeding the tolerance thanks to an emergency stretch
		     ;; as overstretched, regardless of the option. This is
		     ;; done by setting the overstretched parameter to T and
		     ;; not counting emergency stretch in the
		     ;; stretch-tolerance one.
		     (multiple-value-bind (theoretical effective)
			 (actual-scales (kp-node-scale end)
			   :stretch-tolerance stretch-tolerance
			   :overshrink overshrink :overstretch t)
		       (make-instance 'kp-dynamic-line
			 :harray harray
			 :start-idx start :stop-idx stop
			 :scale theoretical
			 :effective-scale effective
			 :fitness-class (kp-node-fitness-class end)
			 :badness (kp-node-badness end)
			 :demerits (kp-node-demerits end)
			 :total-demerits (kp-node-total-demerits end)))
		     (make-instance 'line
		       :harray harray :start-idx start :stop-idx stop))
		   lines)
	 :finally (return lines))
   disposition-type
   (width breakup)))

;; #### NOTE: the call to LENGTH below will return 0 when the RENDITIONS slot
;; is nil, as well as when it's an array of size 0.
(defmethod renditions-# ((breakup kp-dynamic-breakup))
  "Return KP Dynamic BREAKUP's renditions number."
  (length (renditions breakup)))

(defmethod get-rendition (nth (breakup kp-dynamic-breakup))
  "Return the Nth KP Dynamic BREAKUP's rendition."
  (or (aref (renditions breakup) nth) (make-rendition nth breakup)))




;; ==========================================================================
;; Variant Dispatch
;; ==========================================================================

(defmethod break-harray
    (harray disposition width (algorithm (eql :knuth-plass))
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
    harray disposition width))
