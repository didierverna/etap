;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

;; We provide both a full graph-based version and a dynamic programming
;; optimized one (as the original). These variants reimplement the principles
;; of the algorithm, not its original low-level implementation. This, by the
;; way, may induce some subtle behavioral differences.

;; For example, by choosing the first of the available layouts after sorting
;; them, we're doing the opposite of what TeX does in case of total demerits
;; equality (extremely rare), or when there's no solution and we resort to
;; overfulls, because TeX restores the last deactivated node (so the last seen
;; (im)possibility. We could instead check for multiple equivalent layouts and
;; take the last one. On the other hand, while we're using a hash table in the
;; dynamic programming implementation, we're not doing exactly what TeX does
;; either, so there's no rush. It's still important to keep that in mind
;; however, because that explains while we may end up with different solutions
;; between the graph and the dynamic versions.

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


(defmacro define-kp-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd Knuth-Plass caliber.
See `define-caliber' for more information."
  (declare (ignore infinity bounded))
  `(define-caliber kp ,name ,min ,default ,max ,@keys))

(define-kp-caliber glue-penalty -10000 0 10000 :infinity t)

(define-kp-caliber line-penalty -100 10 100)
(define-kp-caliber hyphen-penalty -10000 50 10000 :infinity t)
(define-kp-caliber explicit-hyphen-penalty -10000 50 10000 :infinity t)
(define-kp-caliber adjacent-demerits 0 10000 20000 :bounded :min)
(define-kp-caliber double-hyphen-demerits 0 10000 20000 :bounded :min)
(define-kp-caliber final-hyphen-demerits 0 5000 20000 :bounded :min)
(define-kp-caliber pre-tolerance -1 100 10000 :infinity :max :bounded :min)
;; #### FIXME: I've seen one case of \tolerance-1 in the TeX Book.
(define-kp-caliber tolerance 0 200 10000 :infinity :max :bounded :min)
(define-kp-caliber emergency-stretch 0 0 20 :bounded :min)
(define-kp-caliber looseness -10 0 10)


(define-global-variables variant line-penalty
  hyphen-penalty explicit-hyphen-penalty
  adjacent-demerits double-hyphen-demerits final-hyphen-demerits
  pre-tolerance tolerance emergency-stretch looseness)


(defmacro calibrate-kp (name)
  "Calibrate NAMEd Knuth-Plass variable."
  `(calibrate kp ,name))

(defmacro default-kp (name)
  "Default Knuth-Plass NAMEd variable."
  `(default kp ,name))




;; ==========================================================================
;; HList
;; ==========================================================================

;; #### WARNING: although we have a specific hierarchy for hyphenation points,
;; the Knuth-Plass applies hyphen penalties to all discretionaries, so we do
;; the same here.

(defmethod post-process-hlist
    (hlist disposition (algorithm (eql :knuth-plass))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Process HLIST for DISPOSITION by the Knuth-Plass algorithm.
Append the final glue to HLIST, and finish setting up hyphenation points.
This means defaulting their penalties and initializing the corresponding
caliber.
Return HLIST."
  (calibrate-kp hyphen-penalty)
  (calibrate-kp explicit-hyphen-penalty)
  (mapc (lambda (item)
	  (typecase item
	    (discretionary
	     (cond ((pre-break item)
		    (setf (penalty item) *hyphen-penalty*)
		    (setf (slot-value item 'caliber)
			  *kp-hyphen-penalty*))
		   (t
		    (setf (penalty item) *explicit-hyphen-penalty*)
		    (setf (slot-value item 'caliber)
			  *kp-explicit-hyphen-penalty*))))
	    (glue
	     (setf (slot-value item 'caliber) *kp-glue-penalty*))))
    hlist)
  (endpush (make-glue :stretch +∞ :penalty +∞ :caliber *kp-glue-penalty*) hlist)
  hlist)




;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### WARNING: the logic in SARS is to establish SAR tolerances, whereas TeX
;; uses badness tolerances. Hence I need to convert it back (from a float to a
;; ratio), which is not very nice.
(defun stretch-tolerance (badness-tolerance)
  "Return the stretch tolerance corresponding to BADNESS-TOLERANCE."
  ;; #### NOTE: we don't get a negative pre-tolerance here because pass 1
  ;; would be skipped. So we only need to handle the $>= 0 case.
  (if ($= badness-tolerance +∞)
    +∞
    (rationalize (expt (/ badness-tolerance 100) 1/3))))

;; #### NOTE: we don't use the same numerical values as in the real
;; Knuth-Plass here. The point being that having a decent fitness class of 0
;; makes our life simpler for handling extended fitness classes in KPX.
(defun sar-fitness-class (sar)
  "Return SAR's fitness class.
This is an integer ranging from -1 (tight) to 2 (very loose)."
  (cond (($< sar -1/2) -1)
	(($<= sar 1/2)  0)
	(($<= sar 1)    1)
	(t              2)))

(defun fitness-class-name (fitness-class)
  "Return FITNESS-CLASS's name (a string)."
  (ecase fitness-class (-1 "tight") (0 "decent") (1 "loose") (2 "very loose")))

(defun kp-fitness-demerits (fc1 fc2)
    "Return Knuth-Plass adjacent demerits for fitness classes FC1 and 2."
  (if (> (abs (- fc1 fc2)) 1) *adjacent-demerits* 0))




;; ==========================================================================
;; Variant-Independent Data Structures
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

(defmethod initialize-instance :after
    ((boundary kp-boundary) &key width stretch shrink extra target)
  "Initialize Knuth-Plass BOUNDARY's properties.
This includes its fitness class, badness, and local demerits.
Possibly also restore BOUNDARY's TSAR to the real value in case of EXTRA,
that is, of an emergency stretch ."
  ;; #### WARNING: it is possible to get a rigid line here (TSAR = +/-∞), not
  ;; only an overfull one. For example, we could have collected an hyphenated
  ;; beginning of word thanks to an infinite tolerance, and this would result
  ;; in a rigid underfull. This probably doesn't happen in TeX with its
  ;; dynamic programming implementation. But the problem is that we can't
  ;; define a sensible fitness class in such a case. So we consider those
  ;; lines to be very tight (as overfulls) even if they are actually
  ;; underfull.
  (setf
   (slot-value boundary 'fitness-class) (sar-fitness-class (tsar boundary))
   (slot-value boundary 'badness)       (sar-badness (tsar boundary)))
  (setf (slot-value boundary 'demerits)
	;; #### NOTE: see TeX's use of the artificial_demerits flag in this
	;; situation (#854, #855).
	(if (numberp (badness boundary))
	  (local-demerits (badness boundary) (penalty boundary) *line-penalty*)
	  0))
  (when (and extra (not (zerop extra)))
    (setf (slot-value boundary 'tsar)
	  ;; #### NOTE: the WIDTH slot from the FIXED-BOUNDARY superclass is
	  ;; already initialized by now, but we're still saving a reader call
	  ;; by using the propagated NATURAL-WIDTH keyword argument.
	  (sar width target stretch shrink))))

(defmethod properties strnlcat ((boundary kp-boundary) &key)
  "Advertise Knuth-Plass BOUNDARY's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A; Badness: ~A; Demerits: ~A (line)."
    (fitness-class-name (fitness-class boundary))
    ($float (badness boundary))
    (float (demerits boundary))))


;; -----
;; Lines
;; -----

;; #### TODO: it's probably a bad idea to call the cumulative demerits just
;; "demerits".
(defclass kp-line (line)
  ((demerits
    :documentation "The cumulative demerits so far in the layout."
    :initarg :demerits :reader demerits))
  (:documentation "The Knuth-Plass Line class."))

(defmethod properties strnlcat ((line kp-line) &key)
  "Return a string advertising Knuth-Plass LINE's cumulative demerits."
  (format nil "Cumulative demerits: ~A." (float (demerits line))))

;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce elastic
;; underfulls (in case of an impossible layout, it falls back to overfull
;; boxes). This means that the overstretch option has no effect, but it allows
;; for a nice trick: we can indicate lines exceeding the tolerance thanks to
;; an emergency stretch as overstretched, regardless of the option. This is
;; done by setting the overstretch parameter to T and not counting emergency
;; stretch in the stretch tolerance below.

(defun kp-make-justified-line
    (harray bol boundary stretch-tolerance overshrink demerits
     &rest keys &key previous
     &aux (tsar (tsar boundary)))
  "KP version of `make-line' for justified lines.
By default, this function instantiates a KP-LINE. The dynamic version will
however call this function with a PREVIOUS node, in which case a KP-NODE is
instantiated instead."
  (multiple-value-bind (asar esar)
      (sars tsar
	:stretch-tolerance stretch-tolerance
	:overshrink overshrink
	:overstretch t)
    (apply #'make-instance
      (if previous 'kp-node 'kp-line) ; kp-node forward reference
      :harray harray :bol bol :boundary boundary
      :asar asar :esar esar :demerits demerits
      keys)))

;; #### NOTE: there's no need for a KP-PINNED-NODE because when pinning lines,
;; we don't care about the previous one anymore. Thus, we can safely
;; CHANGE-CLASS a KP-NODE into a KP-PINNED-LINE, thereby dropping the PREVIOUS
;; slot.
(defclass kp-pinned-line (kp-line pin)
  ()
  (:documentation "The Knuth-Plass Pinned Line class."))


;; -------
;; Layouts
;; -------

;; #### NOTE: the layout's demerits is in fact the demerits of the last line.
(defclass kp-layout (layout)
  ((pinned-line-class :initform 'kp-pinned-line) ; slot override
   (demerits
    :documentation "This layout's total demerits."
    :initarg :demerits :reader demerits)
   (size
    :documentation "This layout's size (i.e., the number of lines)."
    :initform 1 :initarg :size :reader size))
  (:documentation "The Knuth-Plass Layout class."))

;; #### NOTE: we only advertise the layout's demerits for now. The other
;; properties (also including the bads in the graph variant subclass) are here
;; for sorting the layouts from best to worse.
(defmethod properties strnlcat ((layout kp-layout) &key)
  "Return a string advertising Knuth-Plass LAYOUT's demerits."
  (format nil "Demerits: ~A." (float (demerits layout))))

(defun kp-remove-unloose-layouts (layouts)
  "Return the subset of Knuth-Plass LAYOUTS conforming to *LOOSENESS*.
The looseness is supposed to be non-zero, and the layouts are supposed to be
already sorted, such that the natural size of the original solution is the
size of the first layout."
  (let ((ideal-size (+ (size (first layouts)) *looseness*)))
    (remove-if-not (lambda (size) (= size ideal-size)) layouts :key #'size)))

(defun kp-sort-layouts-by-looseness (layouts)
  "Return Knuth-Plass LAYOUTS sorted by conformance to *LOOSENESS*.
The looseness is supposed to be non-zero, and the layouts are supposed to be
already sorted, such that the natural size of the original solution is the
size of the first layout."
  (let ((ideal-size (+ (size (first layouts)) *looseness*)))
    (stable-sort layouts (lambda (size1 size2)
			   (< (abs (- size1 ideal-size))
			      (abs (- size2 ideal-size))))
		 :key #'size)))


;; ----------------------
;; Breakup specialization
;; ----------------------

(defclass kp-breakup-mixin ()
  ((pass
    :documentation "Which of the 3 passes produced this breakup."
    :initform 0 :reader pass))
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

;; -----------------
;; Boundaries lookup
;; -----------------

(defun kp-get-boundaries
    (harray bol width threshold &optional hyphenate final emergency-stretch)
  "Get boundaries for an HARRAY line of WIDTH beginning at BOL.
This is the Knuth-Plass version for the graph variant.
- THRESHOLD is the pre-tolerance or tolerance, depending on the pass.
- HYPHENATE means consider hyphenation points as potential breaks. It is NIL
  for pass 1 (the default), and T for passes 2 and 3.
- FINAL means this is the final pass, in which case this function is required
  to return a boundary, albeit unfit.
- EMERGENCY-STRETCH may be available during a final third pass."
  (loop :with boundaries :with overfull :with emergency-boundary
	:with continue := t
	:for eol := (next-break-point harray bol)
	  :then (next-break-point harray eol)
	:while (and eol continue)
	:when (and ($< (penalty eol) +∞)
		   (or hyphenate (not (hyphenation-point-p eol))))
	  :do (let ((boundary (make-instance 'kp-boundary
				:harray harray :bol bol :break-point eol
				:target width
				:extra emergency-stretch)))
		(when (eq (penalty eol) -∞) (setq continue nil))
		(cond ((> (min-width boundary) width)
		       (setq overfull boundary continue nil))
		      (($<= (badness boundary) threshold)
		       (push boundary boundaries))
		      (t
		       (setq emergency-boundary boundary))))
	:finally (return (or boundaries
			     (when final
			       (list (or overfull emergency-boundary)))))))


;; -------
;; Layouts
;; -------

;; #### NOTE: in the graph variant, we need to keep the bads around for
;; sorting. See comment about that in KP-GRAPH-BREAK-HARRAY.
(defclass kp-graph-layout (kp-layout)
  ((bads
    :documentation "This layout's number of bad lines."
    :initarg :bads :reader bads))
  (:documentation "The Knuth-Plass Graph Layout class."))

(defun kp-graph-make-layout
    (breakup path
     &aux (harray (harray breakup))
	  (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (overshrink (getf (disposition-options disposition) :overshrink))
	  ;; #### NOTE: no emergency stretch counted here. See comment on top
	  ;; of KP-MAKE-JUSTIFIED-LINE.
	  (stretch-tolerance
	   (stretch-tolerance
	    (if (> (pass breakup) 1) *tolerance* *pre-tolerance*)))
	  (make-line
	   (case disposition-type
	     (:justified
	      (lambda (harray bol boundary demerits)
		(kp-make-justified-line harray bol boundary
		  stretch-tolerance overshrink demerits)))
	     (t ;; just switch back to normal spacing.
	      (lambda (harray bol boundary demerits)
		(make-instance 'kp-line
		  :harray harray :bol bol :boundary boundary
		  :demerits demerits)))))
	  (layout (make-instance 'kp-graph-layout
		    :breakup breakup
		    :demerits (demerits (first path))
		    :bads (if (numberp (badness (first path))) 0  1))))
  "Create a Knuth-Plass layout for BREAKUP from graph PATH."
  ;; See warning in KP-CREATE-NODES about that.
  (incf (slot-value layout 'demerits)
	(kp-fitness-demerits (fitness-class (first path)) 0))
  (setf (slot-value layout 'lines)
	(cons (funcall make-line harray *bop* (first path) (demerits layout))
	      (when (cdr path)
		(with-slots (demerits bads size) layout
		  (loop :for boundary1 := (first path) :then boundary2
			:for boundary2 :in (cdr path)
			:for finalp := (eopp boundary2)
			:do (incf size)
			:unless (numberp (badness boundary2)) :do (incf bads)
			  :do (incf demerits (demerits boundary2))
			      ;; See comment in dynamic version. Do not
			      ;; consider the very rare case where the
			      ;; paragraph ends with an explicit hyphen.
			:when (and (not finalp)
				   (hyphenated boundary1)
				   (hyphenated boundary2))
			  :do (incf demerits *double-hyphen-demerits*)
			:when (and finalp (hyphenated boundary1))
			  :do (incf demerits *final-hyphen-demerits*)
			:do (incf demerits (kp-fitness-demerits
					    (fitness-class boundary1)
					    (fitness-class boundary2)))
			:collect (funcall make-line
				   harray
				   (break-point boundary1) boundary2
				   demerits))))))
  layout)

;; #### WARNING: the final pass of the graph variant may construct a graph
;; containing a mix of fit and unfit paths, because we can't know in advance
;; whether there will be an actual solution or not (in other words,
;; KP-GET-BOUNDARIES works in a preventive manner in the final pass). Just
;; like in the dynamic variant, and just like in TeX (#855), an unfit line
;; will have its local demerits set to 0. Contrary to the dynamic version
;; however, it may turn out that an unfit path has fewer demerits than a fit
;; one (because of the zero'ed lines). Consequently, the layouts must be
;; sorted by number of bads first, and demerits next.
(defun kp-graph-layout-< (layout1 layout2)
  "Return T if Knuth-Plass graph LAYOUT1 is better than LAYOUT2.
A layout is considered better if it has a lesser number of bads,
or, in case of equality, a lesser amount of demerits."
  (or (< (bads layout1) (bads layout2))
      (and (= (bads layout1) (bads layout2))
	   (< (demerits layout1) (demerits layout2)))))


;; -------
;; Breakup
;; -------

;; #### NOTE: the Knuth-Plass breakup mixin comes first for proper ordering of
;; the displayed properties.
(defclass kp-graph-breakup (kp-breakup-mixin graph-breakup)
  ()
  (:documentation "The Knuth-Plass Graph Breakup class."))

;; #### NOTE: according to #872, TeX will attempt to match a non-zero
;; looseness exactly, or try another pass unless it's already the final one. I
;; got that wrong for quite some time...
(defun kp-graph-break-lineup
    (lineup width
     &optional (make-layout #'kp-graph-make-layout)
     &aux (harray (harray lineup)) breakup)
  "Break LINEUP for paragraph WIDTH with the unoptimized Knuth-Plass algorithm."
  (setq breakup (make-instance 'kp-graph-breakup
		  :lineup lineup :paragraph-width width))
  (unless (zerop (length harray))
    (let (graph layouts)

      ;; Pass 1, never final.
      (when ($<= 0 *pre-tolerance*)
	(setf (slot-value breakup 'pass) 1)
	(setq graph (make-graph harray width
				(lambda (harray bol width)
				  (kp-get-boundaries
				   harray bol width *pre-tolerance*))))
	(when (gethash *bop* graph)
	  (setq layouts (sort (make-graph-layouts graph breakup make-layout)
			    #'< :key #'demerits))
	  (unless (zerop *looseness*)
	    (setq layouts (kp-remove-unloose-layouts layouts)))))

      ;; Pass 2, maybe final.
      (unless layouts
	(let ((final (zerop *emergency-stretch*)))
	  (setf (slot-value breakup 'pass) 2)
	  (setq graph (make-graph harray width
				  (lambda (harray bol width)
				    (kp-get-boundaries
				     harray bol width *tolerance* t final))))
	  (when (gethash *bop* graph)
	    (cond (final
		   (setq layouts
			 (sort (make-graph-layouts graph breakup make-layout)
			     #'kp-graph-layout-<))
		   (unless (zerop *looseness*)
		     (setq layouts (kp-sort-layouts-by-looseness layouts))))
		  (t
		   (setq layouts
			 (sort (make-graph-layouts graph breakup make-layout)
			     #'< :key #'demerits))
		   (unless (zerop *looseness*)
		     (setq layouts (kp-remove-unloose-layouts layouts))))))))

      ;; Pass 3, always final.
      (unless layouts
	(incf (slot-value breakup 'pass))
	(setq graph (make-graph harray width
				(lambda (harray bol width)
				  (kp-get-boundaries
				   harray bol width *tolerance*
				   t t *emergency-stretch*))))
	(setq layouts
	      (sort (make-graph-layouts graph breakup make-layout)
		  #'kp-graph-layout-<))
	(unless (zerop *looseness*)
	  (setq layouts (kp-sort-layouts-by-looseness layouts))))

      ;; We're done here.
      (setf (slot-value breakup 'graph) graph)
      (setf (slot-value breakup 'layouts)
	    (make-array (length layouts) :initial-contents layouts))))
  breakup)




;; ==========================================================================
;; Dynamic Variant
;; ==========================================================================

;; A node in the Knuth-Plass terminology is really just a line in ours, plus a
;; pointer to the previous one.
(defclass kp-node (kp-line)
  ((previous :documentation "This node's previous node."
	     :initform nil :initarg :previous :reader previous))
  (:documentation "The KP Node class."))

;; The active nodes hash table is accessed by
;; key = (break-point line-number fitness-class)
(defun make-key (break-point line-number fitness-class)
  (list break-point line-number fitness-class))
(defun key-break-point (key) (first key))
(defun key-line-number (key) (second key))
(defun key-fitness-class (key) (third key))


;; ---------------
;; Boundary lookup
;; ---------------

(defun kp-try-break
    (break-point nodes harray width make-node threshold final emergency-stretch
     &aux last-deactivation new-nodes)
  "Examine BREAK-POINT and update active NODES accordingly."
  (maphash
   (lambda (key node
	    &aux (bol (key-break-point key)) ; also available in the node
		 (boundary (make-instance 'kp-boundary
			     :harray harray :bol bol :break-point break-point
			     :target width :extra emergency-stretch)))
     ;; #### WARNING: we must deactivate all nodes when we reach the
     ;; paragraph's end. TeX does this by adding a forced break at the end but
     ;; this is a "dangling" penalty, whereas ours are properties of break
     ;; points. This is why we need to check explicitly for the EOP below.
     (when (or ($< (tsar boundary) -1)
	       (eq (penalty boundary) -∞)
	       (eopp boundary))
       (setq last-deactivation (cons key node))
       (remhash key nodes))
     (when (and ($<= -1 (tsar boundary)) ($<= (badness boundary) threshold))
       (let ((demerits (+ (demerits node) (demerits boundary))))
	 ;; #### WARNING: we must use the key's fitness class rather than the
	 ;; node's one below, as accessing the node's one would break on
	 ;; *KP-BOP-NODE*. Besides, we also save a couple of accessor calls
	 ;; that way.
	 (incf demerits (kp-fitness-demerits
			 (fitness-class boundary)
			 (key-fitness-class key)))
	 ;; #### NOTE: according to #859, TeX doesn't consider the admittedly
	 ;; very rare and weird case where a paragraph would end with an
	 ;; explicit hyphen. As stipulated in #829, for the purpose of
	 ;; computing demerits, the end of the paragraph is always regarded as
	 ;; virtually hyphenated, and in case the previous line (really) is
	 ;; hyphenated, the value of final-hyphen-demerits is used instead of
	 ;; double-hyphen-demerits. One could consider using
	 ;; double-hyphen-demerits when there actually is a final hyphen, but
	 ;; on the other hand, the final line is rarely justified so the two
	 ;; hyphens are very unlikely to create a ladder.
	 (when (discretionaryp bol)
	   (if (eopp boundary)
	     (incf demerits *final-hyphen-demerits*)
	     (when (discretionaryp (break-point boundary))
	       (incf demerits *double-hyphen-demerits*))))
	 (let* ((new-key (make-key break-point
				   (1+ (key-line-number key))
				   (fitness-class boundary)))
		(previous (find new-key new-nodes :test #'equal :key #'car))
		(new-node
		  (when (or (not previous)
			    ;; #### NOTE: the inclusive inequality below is
			    ;; conformant with what TeX does in #855.
			    ;; Concretely, it makes the KP algorithm greedy in
			    ;; some sense: in case of demerits equality, TeX
			    ;; keeps the last envisioned solution. On the
			    ;; other hand, we're in fact not doing exactly the
			    ;; same thing because we're using MAPHASH and the
			    ;; order of the nodes in the hash table is not
			    ;; deterministic.
			    (<= demerits (demerits (cdr previous))))
		    (funcall make-node harray bol boundary demerits node))))
	   (when new-node
	     (if previous
	       (setf (cdr previous) new-node)
	       (push (cons new-key new-node) new-nodes)))))))
   nodes)
  (when (and final (zerop (hash-table-count nodes)) (null new-nodes))
    (let* ((bol (key-break-point (car last-deactivation)))
	   (boundary (make-instance 'kp-boundary
		       :harray harray
		       :bol bol
		       :break-point break-point
		       :target width
		       :extra emergency-stretch)))
      ;; #### NOTE: in this situation, TeX sets the local demerits to 0 by
      ;; checking the artificial_demerits flag (#854, #855). The KP-BOUNDARY
      ;; initialization protocol takes care of this. In any case, we can also
      ;; just reuse the previous total demerits in the new node.
      (setq new-nodes
	    (list
	     (cons (make-key break-point
			     (1+ (key-line-number (car last-deactivation)))
			     (fitness-class boundary))
		   (funcall make-node
		     harray bol boundary (demerits (cdr last-deactivation))
		     (cdr last-deactivation)))))))
  (mapc (lambda (new-node)
	  (setf (gethash (car new-node) nodes) (cdr new-node)))
    new-nodes))


;; In a similar way as we defined *BOP*, which is not really a break-point, we
;; define *KP-BOP-NODE* below as a special node which is not really a node.
;; The goal is the same: to make the code more readable by limiting
;; special-casing and simplifying stop conditions in loops notably.
;;
;; The structure of the BOP node is as follows:
;; - previous = NIL
;; - (total) demerits = 0
;; - boundary (base class only) with break-point = *BOP*.
;; - the rest is either uninitialized or doesn't matter.
;;
;; Note that we don't use a KP boundary because we can't let the
;; initialization protocol compute any of its properties (there's no actual
;; line here). It doesn't matter because we don't need to access those, except
;; for the fitness-class. However, the fitness-class is retrieved from the
;; hash table key (see below). Also, *BOP* gives us the correct bol index, and
;; no hyphenation so no double hyphen demerits for the first line.

(defparameter *kp-bop-node*
  (make-instance 'kp-node
    :boundary (make-instance 'boundary :break-point *bop*)
    :demerits 0)
  "The Knuth-Plass (fake) beginning of paragraph node.")


;; The root hash table key under which we store the BOP node is defined as
;; follows:
;; - break point = *bop*
;; - line number = 0
;; fitness-class = 0 (decent).
;;
;; This is straightforward but the fitness class deserves a special comment.
;; TeX computes adjacent demerits even for the first line which doesn't really
;; have a previous line. This has the effect of negatively weighting very
;; loose first lines. This is why we need the appropriate (fake) fitness class
;; to begin with. See https://tug.org/pipermail/texhax/2023-May/026091.html.

(defparameter *kp-bop-key* (make-key *bop* 0 0)
  "The Knuth-Plass beginning of paragraph hash table key.")


(defun kp-create-nodes
    (breakup
     &aux (harray (harray breakup))
	  (pass (pass breakup))
	  (width (paragraph-width breakup))
	  (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (overshrink (getf (disposition-options disposition) :overshrink))
	  (threshold (if (> pass 1) *tolerance* *pre-tolerance*))
	  ;; #### NOTE: no emergency stretch counted here. See comment on top
	  ;; of KP-MAKE-JUSTIFIED-LINE.
	  (stretch-tolerance (stretch-tolerance threshold))
	  (hyphenate (> pass 1))
	  (final (case pass
		   (1 nil)
		   (2 (zerop *emergency-stretch*))
		   (3 t)))
	  (emergency-stretch (when final *emergency-stretch*))
	  (nodes (make-hash-table :test #'equal))
	  (make-node
	   (case disposition-type
	     (:justified
	      (lambda (harray bol boundary demerits previous)
		(kp-make-justified-line harray bol boundary
		  stretch-tolerance overshrink demerits :previous previous)))
	     (t ;; just switch back to normal spacing.
	      (lambda (harray bol boundary demerits previous)
		(make-instance 'kp-node
		  :harray harray :bol bol :boundary boundary
		  :demerits demerits :previous previous))))))
  "Create Knuth-Plass BREAKUP's dynamic nodes."
  (setf (gethash *kp-bop-key* nodes) *kp-bop-node*)
  (loop :for break-point := (next-break-point harray)
	  :then (next-break-point harray break-point)
	:while break-point
	:when (and ($< (penalty break-point) +∞)
		   (or hyphenate (not (hyphenation-point-p break-point))))
	  :do (kp-try-break break-point nodes harray width make-node
			    threshold final emergency-stretch))
  (unless (zerop (hash-table-count nodes)) nodes))


;; -------
;; Layouts
;; -------

;; #### NOTE: a dynamic layout is trivial to create because the dynamic
;; implementation essentially returns lines in last-to-first order. The SIZE
;; argument is directly available from the hash table keys, and avoids
;; the need for counting the lines.
(defun kp-dynamic-make-layout (breakup node size)
  "Create a Knuth-Plass layout for BREAKUP from dynamic NODE of SIZE lines."
  (make-instance 'kp-layout
    :breakup breakup
    :lines (loop :with lines
		 :for line := node :then (previous line)
		 ;; #### NOTE: testing for a previous node instead of
		 ;; comparing the node to *KP-BOP-NODE* allows this function
		 ;; to be reused by the KPX algorithm.
		 :while (previous line)
		 :do (push line lines)
		 :finally (return lines))
    :demerits (demerits node)
    :size size))

;; #### NOTE: contrary to the graph variant, we can't have a mix of good and
;; bad layouts here, so we can directly sort them by demerits.
(defun kp-dynamic-make-layouts (breakup nodes)
  "Make Knuth-Plass NODES layouts for BREAKUP. Return them sorted by demerits."
  (sort (loop :for key :being :the :hash-keys :in nodes :using (hash-value node)
	      :for size := (key-line-number key)
	      :collect (kp-dynamic-make-layout breakup node size))
      #'< :key #'demerits))


;; -------
;; Breakup
;; -------

;; #### NOTE: given the fact that the dynamic implementation simply constructs
;; layouts as they go, keeping the best ones, there's no point in keeping the
;; nodes around in the breakup, not even for properties advertisement. Indeed,
;; nodes boil down to the last lines of each layout.

(defclass kp-dynamic-breakup (kp-breakup-mixin breakup)
  ()
  (:documentation "The Knuth-Plass Dynamic Breakup class."))

;; #### NOTE: according to #872, TeX will attempt to match a non-zero
;; looseness exactly, or try another pass unless it's already the final one. I
;; got that wrong for quite some time...
(defun kp-dynamic-break-lineup
    (lineup width &optional (create-nodes #'kp-create-nodes) &aux breakup)
  "Break LINEUP for paragraph WIDTH with the optimized Knuth-Plass algorithm."
  (setq breakup (make-instance 'kp-dynamic-breakup
		  :lineup lineup :paragraph-width width))
  (unless (zerop (length (harray lineup)))
    (let (nodes layouts)

      ;; Pass 1, never final.
      (when ($<= 0 *pre-tolerance*)
	(setf (slot-value breakup 'pass) 1)
	(setq nodes (funcall create-nodes breakup))
	(when nodes
	  (setq layouts (kp-dynamic-make-layouts breakup nodes))
	  (unless (zerop *looseness*)
	    (setq layouts (kp-remove-unloose-layouts layouts)))))

      ;; Pass 2, maybe final.
      (unless layouts
	(let ((final (zerop *emergency-stretch*)))
	  (setf (slot-value breakup 'pass) 2)
	  (setq nodes (funcall create-nodes breakup))
	  (when nodes
	    (setq layouts (kp-dynamic-make-layouts breakup nodes))
	    (unless (zerop *looseness*)
	      (setq layouts
		    (if final
		      (kp-sort-layouts-by-looseness layouts)
		      (kp-remove-unloose-layouts layouts)))))))

      ;; Pass 3, always final.
      (unless nodes
	(incf (slot-value breakup 'pass))
	(setq nodes (funcall create-nodes breakup))
	(setq layouts (kp-dynamic-make-layouts breakup nodes))
	(unless (zerop *looseness*)
	  (setq layouts (kp-sort-layouts-by-looseness layouts))))

      ;; We're done here.
      (setf (slot-value breakup 'layouts)
	    (make-array (length layouts) :initial-contents layouts))))
  breakup)




;; ==========================================================================
;; Variant Dispatch
;; ==========================================================================

(defmethod break-lineup
    (lineup width (algorithm (eql :knuth-plass))
     &key ((:variant *variant*))
	  ((:line-penalty *line-penalty*))
	  ((:adjacent-demerits *adjacent-demerits*))
	  ((:double-hyphen-demerits *double-hyphen-demerits*))
	  ((:final-hyphen-demerits *final-hyphen-demerits*))
	  ((:pre-tolerance *pre-tolerance*))
	  ((:tolerance *tolerance*))
	  ((:emergency-stretch *emergency-stretch*))
	  ((:looseness *looseness*)))
  "Break LINEUP for paragraph WIDTH with the Knuth-Plass algorithm."
  (default-kp variant)
  (calibrate-kp line-penalty)
  (calibrate-kp adjacent-demerits)
  (calibrate-kp double-hyphen-demerits)
  (calibrate-kp final-hyphen-demerits)
  (calibrate-kp pre-tolerance)
  (calibrate-kp tolerance)
  (calibrate-kp emergency-stretch)
  (calibrate-kp looseness)
  (funcall (ecase *variant*
	     (:graph #'kp-graph-break-lineup)
	     (:dynamic #'kp-dynamic-break-lineup))
    lineup width))
