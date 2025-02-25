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

;; #### WARNING: although we have a specific hierarchy for hyphenation points,
;; the Knuth-Plass applies hyphen penalties to all discretionaries, so we do
;; the same here.

(defmethod process-hlist
    (hlist disposition (algorithm (eql :knuth-plass))
     &key ((:hyphen-penalty *hyphen-penalty*))
	  ((:explicit-hyphen-penalty *explicit-hyphen-penalty*)))
  "Adjust hyphen penalties in HLIST, and append the final glue to it."
  (calibrate-kp hyphen-penalty t)
  (calibrate-kp explicit-hyphen-penalty t)
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

(defmethod properties strnlcat ((boundary kp-boundary) &key)
  "Advertise Knuth-Plass BOUNDARY's fitness class, badness, and demerits."
  (format nil "Fitness class: ~A; Badness: ~A; Demerits: ~A (line)."
    (fitness-class-name (fitness-class boundary))
    ($float (badness boundary))
    ($float (demerits boundary))))


;; Boundaries lookup

(defun kp-get-boundaries
    (harray bol width threshold
     &optional hyphenate final
     &aux (emergency-stretch (when (numberp final) final)))
  "Get boundaries for an HARRAY line of WIDTH beginning at BOL.
This is the Knuth-Plass version for the graph variant.
- THRESHOLD is the pre-tolerance or tolerance, depending on the pass.
- HYPHENATE means consider hyphenation points as potential breaks. It is NIL
  for pass 1 (the default), and T for passes 2 and .3
- FINAL means this is the final pass. If FINAL is NIL, we're in pass 1. If
  FINAL is T, we're in pass 2 and there is no emergency stretch. Otherwise,
  FINAL is a non-zero value (the emergency stretch) and we're in pass 3."
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
		      (($<= (scale-badness (scale boundary)) threshold)
		       (push boundary boundaries))
		      (t
		       (setq emergency-boundary boundary))))
	:finally (return (or boundaries
			     (when final
			       (list (or overfull emergency-boundary)))))))


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
  (format nil "Cumulative demerits: ~A." ($float (demerits line))))

;; #### NOTE: I think that the Knuth-Plass algorithm cannot produce elastic
;; underfulls (in case of an impossible layout, it falls back to overfull
;; boxes). This means that the overstretch option has no effect, but it allows
;; for a nice trick: we can indicate lines exceeding the tolerance thanks to
;; an emergency stretch as overstretched, regardless of the option. This is
;; done by setting the overstretch parameter to T and not counting emergency
;; stretch in the stretch tolerance below.

(defun kp-make-justified-line
    (harray bol boundary demerits stretch-tolerance overshrink
     &aux (scale (scale boundary)))
  "KP version of `make-ledge-line' for justified lines."
  (multiple-value-bind (theoretical effective)
      (actual-scales scale
	:stretch-tolerance stretch-tolerance
	:overshrink overshrink
	:overstretch t)
    (make-instance 'kp-line
      :harray harray :bol bol :boundary boundary
      :scale theoretical
      :effective-scale effective
      :demerits demerits)))


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
   (bads
    :documentation "This layout's number of bad lines."
    :initarg :bads :reader bads)
   (size
    :documentation "This layout's size (i.e. the number of lines)."
    :initform 1 :reader size))
  (:documentation "The KP-LAYOUT class."))

;; #### NOTE: we only advertise the layout's demerits for now. The other
;; properties are here for sorting the layouts from best to worse.
(defmethod properties strnlcat ((layout kp-layout) &key)
  "Return a string advertising Knuth-Plass LAYOUT's demerits."
  (format nil "Demerits: ~A." ($float (demerits layout))))

(defun kp-make-layout
    (breakup path
     &aux (harray (harray breakup))
	  (disposition (disposition breakup))
	  (disposition-type (disposition-type disposition))
	  (overshrink (getf (disposition-options disposition) :overshrink))
	  ;; #### NOTE: no emergency stretch counted here. See comment on top
	  ;; of KP-MAKE-JUSTIFIED-LINE.
	  (stretch-tolerance (if (> (pass breakup) 1)
			       (tolerance breakup)
			       (pre-tolerance breakup)))
	  (make-line (case disposition-type
		       (:justified
			(lambda (harray bol boundary demerits)
			  (kp-make-justified-line
			   harray bol boundary
			   demerits stretch-tolerance overshrink)))
		       (t ;; just switch back to normal spacing.
			(lambda (harray bol boundary demerits)
			  (make-instance 'kp-line
			    :harray harray
			    :bol bol
			    :boundary boundary
			    :demerits demerits)))))
	  (layout (make-instance 'kp-layout
		    :breakup breakup
		    :demerits (demerits (first path))
		    :bads (if (numberp (badness (first path))) 0  1)))
	  line1)
  "Create a Knuth-Plass layout for BREAKUP from PATH."
  ;; See warning in KP-CREATE-NODES about that.
  (when (zerop (fitness-class (first path)))
    (setf (slot-value layout 'demerits)
	  ($+ (slot-value layout 'demerits) *adjacent-demerits*)))
  (setq line1 (funcall make-line harray *bop* (first path) (demerits layout)))
  (when (cdr path)
    (with-slots (demerits bads size) layout
      (loop :for boundary1 := (first path) :then boundary2
	    :for boundary2 :in (cdr path)
	    :for finalp := (eopp boundary2)
	    :do (incf size)
	    :unless (numberp (badness boundary2)) :do (incf bads)
	    :do (setf demerits ($+ demerits (demerits boundary2)))
	    ;; See comment in dynamic version. Do not consider the very
	    ;; rare case where the paragraph ends with an explicit hyphen.
	    :when (and (not finalp)
		       (hyphenated boundary1)
		       (hyphenated boundary2))
	      :do (setf demerits ($+ demerits *double-hyphen-demerits*))
	    :when (and finalp (hyphenated boundary1))
	      :do (setf demerits ($+ demerits *final-hyphen-demerits*))
	    :when (> (abs (- (fitness-class boundary1)
			     (fitness-class boundary2)))
		     1)
	      :do (setf demerits ($+ demerits *adjacent-demerits*))
	    :collect (funcall make-line
		       harray (break-point boundary1) boundary2 demerits)
	      :into lines
	    :finally (setf (slot-value layout 'lines) (cons line1 lines)))))
  layout)


;; -------
;; Breakup
;; -------

;; #### NOTE: the KP Breakup Mixin comes first for proper ordering of the
;; displayed properties.
(defclass kp-graph-breakup (kp-breakup-mixin graph-breakup)
  ()
  (:documentation "The Knuth-Plass Graph Breakup class."))

(defun kp-graph-break-harray
    (harray disposition width
     &aux (breakup (make-instance 'kp-graph-breakup
		     :harray harray :disposition disposition :width width
		     :pre-tolerance *pre-tolerance* :tolerance *tolerance*)))
  "Break HARRAY with the Knuth-Plass algorithm, graph version."
  (unless (zerop (length harray))
    (let (graph layouts)
      (setf (slot-value breakup 'pass) 1)
      (when ($<= 0 *pre-tolerance*)
	(setq graph (make-graph harray width
				(lambda (harray bol width)
				  (kp-get-boundaries
				   harray bol width *pre-tolerance*)))))
      (unless (and graph (gethash *bop* graph))
	(incf (slot-value breakup 'pass))
	(setq graph (make-graph harray width
				(lambda (harray bol width)
				  (kp-get-boundaries
				   harray bol width *tolerance*
				   t (zerop *emergency-stretch*))))))
      (unless (gethash *bop* graph)
	(incf (slot-value breakup 'pass))
	(setq graph (make-graph harray width
				(lambda (harray bol width)
				  (kp-get-boundaries
				   harray bol width *tolerance*
				   t *emergency-stretch*)))))
      (setf (slot-value breakup 'graph) graph)
      (setq layouts (mapcar (lambda (path) (kp-make-layout breakup path))
		      (make-graph-paths graph)))
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
      (setf (slot-value breakup 'layouts)
	    (make-array (length layouts) :initial-contents layouts))))
  breakup)




;; ==========================================================================
;; Dynamic Variant
;; ==========================================================================

;; A node in the Knuth-Plass terminology is really just a ledge in ours, plus
;; a pointer to the previous one.
(defclass kp-node (kp-ledge)
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
    (break-point nodes harray width threshold final
     &aux (emergency-stretch (when (numberp final) final))
	  last-deactivation new-nodes)
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
     (when (or ($< (scale boundary) -1)
	       (eq (penalty boundary) -∞)
	       (eopp boundary))
       (setq last-deactivation (cons key node))
       (remhash key nodes))
     (when (and ($<= -1 (scale boundary)) ($<= (badness boundary) threshold))
       (let ((total-demerits ($+ (demerits node) (demerits boundary))))
	 ;; #### WARNING: we must use the key's fitness class rather than the
	 ;; node's one below, as accessing the node's one would break on
	 ;; *KP-BOP-NODE*. Besides, we also save a couple of accessor calls
	 ;; that way.
	 (when (> (abs (- (fitness-class boundary) (key-fitness-class key))) 1)
	   (setq total-demerits ($+ total-demerits *adjacent-demerits*)))
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
	     (setq total-demerits ($+ total-demerits *final-hyphen-demerits*))
	     (when (discretionaryp (break-point boundary))
	       (setq total-demerits
		     ($+ total-demerits *double-hyphen-demerits*)))))
	 (let* ((new-key (make-key break-point
				   (1+ (key-line-number key))
				   (fitness-class boundary)))
		(previous (find new-key new-nodes :test #'equal :key #'car)))
	   (if previous
	     ;; #### NOTE: the inclusive inequality below is conforment
	     ;; with what TeX does in #855. Concretely, it makes the KP
	     ;; algorithm greedy in some sense: in case of demerits
	     ;; equality, TeX keeps the last envisioned solution. On the
	     ;; other hand, we're in fact not doing exactly the same thing
	     ;; because we're using MAPHASH and the order of the nodes in
	     ;; the hash table is not deterministic.
	     (when ($<= total-demerits (demerits (cdr previous)))
	       (reinitialize-instance (cdr previous)
		 :boundary boundary :demerits total-demerits :previous node))
	     (push (cons new-key
			 (make-instance 'kp-node
			   :boundary boundary
			   :demerits total-demerits
			   :previous node))
		   new-nodes))))))
   nodes)
  (when (and final (zerop (hash-table-count nodes)) (null new-nodes))
    (let ((boundary (make-instance 'kp-boundary
		      :harray harray
		      :bol (key-break-point (car last-deactivation))
		      :break-point break-point
		      :target width
		      :extra emergency-stretch)))
      ;; #### NOTE: in this situation, TeX sets the local demerits to 0 (#855)
      ;; by checking the artificial_demerits flag. The KP-BOUNDARY
      ;; initialization protocol takes care of this (although I should verify
      ;; that we can indeed rely on infinite badness to detect the situation).
      ;; In any case, we can also just reuse the previous total demerits in
      ;; the new node.
      (setq new-nodes
	    (list
	     (cons (make-key break-point
			     (1+ (key-line-number (car last-deactivation)))
			     (fitness-class boundary))
		   (make-instance 'kp-node
		     :boundary boundary
		     :demerits (demerits (cdr last-deactivation))
		     :previous (cdr last-deactivation)))))))
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
;;
;; Note that we don't use a KP boundary because we can't compute any of its
;; properties (there's no actual line here). It doesn't matter because we
;; don't need to access those, except for the fitness-class. However, the
;; fitness-class is retrieved from the hash table key (see below). Also, *BOP*
;; gives us the correct bol index, and no hyphenation so no double hyphen
;; demerits for the first line.

(defvar *kp-bop-node*
  (make-instance 'kp-node
    :boundary (make-instance 'boundary :break-point *bop*))
  "The Knuth-Plass (fake) beginning of paragraph node.")


;; The root hash table key under which we store the BOP node is defined as
;; follows:
;; - break point = *bop*
;; - line number = 0
;; fitness-class = 2 (decent).
;;
;; This is straightforward but the fitness class deserves a special comment.
;; TeX computes adjacent demerits even for the first line which doesn't really
;; have a previous line. This has the effect of negatively weighting very
;; loose first lines. This is why we need the appropriate (fake) fitness class
;; to begin with. See https://tug.org/pipermail/texhax/2023-May/026091.html.

(defvar *kp-bop-key* (make-key *bop* 0 2)
  "The Knuth-Plass beginning of paragraph hash table key.")


(defun kp-create-nodes (harray width pass)
  "Break HARRAY for a paragraph of WIDTH with PASS number of the Knuth-Plass."
  (let ((hyphenate (> pass 1))
	(threshold (if (> pass 1) *tolerance* *pre-tolerance*))
	(final (case pass
		 (1 nil)
		 (2 (zerop *emergency-stretch*))
		 (3 *emergency-stretch*)))
	(nodes (make-hash-table :test #'equal)))
    (setf (gethash *kp-bop-key* nodes) *kp-bop-node*)
    (loop :for break-point := (next-break-point harray)
	    :then (next-break-point harray break-point)
	  :while break-point
	  :when (and ($< (penalty break-point) +∞)
		     (or hyphenate (not (hyphenation-point-p break-point))))
	    :do (kp-try-break break-point nodes harray width threshold final))
    (unless (zerop (hash-table-count nodes)) nodes)))


;; -------
;; Breakup
;; -------

(defclass kp-dynamic-breakup (kp-breakup-mixin breakup)
  ((harray
    :documentation "This breakup's harray."
    :initarg :harray :reader harray)
   (nodes
    :documentation "This breakup's sorted nodes array."
    :initform nil :reader nodes)
   (renditions
    :documentation "This breakups' sorted renditions array."
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
  "Advertise Knuth-Plass dynamic BREAKUP's properties."
  (when nodes
    (strnlcat
     (format nil "Remaining active nodes: ~A." (length nodes))
     ;; #### NOTE: we don't want to use the PROPERTIES protocol on nodes here.
     ;; That's because breakup nodes are just terminal ledges (as opposed to
     ;; layouts in the graph version). We don't want to see ledges properties
     ;; here (they will appear in the pinned lines properties popup). So
     ;; instead we just advertise the node's cumulative demerits, which,
     ;; again, is the equivalent of a layout's demerits.
     (when rendition
       (format nil "Demerits: ~A."
	 ($float (demerits (aref nodes rendition)))))
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
		 (push (cons (key-line-number key) node) nodes-list))
	       nodes)
      (setq nodes-list
	    (sort nodes-list #'$< :key (lambda (elt) (demerits (cdr elt)))))
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
	 :for end := (aref (nodes breakup) nth) :then (previous end)
	 :for beg := (previous end)
	 :while beg
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
			 (actual-scales (scale end)
			   :stretch-tolerance stretch-tolerance
			   :overshrink overshrink :overstretch t)
		       (make-ledge-line
			harray (break-point (boundary beg)) end
			:scale theoretical :effective-scale effective))
		     (make-instance 'line
		       :harray harray
		       :bol (break-point (boundary beg))
		       :boundary (boundary end)))
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
