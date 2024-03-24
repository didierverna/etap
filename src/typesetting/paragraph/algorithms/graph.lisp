;; #### NOTE: with the default text, paragraph width, and all features, there
;; are 66576 paragraph solutions including going through under and overfull
;; lines (21096 without hyphenation). The raw tree of all such solutions has
;; 150860 nodes (48338 without hyphenation). However, once a line stop has
;; been decided, all possible solutions for the next lines remain the same
;; (modulo the rectangular paragraph assumption), however we reached that
;; possible stop. This means that there is a lot of room for re-using
;; branches. And indeed, when sharing nodes, we fall from 150860 to 98 (from
;; 48338 to 83 without hyphenation).

;; If we avoid preventive fulls, that is, if we include only under- and
;; overfull solutions when there is no fit, the number of paragraph solutions
;; falls to 37 (it actually raises up to 61 without hyphenation, all
;; mistfits). The raw tree of all such solutions has only 109 nodes (192
;; without hyphenations). Once shared, the actual number of nodes falls down
;; to 30 (33 without hyphenation).

(in-package :etap)

;; ==========================================================================
;; Graphs
;; ==========================================================================

;; -----
;; Nodes
;; -----

(defclass node ()
  ((boundary :documentation "This node's boundary."
	     :initarg :boundary
	     :reader boundary)
   (edges :documentation "The edges from this node to other nodes."
	  :initarg :edges
	  :reader edges))
  (:documentation "The NODE class.
This is the base class for representing a line break possibility in a graph
representation of paragraph breaking."))

(defmethod hyphenated ((node node))
  "Return NODE's hyphenation status."
  (hyphenated (boundary node)))

(defun make-node (boundary &optional edges)
  "Make a node at BOUNDARY, optionally followed by EDGES."
  (make-instance 'node :boundary boundary :edges edges))


;; -----
;; Edges
;; -----

(defclass edge ()
  ((destination :documentation "The node this edge points to."
		:initarg :destination
		:reader destination))
  ;; This is necessary to allow the code below to pass :harray, :start, and
  ;; :width along, even though this base class doesn't actually use them.
  (:default-initargs :allow-other-keys t)
  (:documentation "The EDGE class.
This is the base class for representing an edge to a node in a graph
representation of paragraph breaking."))

(defmethod hyphenated ((edge edge))
  "Return EDGE's hyphenation status."
  (hyphenated (destination edge)))


;; ------------------
;; Graph construction
;; ------------------

;; #### TODO: for experimentation, we could make PREVENTIVE-*FULLS a number
;; instead of just a Boolean, for keeping more than 1 *full.
(defun next-boundaries (harray start width &key fulls strict)
  "Find the possible endings for a line of WIDTH beginning at START in HARRAY.
If no possible ending is found, return NIL, unless FULLS, in which case return
the last underfull and the first overfull (if any) as a fallback solution.
If FULLS is :PREVENTIVE, also return these fulls even if possible endings were
found. If STRICT, consider that even the last line must fit exactly. Otherwise
(the default), consider a final underfull as a fit.
The possible endings are listed in reverse order (from last to first)."
  (loop :with underfull :with fits := (list) :with overfull
	:for boundary := (next-boundary harray start)
	  :then (next-boundary harray (stop-idx boundary))
	:while (and boundary (not overfull))
	:for (nil max min)
	  := (multiple-value-list
	      (harray-width harray start (stop-idx boundary)))
	:do (cond (($< max width)
		   (if (and (last-boundary-p boundary) (not strict))
		     (push boundary fits)
		     (setq underfull boundary)))
		  ((> min width) (setq overfull boundary))
		  (t (push boundary fits))) ;; note the reverse order
	:finally
	   (return (cond ((eq fulls :preventive)
			  (append (when overfull (list overfull))
				  fits
				  (when underfull (list underfull))))
			 (fulls
			  (or fits
			      (append (when overfull (list overfull))
				      (when underfull (list underfull)))))
			 (t fits)))))

;; #### WARNING: we use a hash table for storing and sharing nodes to express
;; the fact that however we reach a break, all subsequent solutions for the
;; rest of the paragraph will be the same. Note however that this is valid
;; only because we currently only have rectangular paragraphs. With bells and
;; whistles like \parshape, the subsequent solutions would differ, depending
;; on the line number at which a break occurs. Therefore, the sharing would
;; need to take the line number as well as the boundary into account.
(defun make-subgraph
    (harray boundary width graph edge-type
     next-boundaries-fn next-boundaries-args)
  "Make a solutions subgraph for breaking HARRAY's remainder starting at
BOUNDARY into a paragraph of WIDTH.

If no line breaking solution is found, return NIL. Otherwise, return the
subgraph's root node.

This function memoizes the computed subgraphs into a GRAPH hash table."
  ;; #### NOTE: the hash table may contain purposely null entries for some
  ;; keys. This indicates that an attempt at creating a sub-graph was made but
  ;; failed, so there's no point in trying again.
  (multiple-value-bind (value found) (gethash (stop-idx boundary) graph)
    (if found
      value
      (setf (gethash (stop-idx boundary) graph)
	    ;; #### NOTE: this may turn out to be NIL. See comment above.
	    (if (last-boundary-p boundary)
	      (make-node boundary)
	      (let ((nodes (loop :for boundary
				   :in (apply next-boundaries-fn
					 harray (start-idx boundary) width
					 next-boundaries-args)
				 :when (make-subgraph
					harray boundary width graph
					edge-type
					next-boundaries-fn
					next-boundaries-args)
				   :collect :it)))
		(when nodes
		  (make-node
		   boundary
		   (mapcar (lambda (node)
			     (make-instance edge-type
			       :harray harray :start (start-idx boundary)
			       :width width :destination node))
		     nodes)))))))))

;; #### NOTE: this function is not supposed to be called on empty harrays.
;; This should be checked by BREAK-HARRAY beforehand.
(defun make-graph
    (harray width &key (edge-type 'edge) (next-boundaries #'next-boundaries))
  "Make a solutions graph for breaking HARRAY into a paragraph of WIDTH.
Return two values: the graph's root node, and the nodes hash table. Note that
if no breaking solution is found, the root node will have no edges."
  (unless (consp next-boundaries)
    (setq next-boundaries (list next-boundaries)))
  (let* ((graph (make-hash-table))
	 (nodes (loop :for boundary
			:in (apply (car next-boundaries) harray 0 width
				   (cdr next-boundaries))
		      :when (make-subgraph harray boundary width graph
					   edge-type
					   (car next-boundaries)
					   (cdr next-boundaries))
			:collect :it)))
    (values
     (make-node nil
       (mapcar (lambda (node)
		 (make-instance edge-type
		   :harray harray :start 0 :width width :destination node))
	 nodes))
     graph)))




;; ==========================================================================
;; Layouts
;; ==========================================================================

;; #### FIXME: it is not powerful enough to use graph edges when creating
;; layouts. An edge can be used in multiple graph paths, so it cannot store
;; path-specific information. In a layout however, an edge is "frozen" to a
;; particular path.

;; In the KP algorithm for example, edges can only contain local demerits,
;; including line penalties but no adjacent, double, or final hyphen demerits.
;; Not even KP-POSTPOCESS-LAYOUT can handle those, as the edges are still
;; shared. As a result, the graph code creating kp lines can only advertise
;; local demerits, without the aforementioned contextual penalties.

(defclass layout ()
  ((edges :documentation "The list of edges from one break to the next."
	  :accessor edges))
  (:documentation "The LAYOUT class.
A layout represents one path from the root to the leaf node of a graph."))

(defmethod initialize-instance :before ((layout layout) &key edge)
  "Initialize LAYOUT's edges with the the first EDGE."
  (setf (slot-value layout 'edges) (list edge)))

(defgeneric push-edge (edge layout)
  (:documentation "Push EDGE on LAYOUT.")
  (:method (edge layout)
    "Perform the pushing."
    (push edge (edges layout))))

(defun %graph-layouts (graph layout-class layout-initargs)
  "Return GRAPH's layouts of LAYOUT-CLASS initialized with LAYOUT-INITARGS."
  (mapcan (lambda (edge)
	    (if (edges (destination edge))
	      (mapc (lambda (layout) (push-edge edge layout))
		(%graph-layouts
		 (destination edge) layout-class layout-initargs))
	      (list (apply #'make-instance layout-class :edge edge
			   layout-initargs))))
    (edges graph)))

;; #### NOTE: this function is not supposed to be called on null graphs
;; (coming from empty harrays), but might still return null layouts when the
;; graph got no solution.
(defun graph-layouts (graph &optional (layout-type 'layout)
			    &aux (layout-class (car-or-symbol layout-type))
				 (layout-initargs (cdr-or-nil layout-type)))
  "Return GRAPH's layouts of LAYOUT-TYPE.
LAYOUT-TYPE may be a layout class name (LAYOUT by default), or a list of the
form (CLASS-NAME INITARGS...)."
  (%graph-layouts graph layout-class layout-initargs))




;; ==========================================================================
;; Graph Breakups
;; ==========================================================================

;; #### NOTE: MAKE-GRAPH won't even be called on empty harrays. In such a
;; case, a graph breakup (which needs to exist anyway) will have its graph,
;; layouts, and renditions slots set to NIL. On the contrary, if the harray is
;; not empty but no solution is found, the graph will just be an empty root
;; node, and the layouts and renditions slots will contain arrays of size 0.
;; In fact, the distinction between empty harrays and empty graphs is
;; preventive only, because currently, no algorithm refuses to typeset (they
;; all fall back to an emergency solution). But we never know...

(defclass graph-breakup (breakup)
  ((graph :documentation "The breakup's original graph."
	  :initform nil :initarg :graph :reader graph)
   (nodes :documentation "The breakup's nodes hash table."
	  :initform nil :initarg :nodes :reader nodes)
   (layouts :documentation "The breakup's sorted layouts array."
	    :initform nil :reader layouts)
   (renditions :documentation "The breakup's sorted layout renditions array."
	       :initform nil :reader renditions))
  (:documentation "The Graph Breakup class.
This class is used by graph based algorithms."))

(defmethod initialize-instance :after ((breakup graph-breakup) &key layouts)
  "Convert the layouts list to an array and create the renditions array."
  (when (graph breakup)
    (setf (slot-value breakup 'layouts)
	  (make-array (length layouts) :initial-contents layouts))
    (setf (slot-value breakup 'renditions)
	  (make-array (length layouts) :initial-element nil))))

(defmethod pinned-lines
    ((breakup graph-breakup) &aux (renditions (renditions breakup)))
  (when (and renditions (not (zerop (length renditions))))
    (aref renditions 0)))

;; #### TODO: provide information on the graph.
(defmethod properties strnlcat
    ((breakup graph-breakup) &aux (layouts (layouts breakup)))
  "Advertise graph BREAKUP's number of initial layouts."
  (when layouts
    (multiple-value-bind (non-null null) (hash-table-counts (nodes breakup))
      (strnlcat (format nil "From ~A layout~:P.~@
			     ~A break points~:P, ~A dead-end~:P."
		  (length layouts) non-null null)
		(unless (zerop (length layouts))
		  (properties (aref layouts 0)))))))
