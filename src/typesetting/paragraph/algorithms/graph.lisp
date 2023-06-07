;; #### NOTE: with the defaults (default text, 284pt, all features), there are
;; 66576 paragraph solutions including going through under and overfull lines
;; (21096 without hyphenation). The raw tree of all such solutions has 150860
;; nodes (48338 without hyphenation). However, once a line stop has been
;; decided, all possible solutions for the next lines remain the same (modulo
;; the rectangular paragraph assumption), however we reached that possible
;; stop. This means that there is a lot of room for re-using branches. And
;; indeed, when sharing nodes, we fall from 150860 to 98 (from 48338 to 83
;; without hyphenation).

;; If we avoid preventive fulls, that is, if we include only under- and
;; overfull solutions when there is no fit, the number of paragraph solutions
;; falls to 37 (it actually raises up to 61 without hyphenation, all
;; mistfits). The raw tree of all such solutions has only 109 nodes (192
;; without hyphenations). Once shared, the actual number of nodes falls down
;; to 30 (33 without hyphenation).

(in-package :etap)


;; ======
;; Graphs
;; ======

;; ---------------
;; Data Structures
;; ---------------

(defclass edge ()
  ((destination :documentation "The node this edge points to."
		:initarg :destination :reader destination))
  ;; This is necessary to allow the code below to pass :lineup, :start, and
  ;; :width along, even though this base class doesn't actually use them.
  (:default-initargs :allow-other-keys t)
  (:documentation "The EDGE class.
Algorithms using a graph to represent paragraph breaking solutions specialize
this class to add specific properties to their edges."))


(defclass node ()
  ((boundary :documentation "This node's boundary."
	     :initarg :boundary :reader boundary)
   (edges :documentation "The edges from this node to other nodes."
	  :initarg :edges :reader edges))
  (:documentation "The NODE class.
A node represents a boundary at which a paragraph is broken, and links to the
next possible break positions."))

(defun make-node (boundary &optional edges)
  "Make a node at BOUNDARY, optionally followed by EDGES."
  (make-instance 'node :boundary boundary :edges edges))

(defun hyphenp (edge)
  "Return T if EDGE's destination boundary is hyphenated."
  (hyphenation-point-p (item (boundary (destination edge)))))


;; ------------------
;; Graph construction
;; ------------------

;; #### TODO: for experimentation, we could make PREVENTIVE-*FULLS a number
;; instead of just a Boolean, for keeping more than 1 *full.
(defun next-boundaries (lineup start width &key fulls strict)
  "Find the possible endings for LINEUP line of WIDTH beginning at START.
If no possible ending is found, return NIL, unless FULLS, in which case return
the last underfull and the first overfull (if any) as a fallback solution.
If FULLS is :PREVENTIVE, also return these fulls even if possible endings were
found. If STRICT, consider that even the last line must fit exactly. Otherwise
(the default), consider a final underfull as a fit.
The possible endings are listed in reverse order (from last to first)."
  (loop :with underfull :with fits := (list) :with overfull
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	:while (and boundary (not overfull))
	:for (nil max min)
	  := (multiple-value-list
	      (lineup-width lineup start (stop-idx boundary)))
	:do (cond ((i< max width)
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
    (lineup width boundary edge-type edge-options next-boundaries hash
     &rest options)
  "Make a solutions sub-graph for breaking LINEUP's remainder starting at
BOUNDARY into a paragraph of WIDTH.
If no line breaking solution is found, this function returns NIL.
Otherwise, it returns the sub-graph's root node.
This function memoizes previously computed sub-graphs into HASH table."
  (multiple-value-bind (value found) (gethash (stop-idx boundary) hash)
    (if found
      value
      (setf (gethash (stop-idx boundary) hash)
	    (if (last-boundary-p boundary)
	      (make-node boundary)
	      (let ((nodes (loop :for next-boundary
				   :in (apply next-boundaries
					 lineup (start-idx boundary) width
					 options)
				 :when (apply #'make-subgraph
					 lineup width next-boundary
					 edge-type edge-options
					 next-boundaries hash options)
				   :collect :it)))
		(when nodes
		  (make-node
		   boundary
		   (mapcar (lambda (node)
			     (apply #'make-instance edge-type
				    :lineup lineup :start (start-idx boundary)
				    :width width :destination node
				    edge-options))
		     nodes)))))))))

(defun make-graph
    (lineup width
     &rest keys &key (edge-type 'edge) (next-boundaries #'next-boundaries)
     &allow-other-keys
     &aux (hash (make-hash-table))
	  (edge-options (when (consp edge-type) (cdr edge-type)))
	  (edge-type (if (consp edge-type) (car edge-type) edge-type))
	  (options (remove-keys keys :edge-type :next-boundaries))
	  (nodes (loop :for next-boundary
			 :in (apply next-boundaries lineup 0 width options)
		       :when (apply #'make-subgraph
			       lineup width next-boundary
			       edge-type edge-options next-boundaries hash
			       options)
			 :collect :it)))
  "Make a solutions graph for breaking LINEUP into a paragraph of WIDTH.
If no line breaking solution is found, this function returns NIL.
Otherwise, it returns the graph's root node, and the nodes hash table as a
second value."
  (values
   (when nodes
     (make-node
      nil
      (mapcar (lambda (node)
		(apply #'make-instance edge-type
		       :lineup lineup :start 0 :width width :destination node
		       edge-options))
	nodes)))
   hash))



;; =======
;; Layouts
;; =======

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

(defun layouts (graph &optional (layout-type 'layout))
  "Return GRAPH's layouts of LAYOUT-TYPE."
  (when graph
    (mapcan (lambda (edge)
	      (if (edges (destination edge))
		(mapc (lambda (layout) (push-edge edge layout))
		  (layouts (destination edge) layout-type))
		(list (make-instance layout-type :edge edge))))
      (edges graph))))
