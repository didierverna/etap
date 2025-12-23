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

;; The set of all possible solutions found by an algorithm is represented by a
;; graph rather than a tree, so that storage is saved by sharing nodes. Such a
;; graph is oriented and acyclic. It also has only one root (the beginning of
;; the paragraph), and only one leaf (the end of the paragraph).

;; A graph is represented by a hash table storing edges rather than nodes. The
;; hash table keys are the harray break points (nodes), and each value is a
;; list of edges from that break point (boundaries). The graph construction
;; process is parameterized by an algorithm-specific function collecting the
;; possible boundaries for a line starting at a particular break point. This
;; allows each algorithm to use it's own boundary subclass.

;; Right now, we only have rectangular paragraphs so graph nodes are just
;; break points in the harray (this expresses the fact that however we reach a
;; particular break point has no influence on the subsequent breaking
;; solutions). Later we will need to make them a combination of break point
;; and line number. Graph edges don't need a specific data structure either,
;; as they can be conveniently reified as boundaries. Because of node sharing
;; however, a graph cannot store information specific to a particular path
;; from the beginning to the end of the paragraph. For that, another data
;; structure called a "layout", and containing a list of "lines", also
;; layout-specific, is used.

;; If there's no entry in the hash table for a particular break point, it
;; means that it was never considered as a possibility. If a break point was
;; considered, but eventually found out to be a dead end, the hash table entry
;; will be NIL (meaning that this break point has no outward edges). The only
;; exception to this is the end of paragraph break point, which has a hash
;; table value of T to indicate that it has no outward edges, but it is
;; normal.


(in-package :etap)
(in-readtable :etap)


;; ==========================================================================
;; Graph Construction
;; ==========================================================================

(defun %make-graph (harray bol width get-boundaries hash-table)
  "Make a solutions graph for breaking HARRAY at BOL into a paragraph of WIDTH.
If BOL is at the end of HARRAY, return T. Otherwise, return a possibly empty
list of boundaries representing edges leading to the end of HARRAY.

(GET-BOUNDARIES HARRAY BOL WIDTH) is called to get the possible end of line
boundaries for a line starting at BOL.

This function updates the graph's HASH-TABLE."
  ;; #### NOTE: we distinguish between a value of NIL and a value not found in
  ;; order to avoid trying dead ends multiple times.
  (multiple-value-bind (value found) (gethash bol hash-table)
    (if found
      value
      (setf (gethash bol hash-table)
	    (if (eopp bol)
	      t
	      (loop :for boundary :in (funcall get-boundaries harray bol width)
		    :when (%make-graph harray (break-point boundary) width
				       get-boundaries hash-table)
		      :collect boundary))))))

;; #### NOTE: this function is not supposed to be called on empty harrays.
;; This should be checked by BREAK-HARRAY beforehand.
(defun make-graph (harray width get-boundaries
		   &aux (hash-table (make-hash-table :test #'eq)))
  "Make a solutions graph for breaking HARRAY into a paragraph of WIDTH.
Return the graph's hash table.

The hash table keys (graph nodes) are break points in HARRAY. Thus, the root
node is found under the *BOP* key. The hash table values (list of graph edges)
are lists of boundaries. Thus, if no breaking solution is found, the *BOP*
hash table entry is NIL.

(GET-BOUNDARIES HARRAY *BOP* WIDTH) is called to get the possible end of line
boundaries for a line starting at *BOP*."
  ;; #### NOTE: we require GET-BOUNDARIES rather than GET-BOUNDARY (as in
  ;; greedy algorithm support) because some algorithms (e.g. our current
  ;; implementation of Duncan) may need to process all possibilities first,
  ;; before subsequently returning a filtered list of some. In fact, the Fit
  ;; algorithm has such a function as well, but it's an internal one.
  (setf (gethash *bop* hash-table)
	(loop :for boundary :in (funcall get-boundaries harray *bop* width)
	      :when (%make-graph harray (break-point boundary) width
				 get-boundaries hash-table)
		:collect boundary))
  hash-table)




;; ==========================================================================
;; Paths Construction
;; ==========================================================================

(defun make-graph-paths (graph &optional (boundaries (gethash *bop* graph)))
  "Return the list of all GRAPH paths.
A graph path is a list of boundaries from the beginning to the end of the
paragraph. Note that there is no boundary for the beginning of the paragraph,
but there is one for the end."
  (mapcan (lambda (boundary &aux (break-point (break-point boundary)))
	    (if (eopp break-point)
	      (list (list boundary))
	      (mapcar (lambda (path) (push boundary path))
		(make-graph-paths graph (gethash break-point graph)))))
    boundaries))

(defun make-graph-layouts (graph breakup make-layout)
  "Make GRAPH layouts for BREAKUP.
(MAKE-LAYOUT BREAKUP PATH) is called to create a layout from a GRAPH path."
  (mapcar (lambda (path) (funcall make-layout breakup path))
    (make-graph-paths graph)))




;; ==========================================================================
;; Graph Breakups
;; ==========================================================================

(defclass graph-breakup (breakup)
  ((graph
    :documentation "This breakup's graph hash table."
    :initform nil :reader graph))
  (:documentation "The Graph Breakup class.
This class is used by graph based algorithms."))

(defmethod properties strnlcat
    ((breakup graph-breakup) &key &aux (graph (graph breakup)))
  "Return a string advertising BREAKUP's graph properties."
  (when graph
    (multiple-value-bind (non-null null) (hash-table-counts graph)
      (format nil "Break points: ~A (~A dead-end~:P)." non-null null))))
