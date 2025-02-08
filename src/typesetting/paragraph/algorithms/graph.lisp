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
;; graph rather than a tree, so that storage is saved by sharing nodes. Right
;; now, we only have rectangular paragraphs so graph nodes are just break
;; points in the harray. Later we will need to make them a combination of
;; break point and line number. Graph edges don't need a specific data
;; structure either, as they can be conveniently reified as boundaries.
;; Because of node sharing however, a graph cannot store information specific
;; to a particular path from the beginning to the end of the paragraph. For
;; that, another data structure called a "layout" is used.


(in-package :etap)


;; ==========================================================================
;; Graphs
;; ==========================================================================

;; A graphs is represented by a hash table storing edges rather than nodes.
;; The hash table keys are the harray break points (nodes), and each value is
;; a list of edges (boundaries).

;; ------------------
;; Graph construction
;; ------------------

;; #### NOTE: if there's no entry in the hash table for a particular break
;; point, it means that it was never considered. If a break point was
;; considered, but eventually found out to be a dead end, the hash table entry
;; will be NIL (meaning that this break point has no outward edges). The only
;; exception to this is the end of paragraph break point, which has a hash
;; table value of T to indicate that it has no outward edges, but it is
;; normal.

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
;; Layouts
;; ==========================================================================

;; ---------------------
;; Layout edges (ledges)
;; ---------------------

(defclass ledge ()
  ((boundary :documentation "The corresponding boundary (graph edge)."
	     :initarg :boundary :reader boundary))
  (:documentation "The LEDGE class.
A ledge represents a line ending at a certain break point in one particular
path from the beginning to the end of the paragraph (that is, a layout). As
boundaries, they do not store the position of the beginning of the line.
Contrary to boundaries however, they are specific to one path. Consequently,
algorithms may subclass this class in order to store cumulative properties
about the path so far."))

(defmethod hyphenated ((ledge ledge))
  "Return LEDGE's hyphenation status."
  (hyphenated (boundary ledge)))


;; -------
;; Layouts
;; -------

(defclass layout ()
  ((ledges :documentation "This layout's list of ledges." :reader ledges))
  (:documentation "The LAYOUT class.
A layout represents one specific path from the beginning to the end of the
paragraph in a graph of possible solutions. Algorithms may subclass this class
in order to store layout specific properties."))

(defmethod initialize-instance :after ((layout layout) &key boundary)
  "Initialize LAYOUT's ledges with one ledge leading to final BOUNDARY."
  (setf (slot-value layout 'ledges)
	(list (make-instance 'ledge :boundary boundary))))

;; #### NOTE: because the ledges are created and pushed bottom-up in layouts,
;; it is not possible to compute cumulative properties (which are top-down) in
;; the process. Because of that, algorithms need to post-process the created
;; layouts anyway, so we might as well just use the base classes here, and let
;; the algorithms change-class at will.
(defun make-layouts (graph &optional (boundaries (gethash *bop* graph)))
  "Return the list of all GRAPH layouts.
Layouts and ledges are instantiated from the corresponding base classes only.
Algorithms may subsequently post-process the created objects and change their
classes into more specific ones."
  (mapcan (lambda (boundary &aux (break-point (break-point boundary)))
	    (if (eopp break-point)
	      (list (make-instance 'layout :boundary boundary))
	      (mapc (lambda (layout)
		      (setf (slot-value layout 'ledges)
			    (cons (make-instance 'ledge :boundary boundary)
				  (ledges layout))))
		(make-layouts graph (gethash break-point graph)))))
    boundaries))




;; ==========================================================================
;; Ledge Lines
;; ==========================================================================

(defclass ledge-line (line)
  ((ledge :documentation "This line's corresponding ledge."
	  :initarg :ledge :reader ledge))
  (:documentation "The Ledge Line class."))

(defun make-ledge-line
    (harray bol ledge beds &rest keys &key scale effective-scale)
  "Make an HARRAY line from BOL for LEDGE, possibly including river BEDS."
  (declare (ignore scale effective-scale))
  (apply #'make-instance 'ledge-line
	 :harray harray
	 :start-idx (bol-idx bol)
	 :stop-idx (eol-idx (break-point (boundary ledge)))
	 :beds beds
	 :ledge ledge
	 keys))

(defmethod properties strnlcat ((line ledge-line))
  "Advertise LINE's ledge properties."
  (properties (ledge line)))


(defun make-layout-lines (harray beds layout make-line)
  "Make LAYOUT lines."
  (loop :for bol := *bop* :then (break-point (boundary ledge))
	:for ledge :in (ledges layout)
	:collect (funcall make-line harray bol ledge beds)))




;; ==========================================================================
;; Graph / Layout Breakups
;; ==========================================================================

;; #### NOTE: graph breakups remember their harrays in order to be able to
;; support lazy computation of renditions.

;; #### NOTE: MAKE-GRAPH won't even be called on empty harrays. In such a
;; case, a graph breakup (which needs to exist anyway) will have all its slots
;; slots (but the HARRAY one) set to NIL. On the contrary, if the harray is
;; not empty but no solution is found, the layouts and renditions slots will
;; contain arrays of size 0. In fact, the distinction between empty harrays
;; and empty graphs is preventive only, because currently, no algorithm
;; refuses to typeset (they all fall back to an emergency solution). But we
;; never know...

(defclass graph-breakup (breakup)
  ((harray
    :documentation "This breakup's harray."
    :initarg :harray :reader harray)
   (beds
    :documentation "Whether to include river beds in renditions."
    :initarg :beds :reader beds)
   (graph
    :documentation "This breakup's graph hash table."
    :initform nil :initarg :graph :reader graph)
   (layouts
    :documentation "This breakup's sorted layouts array."
    :initform nil :reader layouts)
   (renditions
    :documentation "This breakup's sorted layout renditions array."
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


;; #### TODO: provide information on the graph.
(defmethod properties strnlcat
    ((breakup graph-breakup) &aux (layouts (layouts breakup)))
  "Advertise graph BREAKUP's number of initial layouts."
  (when layouts
    (multiple-value-bind (non-null null) (hash-table-counts (graph breakup))
      (strnlcat (unless (zerop (length layouts))
		  ;; #### FIXME: this is wrong in multiple renditions support.
		  (properties (aref layouts 0)))
		(format nil "Break points: ~A (~A dead-end~:P).~@
			     Layouts: ~A."
		  non-null null (length layouts))))))


;; ==========================================================================
;; Renditions
;; ==========================================================================

(defgeneric make-rendition (nth breakup)
  (:documentation "Make the Nth BREAKUP rendition.")
  (:method :around (nth (breakup graph-breakup))
    "Save the rendition in graph BREAKUP's renditions array."
    (setf (aref (renditions breakup) nth) (call-next-method))))

(defmethod renditions-# ((breakup graph-breakup))
  "Return graph BREAKUP's renditions number."
  (length (renditions breakup)))

(defmethod get-rendition (nth (breakup graph-breakup))
  "Return the Nth graph BREAKUP's rendition."
  (or (aref (renditions breakup) nth) (make-rendition nth breakup)))
