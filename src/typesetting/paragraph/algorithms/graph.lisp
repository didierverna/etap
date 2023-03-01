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


(defclass edge ()
  ((destination :documentation "The node this edge points to."
		:initarg :destination :reader destination))
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

(defun make-node (boundary edges)
  "Make a node at BOUNDARY, followed by EDGES."
  (make-instance 'node :boundary boundary :edges edges))



;; #### FIXME: the &allow-other-keys below is dirty. That's because we don't
;; currently make a distinction in the OPTIONS argument to MAKE-GRAPH, between
;; what goes to NEXT-BOUNDARIES and to MAKE-INSTANCE EDGE-TYPE. This needs to
;; be cleaned up.

;; #### TODO: for experimentation, we could make PREVENTIVE-*FULLS a number
;; instead of just a Boolean, for keeping more than 1 *full.
(defun next-boundaries
    (lineup start width &key fulls &allow-other-keys)
  "Find the possible endings for LINEUP line of WIDTH beginning at START.
If no possible ending is found, return NIL, unless FULLS, in which case return
the last underfull and the first overfull (if any) as a fallback solution.
If FULLS is :PREVENTIVE, also return these fulls even if possible endings were
found.
The possible endings are listed in reverse order (from last to first)."
  (loop :with underfull
	:with fits := (list)
	:with overfull
	;; #### NOTE: if we reach the end of the lineup, we get #S(LENGTH NIL)
	;; first, and then NIL.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (stop-idx boundary))
	:while (and boundary (not overfull))
	:for span := (lineup-span lineup start (stop-idx boundary))
	:if (< (max-width span) width)
	  :do (setq underfull boundary)
	:else :if (and (<= (min-width span) width) (>= (max-width span) width))
	  :do (push boundary fits)
	:else
	  :do (setq overfull boundary)
	:finally
	   (return
	     (cond ((eq fulls :preventive)
		    (append
		     (when overfull (list overfull))
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
    (lineup width boundary edge-type next-boundaries hash &rest options)
  "Make a solutions sub-graph for breaking LINEUP's remainder starting at
BOUNDARY into a paragraph of WIDTH.
If no line breaking solution is found, this function returns NIL.
Otherwise, it returns the sub-graph's root node.
This function memoizes previously computed sub-graphs into HASH table."
  (or (gethash (stop-idx boundary) hash)
      (setf (gethash (stop-idx boundary) hash)
	    (if (null (item boundary))
	      (make-node boundary nil)
	      (let ((nodes (loop :for next-boundary
				   :in (apply next-boundaries
					 lineup (start-idx boundary) width
					 options)
				 :when (apply #'make-subgraph
					 lineup width next-boundary
					 edge-type next-boundaries hash
					 options)
				   :collect :it)))
		(when nodes
		  (make-node
		   boundary
		   (mapcar (lambda (node)
			     (apply #'make-instance edge-type
				    :lineup lineup :width width
				    :start (start-idx boundary)
				    :destination node
				    options))
		     nodes))))))))

(defun make-graph
    (lineup width
     &rest keys &key (edge-type 'edge) (next-boundaries #'next-boundaries)
     &allow-other-keys
     &aux (hash (make-hash-table))
	  (options (remove-keys keys :edge-type :next-boundaries))
	  (nodes (loop :for next-boundary
			 :in (apply next-boundaries lineup 0 width options)
		       :when (apply #'make-subgraph
			       lineup width next-boundary
			       edge-type next-boundaries hash
			       options)
			 :collect :it)))
  "Make a solutions graph for breaking LINEUP into a paragraph of WIDTH.
If no line breaking solution is found, this function returns NIL.
Otherwise, it returns the graph's root node."
  (when nodes
    (make-node
     nil
     (mapcar (lambda (node)
	       (apply #'make-instance edge-type
		      :lineup lineup :width width :start 0 :destination node
		      options))
       nodes))))


(defclass paragraph-layout ()
  ((edges :documentation "The list of edges from one break to the next."
	  :initarg :edges :accessor edges))
  (:documentation "The PARAGRAPH-LAYOUT class.
A paragraph layout represents one possible way to break the lineup."))

(defmethod initialize-instance :around ((layout paragraph-layout) &key edge)
  "Initialize LAYOUT's edges with the the first EDGE."
  (call-next-method layout :edges (list edge)))

(defgeneric update-paragraph-layout (layout edge)
  (:documentation "Update LAYOUT after the addition of EDGE."))

(defun %paragraph-layouts (node layout-type)
  "Return the list of possible layouts starting at NODE."
  (mapcan (lambda (edge)
	    (if (edges (destination edge))
	      (mapc (lambda (layout)
		      (push edge (edges layout))
		      (update-paragraph-layout layout edge))
		(%paragraph-layouts (destination edge) layout-type))
	      (list (make-instance layout-type :edge edge))))
    (edges node)))

(defun paragraph-layouts
    (node algorithm
     &aux (layout-type
	   (intern (format nil "~A-LAYOUT" (algorithm-type algorithm)) :etap)))
  "Return ALGORITHM's view of layouts for paragraph starting at NODE."
  (%paragraph-layouts node layout-type))
