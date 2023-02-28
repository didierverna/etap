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


(defclass paragraph-edge ()
  ((node :documentation "The node this edge points to."
	 :initarg :node :reader node))
  (:documentation "The PARAGRAPH-EDGE class."))


(defclass paragraph-node ()
  ((boundary :documentation "This node's boundary."
	     :initarg :boundary :reader boundary)
   (edges :documentation "The edges from this node to other nodes."
	  :initarg :edges :reader edges))
  (:documentation "The PARAGRAPH-NODE class."))

(defun make-paragraph-node (boundary edges)
  "Make a paragraph node out of BOUNDARY and EDGES."
  (make-instance 'paragraph-node :boundary boundary :edges edges))


(defgeneric next-boundaries
    (lineup start width child-type &key &allow-other-keys)
  (:documentation
   "Find the possible next boundaries from START position in LINEUP."))


;; #### WARNING: we use a hash table for storing and sharing nodes to express
;; the fact that however we reach a break, all subsequent solutions for the
;; rest of the paragraph will be the same. Note however that this is valid
;; only because we currently only have rectangular paragraphs. With bells and
;; whistles like \parshape, the subsequent solutions would differ, depending
;; on the line number at which a break occurs. Therefore, the sharing would
;; need to take the line number as well as the boundary into account.
(defun create-paragraph-node
    (lineup width algorithm-type edge-type boundary hash &rest options)
  "Create a node representing ALGORITHM-TYPE's view a line breaking solution
for a LINEUP paragraph of WIDTH starting at BOUNDARY. If no line breaking
solution is found, this function returns NIL."
  (or (gethash (stop-idx boundary) hash)
      (setf (gethash (stop-idx boundary) hash)
	    (if (null (stop-elt boundary))
	      (make-paragraph-node boundary nil)
	      (let ((nodes (loop :for next-boundary
				   :in (apply #'next-boundaries
					 lineup (next-start boundary) width
					 algorithm-type options)
				 :when (apply #'create-paragraph-node
					 lineup width algorithm-type edge-type
					 next-boundary hash
					 options)
				   :collect :it)))
		(when nodes
		  (make-paragraph-node
		   boundary
		   (mapcar (lambda (node)
			     (apply #'make-instance edge-type
				    :lineup lineup :width width
				    :start (next-start boundary)
				    :node node
				    options))
		     nodes))))))))

(defun paragraph-graph
    (lineup width algorithm-type
     &rest options
     &aux (edge-type (intern (format nil "~A-EDGE" algorithm-type) :etap))
	  (hash (make-hash-table))
	  (nodes (loop :for next-boundary
			 :in (apply #'next-boundaries lineup 0 width
				    algorithm-type options)
		       :when (apply #'create-paragraph-node
			       lineup width algorithm-type edge-type
			       next-boundary hash
			       options)
			 :collect :it)))
  "Create a graph representing a LINEUP paragraph of WIDTH with ALGORITHM-TYPE.
The graph contains the algorithm's view of all line break solutions, that is,
all possible routes from one boundary to another.
If no line breaking solution is found, this function returns NIL.
Otherwise, it returns the first node, that is, the node representing the start
of the paragraph. Thus, this node contains a null boundary and the edges
pointing to the first possible breaks."
  (when nodes
    (make-paragraph-node
     nil
     (mapcar (lambda (node)
	       (apply #'make-instance edge-type
		      :lineup lineup :width width :start 0 :node node
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
	    (if (edges (node edge))
	      (mapc (lambda (layout)
		      (push edge (edges layout))
		      (update-paragraph-layout layout edge))
		(%paragraph-layouts (node edge) layout-type))
	      (list (make-instance layout-type :edge edge))))
    (edges node)))

(defun paragraph-layouts
    (node algorithm
     &aux (layout-type
	   (intern (format nil "~A-LAYOUT" (algorithm-type algorithm)) :etap)))
  "Return ALGORITHM's view of layouts for paragraph starting at NODE."
  (%paragraph-layouts node layout-type))
