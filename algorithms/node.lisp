(in-package :etap)


(defclass paragraph-edge ()
  ((node :initarg :node :reader node)))

(defclass paragraph-node ()
  ((boundary :initarg :boundary :reader boundary)
   (edges :initarg :edges :reader edges)))

(defun make-paragraph-node (boundary edges)
  (make-instance 'paragraph-node :boundary boundary :edges edges))

(defgeneric next-boundaries
    (lineup start width child-type &key &allow-other-keys))

(defun create-paragraph-node
    (lineup width algorithm-type edge-type boundary hash &rest options)
  (or (gethash (stop boundary) hash)
      (setf (gethash (stop boundary) hash)
	    (if (= (stop boundary) (length lineup))
	      (make-paragraph-node boundary nil)
	      (let ((next-boundaries (apply #'next-boundaries
				       lineup (next-start boundary) width
				       algorithm-type options)))
		(make-paragraph-node
		 boundary
		 (mapcar
		     (lambda (next-boundary)
		       (apply #'make-instance edge-type
			      :lineup lineup :width width
			      :start (next-start boundary)
			      :node (apply #'create-paragraph-node
				      lineup width algorithm-type edge-type
				      next-boundary hash
				      options)
			      options))
		   next-boundaries)))))))

(defun paragraph-graph
    (lineup width algorithm-type
     &rest options
     &aux (edge-type (intern (format nil "~A-EDGE" algorithm-type) :etap))
	  (next-boundaries (apply #'next-boundaries lineup 0 width
				  algorithm-type options))
	  (hash (make-hash-table)))
  (make-paragraph-node
   nil
   (mapcar
       (lambda (next-boundary)
	 (apply #'make-instance edge-type
		:lineup lineup :width width :start 0
		:node (apply #'create-paragraph-node
			lineup width algorithm-type edge-type
			next-boundary hash
			options)
		options))
     next-boundaries)))


(defclass paragraph-layout () ((nodes :initarg :nodes :accessor nodes)))

(defmethod initialize-instance :around ((layout paragraph-layout) &key node)
  (call-next-method layout :nodes (list node)))

(defgeneric update-paragraph-layout (layout edge))

(defun %paragraph-layouts (node layout-type)
  (if (edges node)
    (mapcan (lambda (edge)
	      (mapc (lambda (layout)
		      (push node (nodes layout))
		      (update-paragraph-layout layout edge))
		(%paragraph-layouts (node edge) layout-type)))
      (edges node))
    (list (make-instance layout-type :node node))))

(defun paragraph-layouts
    (node algorithm
     &aux (layout-type
	   (intern (format nil "~A-LAYOUT" (algorithm-type algorithm)) :etap)))
  (%paragraph-layouts node layout-type))



;; #### NOTE: with the defaults (default text, 284pt, all features), there are
;; #### 66576 paragraph solutions including going through under and overfull
;; #### lines (21096 without hyphenation). The raw tree of all such solutions
;; #### has 150860 nodes (48338 without hyphenation). However, once a line
;; #### stop has been decided, all possible solutions for the next lines
;; #### remain the same, however we reached that possible stop. This means
;; #### that there is a lot of room for re-using branches. And indeed, when
;; #### sharing nodes, we fall from 150860 to 98 (from 48338 to 83 without
;; #### hyphenation).

;; #### If we avoid preventive fulls, that is, if we include only under- and
;; #### overfull solutions when there is not fit, the number of paragraph
;; #### solutions falls to 37 (it actually raises up to 61 without
;; #### hyphenation, all mistfits). The raw tree of all such solutions has
;; #### only 109 nodes (192 without hyphenations). Once shared, the actual
;; #### number of nodes falls down to 30 (33 without hyphenation).
#+()(defun report-solutions
    (context
     &key (width (paragraph-width context)) preventive-fulls)
  (let* ((lineup (create-lineup context))
	 (solutions
	   (mapcar (lambda (lines) (create-solution lineup width lines))
	     (root-node-lines (root-node lineup width preventive-fulls))))
	 (length 0)
	 (fits 0)
	 (fits-hyphened (make-hash-table))
	 (misfits 0))
    (loop :for solution :in solutions
	  :do (incf length)
	  :if (and (zerop (solution-hyphens solution))
		   (zerop (solution-underfulls solution))
		   (zerop (solution-overfulls solution)))
	    :do (incf fits)
	  :else :if (and (zerop (solution-underfulls solution))
			 (zerop (solution-overfulls solution)))
	    :do (if (gethash (solution-hyphens solution) fits-hyphened)
		  (setf (gethash (solution-hyphens solution) fits-hyphened)
			(1+ (gethash (solution-hyphens solution)
				     fits-hyphened)))
		  (setf (gethash (solution-hyphens solution) fits-hyphened) 1))
	  :else
	    :do (incf misfits))
    (format t "~A solutions in total.~%
~A fit solutions without hyphens.~%"
      length fits)
    (maphash (lambda (key value)
	       (format t "~A fit solutions with ~A hyphen~:P.~%"
		 value key))
	     fits-hyphened)
    (format t "~A mistfit solutions.~%" misfits)))
