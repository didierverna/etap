(in-package :etap)

;; =========
;; Utilities
;; =========

(defun magnitude (dx dy)
  "Return the magnitude of vector (dx, dy)."
  (sqrt (+ (* dx dx) (* dy dy))))

(defun scalar-product (dx1 dy1 dx2 dy2)
  "Return the scalar product of vectors (dx1 dy1) and (dx2 dy2)."
  (+ (* dx1 dx2) (* dy1 dy2)))

(defun angle (dx dy &aux (magnitude (magnitude dx dy)))
  "Return the orientation of vector (dx, dy) in degrees.
The orientation is relative to the downward vertical direction,
that is, vector (0, 1)."
  (setq dx (/ dx magnitude) dy (/ dy magnitude))
  (* 180 (/ (acos (scalar-product 0 1 dx dy)) pi)))


;; ==========
;; River Arms
;; ==========

(defclass arm ()
  ((source :documentation "This river arm's source bed."
	   :initarg :source :reader source)
   (orientation :documentation "This river arm's orientation."
		:reader orientation))
  (:documentation "The river ARM class. River arms are relative a mouth bed."))

(defmethod initialize-instance :after ((arm arm) &key mouth)
  "Compute ARM's orientation relative to the downward vertical direction.
ARM goes from MOUTH to its source."
  (setf (slot-value arm 'orientation)
	(angle (- (+ (x (board mouth)) (x mouth))
		  (+ (x (board (source arm))) (x (source arm))))
	       (- (+ (y (board mouth)) (y mouth))
		  (+ (y (board (source arm))) (y (source arm)))))))

(defun make-arm (mouth source)
  "Mkae a river arm from MOUTH to SOURCE."
  (make-instance 'arm :mouth mouth :source source))

(defun arms (mouth line &aux (mouth-x (+ (x (board mouth)) (x mouth))))
  "Return a list of at most three river arms from MOUTH to the previous LINE.
Arms are only considered from MOUTH to three possible source beds in LINE:
the closest to MOUTH's left, one directly above it, and the closest to MOUTH's
right, all of these X-wise."
  (mapcar (lambda (bed) (make-arm mouth bed))
    (loop :with left :with above
	  :for bed :in (remove-if-not #'bedp (pinned-objects (line line)))
	  :for bed-x := (+ (x (board bed)) (x bed))
	  :if (< bed-x mouth-x) :do (setq left (list bed))
	    :else :if (= bed-x mouth-x) :do (setq above (list bed))
		    :else :do (return (append left above (list bed)))
	  :finally (return (append left above)))))

(defun detect-rivers
    (paragraph mouth-angle bed-angle &aux (hash (make-hash-table)))
  "Detect rivers in PARAGRAPH.
- MOUTH-ANGLE is the river's initial direction angle threshold,
  relative to the downward vertical direction, in absolute value.
- BED-ANGLE is the river's continuing direction angle threshold, relative to
  the previous one, in absolute value.

The return value is a hash table mapping mouths (beds) to a list of arms."
  ;; A river arm is a list of the form ((orientation...) . mouth).
  (loop :for line1 :in (pinned-lines paragraph)
	:for line2 :in (cdr (pinned-lines paragraph))
	:for mouths := (remove-if-not #'bedp (pinned-objects (line line2)))
	:when mouths
	  :do (mapc
		  (lambda (mouth &aux (arms (arms mouth line1)))
		    (mapc
			(lambda
			    (arm &aux (confluents (gethash (source arm) hash)))
			  (when (or (and confluents
					 (some (lambda (confluent)
						 (<= (abs (- (orientation arm)
							     (orientation
							      confluent)))
						     bed-angle))
					       confluents))
				    (and (not confluents)
					 (<= (abs (orientation arm))
					     mouth-angle)))
			    (setf (gethash mouth hash)
				  (append (gethash mouth hash) (list arm)))))
		      arms))
		mouths))
  hash)
