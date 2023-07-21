(in-package :etap)

(defun magnitude (dx dy)
  "Return the magnitude of vector (dx, dy)."
  (sqrt (+ (* dx dx) (* dy dy))))

(defun scalar-product (dx1 dy1 dx2 dy2)
  "Return the scalar product of vectors (dx1 dy1) and (dx2 dy2)."
  (+ (* dx1 dx2) (* dy1 dy2)))

(defun orientation (dx dy &aux (magnitude (magnitude dx dy)))
  "Return the orientation of vector (dx, dy) in degrees.
The orientation is relative to the downward vertical direction,
that is, vector (0, 1)."
  (setq dx (/ dx magnitude) dy (/ dy magnitude))
  (* 180 (/ (acos (scalar-product 0 1 dx dy)) pi)))

(defun sources (mouth beds &aux (mouth-x (+ (x (board mouth)) (x mouth))))
  "Return the river sources going from the previous line's BEDS to MOUTH.
There might be at most three beds constituting a river source to MOUTH: the
closest to MOUTH's left, one directly above it, and the closest to MOUTH's
right, all of these X-wise."
  (loop :with left :with above :with right
	:for bed :in beds
	:for bed-x := (+ (x (board bed)) (x bed))
	:if (< bed-x mouth-x) :do (setq left (list bed))
	  :else :if (= bed-x mouth-x) :do (setq above (list bed))
		  :else :do (return (append left above (list bed)))
	:finally (return (append left above))))

(defun detect-rivers
    (paragraph mouth-angle bed-angle &aux (hash (make-hash-table)))
  "Detect rivers in PARAGRAPH.
- MOUTH-ANGLE is the river's initial direction angle threshold,
  relative to the downward vertical direction, in absolute value.
- BED-ANGLE is the river's continuing direction angle threshold, relative to
  the previous one, in absolute value.

The return value is a hash table in which each key is a river bed, and each
value is a list of other rivers beds connected to the key by a river arm."
  ;; A river arm is a list of the form ((orientation...) . mouth).
  (loop :for line1 :in (pinned-lines paragraph)
	:for line2 :in (cdr (pinned-lines paragraph))
	:for beds1 := (remove-if-not #'bedp (pinned-objects (line line1)))
	:for beds2 := (remove-if-not #'bedp (pinned-objects (line line2)))
	:when beds2
	  :do (mapc (lambda (mouth)
		      (let ((arms (mapcar
				      (lambda (source)
					(cons source
					      (orientation
					       (- (+ (x (board mouth))
						     (x mouth))
						  (+ (x (board source))
						     (x source)))
					       (- (+ (y (board mouth))
						     (y mouth))
						  (+ (y (board source))
						     (y source))))))
				    (sources mouth beds1))))
			;; Silly visualization pre-test.
			(setq arms (remove-if
				       (lambda (arm)
					 (> (abs (cdr arm)) mouth-angle))
				       arms))
			(setf (gethash mouth hash) (mapcar #'car arms))))
		beds2))
  hash)
