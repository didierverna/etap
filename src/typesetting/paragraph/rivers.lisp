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
  ((mouth :documentation "This river arm's mouth (the bed it leads to)."
	  :initarg :mouth :reader mouth)
   (orientation :documentation "This river arm's orientation."
		:reader orientation))
  (:documentation "The river ARM class.
River arms are relative a source bed."))

(defmethod initialize-instance :after
    ((arm arm) &key source &aux (mouth (mouth arm)))
  "Compute ARM's orientation relative to the downward vertical direction.
ARM goes from SOURCE to its mouth."
  (setf (slot-value arm 'orientation)
	(angle (- (+ (x (board mouth))  (x mouth))
		  (+ (x (board source)) (x source)))
	       (- (+ (y (board mouth))  (y mouth))
		  (+ (y (board source)) (y source))))))

(defun make-arm (source mouth)
  "Mkae a river arm from SOURCE to MOUTH."
  (make-instance 'arm :source source :mouth mouth))

(defun arms (source line &aux (source-x (+ (x (board source)) (x source))))
  "Return a list of at most three river arms from SOURCE bed to the next LINE.
Arms are only considered from SOURCE bed to three possible mouth beds in LINE:
the closest to SOURCE's left, one directly below it, and the closest to
SOURCE's right, all of these X-wise."
  (mapcar (lambda (mouth) (make-arm source mouth))
    (loop :with left :with below
	  :for bed :in (remove-if-not #'bedp (pinned-objects (line line)))
	  :for bed-x := (+ (x (board bed)) (x bed))
	  :if (< bed-x source-x) :do (setq left (list bed))
	  :else :if (= bed-x source-x) :do (setq below (list bed))
	  :else :do (return (append left below (list bed)))
	  :finally (return (append left below)))))

(defun detect-rivers (paragraph angle &aux (hash (make-hash-table)))
  "Detect rivers of at most ANGLE threshold in PARAGRAPH.
The return value is a hash table mapping source beds to a list of arms."
  (loop :for line1 :in (get-rendition 0 (breakup paragraph))
	:for line2 :in (cdr (get-rendition 0 (breakup paragraph)))
	:for sources := (remove-if-not #'bedp (pinned-objects (line line1)))
	:when sources
	  :do (mapc (lambda (source &aux (arms (arms source line2)))
		      (setq arms (remove-if (lambda (orientation)
					      (> orientation angle))
				     arms
				   :key #'orientation))
		      (setf (gethash source hash) arms))
		sources))
  hash)
