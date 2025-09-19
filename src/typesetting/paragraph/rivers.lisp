(in-package :etap)

;; #### FIXME: there's currently one slot in the interface for storing the
;; currently detected rivers. This was a quick hack to test the idea but it is
;; obviously wrong, as rivers are a property of a specific rendition. The
;; storage should thus be implemented in breakups.


;; =========
;; Utilities
;; =========

(defun vector-magnitude (dx dy)
  "Return the magnitude of vector (dx, dy)."
  (sqrt (+ (* dx dx) (* dy dy))))

(defun scalar-product (dx1 dy1 dx2 dy2)
  "Return the scalar product of vectors (dx1 dy1) and (dx2 dy2)."
  (+ (* dx1 dx2) (* dy1 dy2)))

(defun vector-orientation (dx dy &aux (magnitude (vector-magnitude dx dy)))
  "Return the orientation of vector (dx, dy) in degrees.
The orientation is relative to the downward vertical direction,
that is, vector (0, 1)."
  (setq dx (/ dx magnitude) dy (/ dy magnitude))
  (* 180 (/ (acos (scalar-product 0 1 dx dy)) pi)))


;; ==========
;; River Arms
;; ==========

(defclass arm ()
  ((mouth :documentation "This river arm's mouth (the whitespace it leads to)."
	  :initarg :mouth :reader mouth)
   (orientation :documentation "This river arm's orientation."
		:reader orientation))
  (:documentation "The river ARM class.
River arms are relative a source whitespace."))

(defmethod initialize-instance :after
    ((arm arm) &key source &aux (mouth (mouth arm)))
  "Compute ARM's orientation relative to the downward vertical direction.
ARM goes from SOURCE to its mouth."
  (setf (slot-value arm 'orientation)
	(vector-orientation (- (+ (x (board mouth))  (x mouth))
			       (+ (x (board source)) (x source)))
			    (- (+ (y (board mouth))  (y mouth))
			       (+ (y (board source)) (y source))))))

(defun make-arm (source mouth)
  "Mkae a river arm from SOURCE to MOUTH."
  (make-instance 'arm :source source :mouth mouth))

(defun arms
    (source line
     &aux (source-x (+ (x (board source)) (x source) (/ (width source) 2))))
  "Return a list of at most three river arms from SOURCE whitespace to the
next LINE. Arms are only considered from SOURCE whitespace to three possible
mouth whitespaces in LINE: the closest to SOURCE's left, one directly below
it, and the closest to SOURCE's right, all of these X-wise."
  (mapcar (lambda (mouth) (make-arm source mouth))
    (loop :with left :with below
	  :for ws :in (remove-if-not #'whitespacep (items line))
	  :for ws-x := (+ (x (board ws)) (x ws) (/ (width ws) 2))
	  :if (< ws-x source-x) :do (setq left (list ws))
	  :else :if (= ws-x source-x) :do (setq below (list ws))
	  :else :do (return (append left below (list ws)))
	  :finally (return (append left below)))))

(defun detect-rivers
    (layout &key (angle 0) &aux (lines (lines layout)) (hash (make-hash-table)))
  "Detect rivers of at most ANGLE threshold in LINES.
The return value is a hash table mapping source whitespaces to a list of arms."
  (loop :for line1 :in lines
	:for line2 :in (cdr lines)
	:for sources := (remove-if-not #'whitespacep (items line1))
	:when sources
	  :do (mapc (lambda (source &aux (arms (arms source line2)))
		      (setq arms (remove-if (lambda (orientation)
					      (> orientation angle))
				     arms
				   :key #'orientation))
		      (setf (gethash source hash) arms))
		sources))
  hash)
