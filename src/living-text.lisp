(in-package :etap)

(defvar line-x-shift (lambda (line) 0))
(defvar line-y-shift (lambda (line) 0))
(defvar char-x-shift (lambda (item) 0))
(defvar char-y-shift (lambda (item) 0))


(defun demo-1-step (pane)
  (cond ((eq (capi-object-property pane :stop) t)
	 (setf (capi-object-property pane :line-x-shift) nil)
	 (redisplay-element pane)
	 (setf (capi-object-property pane :stop) nil)
	 :stop)
	(t
	 (setf (capi-object-property pane :shift)
	       (mod (1+ (capi-object-property pane :shift)) 200))
	 (redisplay-element pane))))

(defun demo-1-initialize (pane axis)
  (setf (capi-object-property pane :shift) 0)
  (setf (capi-object-property
	 pane (case axis (:x :line-x-shift) (:y :line-y-shift)))
	(lambda (line)
	  (if line
	    (+ 5 (* 5 (sin (+ (/ (* (capi-object-property pane :shift) pi)
				 100)
			      (y line))))))))
  (mp:schedule-timer-relative-milliseconds
   (mp:make-timer 'demo-1-step pane) 10 10))


(defun demo-1 (etap &optional (axis :x))
  (execute-with-interface-if-alive etap 'demo-1-initialize (view etap) axis))

;; #### WARNING: STOP is executed asynchronously, so it's not a good idea to
;; mess around with any property that the demos may use. Hence a specific STOP
;; property below.
(defun stop (etap)
  (execute-with-interface-if-alive etap
    (lambda (pane) (setf (capi-object-property pane :stop) t))
    (view etap)))
