(in-package :etap)

(defvar line-x-shift (lambda (line) 0))
(defvar line-y-shift (lambda (line) 0))
(defvar char-x-shift (lambda (item) 0))
(defvar char-y-shift (lambda (item) 0))


(defun demo-1-step (pane)
  (cond ((eq (capi-object-property pane :stop) t)
	 (setf (capi-object-property pane :stop) nil)
	 :stop)
	(t
	 (setf (capi-object-property pane :shift)
	       (mod (1+ (capi-object-property pane :shift)) 200))
	 (setq line-x-shift
	       (lambda (line)
		 (if line
		   (+ 5
		      (* 5 (sin (+ (/ (* (capi-object-property pane :shift) pi)
				      100)
				   (y line))))))))
	 (redisplay-element pane))))

(defun demo-1-initialize (pane)
  (setf (capi-object-property pane :shift) 0)
  (mp:schedule-timer-relative-milliseconds
   (mp:make-timer 'demo-1-step pane)
   10 10))


(defun demo-1 (etap)
  (execute-with-interface-if-alive etap 'demo-1-initialize (view etap)))

(defun stop (etap)
  (execute-with-interface-if-alive etap
    (lambda (pane) (setf (capi-object-property pane :stop) t))
    (view etap)))
