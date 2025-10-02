(in-package :etap)

(defstruct demo-1 xinc yinc xshift yshift)

(defun demo-1-step (pane)
  (cond ((eq (capi-object-property pane :stop) t)
	 (setf (capi-object-property pane :demo-1) nil)
	 (setf (capi-object-property pane :line-x-shift) nil)
	 (setf (capi-object-property pane :line-y-shift) nil)
	 (redisplay-element pane)
	 (setf (capi-object-property pane :stop) nil)
	 :stop)
	(t
	 (when (demo-1-xinc (capi-object-property pane :demo-1))
	   (setf (demo-1-xshift (capi-object-property pane :demo-1))
		 (mod (+ (demo-1-xinc (capi-object-property pane :demo-1))
			 (demo-1-xshift (capi-object-property pane :demo-1)))
		      200)))
	 (when (demo-1-yinc (capi-object-property pane :demo-1))
	   (setf (demo-1-yshift (capi-object-property pane :demo-1))
		 (mod (+ (demo-1-yinc (capi-object-property pane :demo-1))
			 (demo-1-yshift (capi-object-property pane :demo-1)))
		      200)))
	 (redisplay-element pane))))

(defun demo-1-initialize (pane demo-1)
  (setf (capi-object-property pane :demo-1) demo-1)
  (when (demo-1-xinc demo-1)
    (setf (capi-object-property pane :line-x-shift)
	(lambda (line)
	  (if line
	    (+ 5 (* 5 (sin (+ (/ (* (demo-1-xshift (capi-object-property pane :demo-1)) pi)
				 100)
			      (y line)))))))))
  (when (demo-1-yinc demo-1)
    (setf (capi-object-property pane :line-y-shift)
	(lambda (line)
	  (if line
	    (+ 5 (* 5 (sin (+ (/ (* (demo-1-yshift (capi-object-property pane :demo-1)) pi)
				 100)
			      (y line)))))))))
  (mp:schedule-timer-relative-milliseconds
   (mp:make-timer 'demo-1-step pane) 10 10))


(defun demo-1 (etap &rest keys &key xinc yinc)
  (declare (ignore xinc yinc))
  (execute-with-interface-if-alive etap 'demo-1-initialize
    (view etap) (apply #'make-demo-1 :xshift 0 :yshift 0 keys)))

;; #### WARNING: STOP is executed asynchronously, so it's not a good idea to
;; mess around with any property that the demos may use. Hence a specific STOP
;; property below.
(defun stop (etap)
  (execute-with-interface-if-alive etap
    (lambda (pane) (setf (capi-object-property pane :stop) t))
    (view etap)))
