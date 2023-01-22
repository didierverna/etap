(in-package :etap)


;; ================
;; Lineup Utilities
;; ================

;; ------------
;; Lineup spans
;; ------------

(defstruct (span :conc-name
		 (:constructor make-span (normal-width min-width max-width)))
  "The SPAN structure.
A span contains normal, min, and max width information, and represents
length properties of a lineup (chunk)."
  normal-width min-width max-width)

(defmethod width ((span span))
  "Return SPAN's normal width."
  (normal-width span))

(defun lineup-span (lineup start stop)
  "Return the span of LINEUP between START and STOP."
  (multiple-value-bind (width stretch shrink) (lineup-width lineup start stop)
    (make-span width (- width shrink) (+ width stretch))))


;; -----------------
;; Lineup boundaries
;; -----------------

(defstruct (boundary
	    :conc-name
	    (:constructor make-boundary (stop next-start)))
  "The BOUNDARY structure.
A boundary contains a STOP index at which a lineup can be broken, and a
NEXT-START at which the next line may begin."
  stop next-start)

(defun word-boundary-p (lineup boundary)
  "Return T if BOUNDARY is at an end of word in LINEUP."
  (word-stop-p lineup (stop boundary)))

(defun next-boundary (lineup &optional (start 0) &aux (length (length lineup)))
  "Return the next boundary in LINEUP from START, or NIL."
  (unless (= start length)
    (let ((point (position-if #'break-point-p lineup :start (1+ start))))
      ;; #### WARNING: this is a kludge to never break at the end of the final
      ;; word (that is, just before the final glue). Otherwise, we would end
      ;; up with a line containing only the final glue. TeX does it by adding
      ;; \penalty10000 before the final glue (and it also adds \penalty-10000
      ;; afterwards), but we don't have that level of generality yet.
      (when (eql point (1- length)) (setq point nil))
      (if point
	(let ((next (1+ point)))
	  (typecase (aref lineup point)
	    (glue (make-boundary point next))
	    (discretionary (make-boundary next point))))
	(make-boundary length length)))))



;; =========================
;; Parametrization Utilities
;; =========================

(defstruct (caliber (:constructor make-caliber (min default max)))
  "The CALIBER structure.
A caliber represents values that have a mininum, a maximum, and a default."
  min default max)

(defmacro define-caliber (prefix name min default max)
  "Define a *PREFIX-NAME* caliber with MIN, DEFAULT, and MAX values."
  `(defparameter ,(intern (format nil "*~A-~A*" prefix name))
     (make-caliber ,min ,default, max)))

(defmacro calibrate
    (prefix name
     &optional infinity
     &aux (variable (intern (format nil "~A" name)))
	  (caliber (intern (format nil "*~A-~A*" prefix name))))
  "Calibrate NAMEd variable according to the *PREFIX-NAME* caliber.
If the variable's value is out of bounds, either clamp it (the default),
or use INFINITY values."
  `(cond ((null ,variable)
	  (setq ,variable (caliber-default ,caliber)))
	 ((<= ,variable (caliber-min ,caliber))
	  (setq ,variable ,(if infinity :-infinity `(caliber-min ,caliber))))
	 ((>= ,variable (caliber-max ,caliber))
	  (setq ,variable ,(if infinity :+infinity `(caliber-max ,caliber))))))


(defmacro default
    (prefix name
     &aux (variable (intern (format nil "~A" name)))
	  (choices (intern (format nil "*~A-~AS*" prefix name))))
  "If NAMEd variable is null, set it to the first *PREFIX-NAMES* choice.
Note the S appended to NAME in the choices variable name."
  `(when (null ,variable) (setq ,variable (car ,choices))))


(defun car-or-symbol (object)
  (etypecase object
    (cons (car object))
    (symbol object)))

(defun cdr-or-nil (object)
  (etypecase object
    (cons (cdr object))
    (symbol nil)))


(defun algorithm-type (algorithm) (car-or-symbol algorithm))
(defun algorithm-options (algorithm) (cdr-or-nil algorithm))
(defun disposition-type (disposition) (car-or-symbol disposition))
(defun disposition-options (disposition) (cdr-or-nil disposition))

(defparameter *maximum-badness* 10000)

(defun scale-badness (scale)
  (if (or (null scale) (< scale -1))
    :+infinity
    (min (* 100 (expt (abs scale) 3)) *maximum-badness*)))

(defun badness (lineup start stop width &optional emergency-stretch)
  (scale-badness (lineup-scale lineup start stop width emergency-stretch)))

(defun !< (x y)
  (cond ((eql x y) nil)
	((and (numberp x) (numberp y)) (< x y))
	((or (eq x :-infinity) (eq y :+infinity)) t)
	(t nil)))

(defun !<= (x y) (or (eql x y) (!< x y)))

(defun !+ (x y)
  (cond ((and (numberp x) (numberp y)) (+ x y))
	((numberp x) y)
	((numberp y) x)
	((eq x y) x)
	;; #### WARNING: we consider that -infinity + +infinity gives
	;; +infinity. The rationale is that this situation would occur when
	;; calculating badness + hyphen-penalty on an hyphen-overfull, with
	;; hyphen penalties of -infinity. But we always consider overfulls as
	;; infinitely bad. Note that this means that mandatory but not
	;; overfull hyphen breaks need to be handled first.
	(t :+infinity)))

(defun !expt (base power)
  (cond ((eq base :+infinity) :+infinity)
	((numberp base) (expt base power))
	((eq base :-infinity)
	 (cond ((zerop power) 1)
	       ((evenp power) :+infinity)
	       (t :-infinity)))))


(defgeneric create-lines
    (lineup width disposition algorithm &key &allow-other-keys))
