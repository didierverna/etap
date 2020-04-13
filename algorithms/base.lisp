(in-package :etap)


(define-constant +dispositions+
    '(:flush-left :centered :flush-right :justified))

(define-constant +disposition-options+ '((:sloppy t)))

(define-constant +disposition-options-help-keys+
    '(:disposition-option-sloppy))

(define-constant +disposition-options-tooltips+
    '(:disposition-option-sloppy
      "In Justified disposition, stretch or shrink as needed,
ignoring the font's inter-word spacing boundaries."))


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

(define-constant +maximum-badness+ 10000)

(defun badness (lineup start stop width
		&aux (scale (lineup-scale lineup start stop width)))
  (if (or (null scale) (< scale -1))
    :+infinity
    (min (* 100 (expt (abs scale) 3)) +maximum-badness+)))

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
