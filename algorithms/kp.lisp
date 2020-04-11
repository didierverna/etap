;; This is TeX's algorithm, initially described in Knuth, Donald E.; Plass,
;; Michael F. (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)

(define-constant +kp-variants+
    '(:graph :dynamic))

(define-constant +kp-variants-help-keys+
    '(:kp-variant-graph :kp-variant-dynamic))


(define-constant +kp-line-penalty+ '(0 10 100))
(define-constant +kp-hyphen-penalty+ '(-1000 50 1000))
(define-constant +kp-explicit-hyphen-penalty+ '(-1000 50 1000))
(define-constant +kp-adjacent-demerits+ '(0 10000 10000))
(define-constant +kp-double-hyphen-demerits+ '(0 10000 10000))
(define-constant +kp-final-hyphen-demerits+ '(0 5000 10000))
(define-constant +kp-pre-tolerance+ '(0 100 1000))
(define-constant +kp-tolerance+ '(0 200 1000))
(define-constant +kp-emergency-stretch+ '(0 5 10))
(define-constant +kp-looseness+ '(-10 0 10))

(define-constant +kp-tooltips+
    '(:kp-variant-graph "Graph-based implementation."
      :kp-variant-dynamic "Dynamic programming implementation."))


(defclass kp-edge (paragraph-edge)
  ((demerits :initform 0 :accessor demerits)))

(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start (hyphen-penalty +kp-default-hyphen-penalty+)
     &aux (stop (stop (boundary (node edge))))
	  (badness (badness lineup start stop width))
	  (penalty (if (word-stop-p lineup stop)
		     0
		     (unless (= hyphen-penalty +kp-max-hyphen-penalty+)
		       hyphen-penalty))))
  ;; #### WARNING: this is not the complete demerits function because 1. we
  ;; assume only positive hyphen penalties for now, and 2. it only takes the
  ;; current line into account. Additional weights like double hyphen
  ;; penalties will need to be handled later.
  (setf (demerits edge) (!expt (!+ 1 (!+ badness penalty)) 2)))


;; #### NOTE: in this version, we collect only the fit solutions if any,
;; otherwise the last underfull if any, and as a last resort the first
;; overfull. In order to properly handle potentially infinite hyphen
;; penalties, we would need to collect all boundaries from the last
;; word-underfull one, up to the first overfull, as in the Best/Justified Fit
;; version. However, it is unrealistic to work on a paragraph graph in such a
;; case. For example, with all features enabled (hyphenation most
;; importantly), the default paragraph leads to a graph which has more than
;; 12032 solutions. And even then, we haven't even begun to handle negative
;; hyphen penalties (in which case we would need to go back to the last
;; hyphen-underfull), let alone -\infty ones, or variable penalties.
(defmethod next-boundaries
    (lineup start width (algorithm (eql :kp)) &key)
  (loop :with underfull
	:with fits := (list)
	:with overfull
	;; #### NOTE: this works even the first time because at worst,
	;; BOUNDARY is gonna be #S(LENGTH LENGTH LENGTH) first, and NIL only
	;; afterwards.
	:for boundary := (next-boundary lineup start)
	  :then (next-boundary lineup (next-start boundary))
	:while (and boundary (not overfull))
	:for span := (lineup-span lineup start (stop boundary))
	:if (< (max-width span) width)
	  :do (setq underfull boundary)
	:else :if (and (<= (min-width span) width)
		       (>= (max-width span) width))
	  :do (push boundary fits)
	:else
	  :do (setq overfull boundary)
	:finally
	   (return (cond (fits fits)
			 ;; #### NOTE: contrary to the Duncan version, we
			 ;; don't even bother to return both an underfull and
			 ;; an overfull here, since we already know that the
			 ;; badness for overfull is +\infty.
			 (underfull (list underfull))
			 (t (list overfull))))))


(defclass kp-layout (paragraph-layout)
  ((demerits :initform 0 :accessor demerits)))

(defmethod update-paragraph-layout ((layout kp-layout) (edge kp-edge))
  (setf (demerits layout) (!+ (demerits layout) (demerits edge))))


(defun kp-create-lines (lineup layout width sloppy)
  (loop :for node :in (cdr (nodes layout))
	:and start := 0 :then (next-start (boundary node))
	:for stop := (stop (boundary node))
	:collect (create-justified-line lineup start stop width sloppy)))

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass))
     &rest options &key
     &aux (sloppy (cadr (member :sloppy (disposition-options disposition)))))
  #+()(let* ((graph (apply #'paragraph-graph lineup width :kp options))
	 (layouts (paragraph-layouts graph :kp))
	 (acceptable (remove-if (lambda (layout) (null (demerits layout)))
				layouts))
	 (fallbacks (remove-if-not (lambda (layout) (null (demerits layout)))
				   layouts)))
    ;; #### FIXME: options to do better than just returning the first ones.
    (cond (acceptable
	   (let ((minimum-demerits (loop :for layout :in acceptable
					 :minimize (demerits layout))))
	     (kp-create-lines lineup (find minimum-demerits acceptable
					   :key #'demerits)
			      width sloppy)))
	  (t
	   (kp-create-lines lineup (car fallbacks) width sloppy)))))
