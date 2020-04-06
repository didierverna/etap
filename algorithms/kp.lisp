;; This is the Knuth-Plass algorithm from: Knuth, Donald E.; Plass, Michael F.
;; (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)


(define-constant +kp-default-hyphen-penalty+ 50)
(define-constant +kp-min-hyphen-penalty+ 0)
(define-constant +kp-max-hyphen-penalty+ 1000)


#+()(defclass kp-edge (paragraph-edge)
  ((badness :initform 0 :accessor badness)
   (penalty :initform 0 :accessor penalty)))

#+()(defmethod initialize-instance :after
    ((edge kp-edge)
     &key lineup width start
     &aux (stop (stop (boundary (node edge)))))
)

(defmethod create-lines
    (lineup width disposition (algorithm (eql :knuth-plass)) &key)
  )
