;; This is the Knuth-Plass algorithm from: Knuth, Donald E.; Plass, Michael F.
;; (1981), "Breaking paragraphs into lines", Software: Practice and
;; Experience, 11 (11): 1119–1184.

(in-package :etap)


(defmethod create-lines
    (lineup disposition width (algorithm (eql :knuth-plass)) &key)
)
