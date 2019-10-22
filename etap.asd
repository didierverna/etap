(asdf:defsystem :etap
  :description "The Typesetting Experimental Algorithms Platform"
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage
  "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#etap"
  :source-control "https://github.com/didierverna/etap"
  :license "BSD"
  ;; :version 1.0
  :depends-on (:net.didierverna.tfm)
  :serial t
  :components ((:file "meta")
	       (:file "glyphlist")
	       (:file "lm-ec")
	       (:file "state")
	       (:file "lineup")
	       (:file "typesetting")
	       (:module "algorithms"
		:components ((:file "base")
			     (:file "fixed" :depends-on ("base"))
			     (:file "fit" :depends-on  ("base"))))
	       (:file "paragraph")
	       (:file "interface")))
