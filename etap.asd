(asdf:defsystem :etap
  :description "The Experimental Typesetting Algorithms Platform"
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage
  "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#etap"
  :source-control "https://github.com/didierverna/etap"
  :license "BSD"
  ;; :version 1.0
  :depends-on (:net.didierverna.tfm)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "meta")
     (:file "util")
     (:module "typesetting"
      :serial t
      :components
      ((:file "glyphlist")
       (:file "lm-ec")
       (:file "hyphenation")
       (:file "lineup")
       (:file "common")
       (:module "paragraph"
	:serial t
	:components
		((:file "common")
		 (:module "algorithms"
		  :components
			  ((:file "base")
			   (:file "fixed" :depends-on ("base"))
			   (:file "fit" :depends-on  ("base"))
			   (:file "barnett" :depends-on  ("base"))
			   (:file "node" :depends-on ("base"))
			   (:file "duncan" :depends-on  ("node"))
			   (:file "kp" :depends-on  ("node"))))
		 (:file "paragraph")))))
     (:file "context")
     (:module "interface"
      :components
      ((:file "capi" :if-feature :lispworks)))
     (:file "post")))))
