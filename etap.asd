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
     (:module "font"
      :serial t
      :components ((:file "glyphlist.txt")
		   (:file "lm-ec.enc")
		   (:file "lm-ec")
		   (:file "font")))
     (:module "hyphenation"
      :serial t
      :components ((:file "rules")
		   (:file "load")
		   (:file "hyphenate")))
     (:file "context")
     (:module "typesetting"
      :serial t
      :components
      ((:file "arith")
       (:file "lineup")
       (:file "common")
       (:module "paragraph"
	:serial t
	:components
		((:file "common")
		 (:module "algorithms"
		  :components
			  ((:file "common")
			   (:file "fixed" :depends-on ("common"))
			   (:file "fit" :depends-on  ("fixed"))
			   (:file "barnett" :depends-on  ("fixed"))
			   (:file "graph" :depends-on ("fit"))
			   (:file "duncan" :depends-on  ("graph"))
			   (:file "kp" :depends-on  ("graph"))))
		 (:file "paragraph")
		 (:file "rivers")))))
     (:module "interface"
      :components
      ((:file "capi" :if-feature :lispworks)))
     (:module "experiments"
      :serial t
      :components
      ((:file "scalar")
       (:file "graph")))
     (:file "post")))))
