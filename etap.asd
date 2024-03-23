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
      :components
      ((:file "glyphlist.txt")
       (:file "lm-ec.enc")
       (:file "lm-ec")
       (:file "font")))
     (:module "language"
      :serial t
      :components
      ((:file "common")
       (:module "hyphenation"
	:serial t
	:components
		((:file "rules")
		 (:file "load")
		 (:file "hyphenate")))
       (:file "language")
       (:file "text")))
     (:module "typesetting"
      :serial t
      :components
      ((:file "arith")
       (:file "common")
       (:module "paragraph"
	:serial t
	:components
		((:file "hlist")
		 (:file "common")
		 (:module "algorithms"
		  :components
			  ((:file "common")
			   (:file "lineup" :depends-on ("common"))
			   (:file "breakup" :depends-on ("lineup"))
			   (:file "fixed" :depends-on ("breakup"))
			   (:file "fit" :depends-on  ("fixed"))
			   (:file "barnett" :depends-on  ("fixed"))
			   (:file "graph" :depends-on ("breakup"))
			   (:file "duncan" :depends-on  ("graph"))
			   (:file "kp" :depends-on  ("graph"))))
		 (:file "paragraph")
		 (:file "rivers")))))
     (:file "context")
     (:module "interface"
      :components
      ((:file "capi" :if-feature :lispworks)))
     (:module "experiments"
      :serial t
      :components
      ((:file "scalar")
       (:file "graph")))
     (:file "post")))))
