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
     (:module "util"
      :serial t
      :components
      ((:file "misc")
       (:file "arith")))
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
       (:file "text")
       (:file "nlstring")))
     (:module "typesetting"
      :serial t
      :components
      ((:file "common")
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
			   (:file "greedy" :depends-on ("breakup"))
			   (:file "fixed" :depends-on ("greedy"))
			   (:file "fit" :depends-on  ("fixed"))
			   (:file "barnett" :depends-on  ("fixed"))
			   (:file "graph" :depends-on ("breakup"))
			   (:file "duncan" :depends-on  ("graph"))
			   (:file "kp" :depends-on  ("graph"))
			   (:file "kpx" :depends-on  ("kp"))))
		 (:file "paragraph")
		 (:file "rivers")))))
     (:file "context")
     (:module "interface"
      :components
      ((:file "capi" :if-feature :lispworks)))
     (:file "post")))))
