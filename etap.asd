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
       (:file "lm-ec")))
     (:file "hyphenation")
     (:module "typesetting"
      :serial t
      :components
      ((:file "lineup")
       (:file "common")
       (:module "paragraph"
	:serial t
	:components
		((:file "common")
		 (:module "algorithms"
		  :components
			  ((:file "common")
			   (:file "fixed" :depends-on ("common"))
			   (:file "fit" :depends-on  ("common"))
			   (:file "barnett" :depends-on  ("common"))
			   (:file "node" :depends-on ("common"))
			   (:file "duncan" :depends-on  ("node"))
			   (:file "kp" :depends-on  ("node"))))
		 (:file "paragraph")))))
     (:file "context")
     (:module "interface"
      :components
      ((:file "capi" :if-feature :lispworks)))
     (:file "post")))))
