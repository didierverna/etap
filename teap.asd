(asdf:defsystem :teap
  :description "The Typesetting Experimental Algorithms Platform"
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage
  "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#teap"
  :source-control "https://github.com/didierverna/teap"
  :license "BSD"
  ;; :version 1.0
  :depends-on (:mcclim)
  :components ((:file "teap")))
