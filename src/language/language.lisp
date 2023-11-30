(in-package :etap)

(defparameter *languages* '((:english . "en-us") (:français . "fr"))
  "An alist matching language names (keywords) to radicals.
The radicals are currently used to find hyphenation files in ETAP's
share/hyphenation/ subdirectory. For each RADICAL, there must be a
patterns file named RADICAL.pat.txt, and possibly an exceptions file
named RADICAL.hyp.txt.")

(defvar *language* :english
  "The language.")

