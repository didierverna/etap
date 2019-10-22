(in-package :etap)

(defgeneric create-lines (lineup width disposition algorithm
			  &rest options
			  &key &allow-other-keys))
