(in-package :etap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (net.didierverna.tfm:nickname-package))

(defconstant +initial-text+
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defconstant +font-file+
  #p"/usr/local/texlive/2019/texmf-dist/fonts/tfm/adobe/times/ptmr.tfm")

(defclass state ()
  ((font :initform (tfm:load-font +font-file+) :reader font)
   (disposition :initform :flush-left :accessor disposition)
   (features :initform (list) :accessor features)
   ;; 284.52756pt = 10cm
   (paragraph-width :initform 284 :accessor paragraph-width)
   (text :initform +initial-text+ :accessor text)
   (paragraph :accessor paragraph)))

(defstruct char-box x char)

(defun render
    (state &aux (text (string-trim '(#\Space #\Tab #\Newline) (text state)))
		(font (font state))
		(? (tfm:get-character (char-code #\?) font))
		(design-size (tfm:design-size font))
		(space (* (tfm:interword-space font) design-size))
		(line (make-array (length text) :fill-pointer 0)))
  (loop :with pos := 0
	:until (zerop (length text))
	:do (cond ((member (aref text 0) '(#\Space #\Tab #\Newline))
		   (setq text (string-left-trim '(#\Space #\Tab #\Newline)
						text))
		   (incf pos space))
		  (t
		   (let* ((code (char-code (aref text 0)))
			  (char (or (tfm:get-character code font) ?)))
		     (vector-push (make-char-box :x pos :char code) line)
		     (incf pos (* (tfm:width char) design-size))
		     (setq text (subseq text 1))))))
  (setf (paragraph state) line))
