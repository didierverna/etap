(in-package :teap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (net.didierverna.tfm:nickname-package))

(defclass state ()
  ((kerning :initform nil :accessor kerning)
   (ligatures :initform nil :accessor ligatures)
   (hyphenation :initarg nil :accessor hyphenation)
   (disposition :initform :flush-left :accessor disposition)
   (font :initarg :font :reader font)
   (text :accessor text)
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
