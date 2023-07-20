(in-package :etap)



(defun detect-rivers
    (paragraph mouth-angle bed-angle &aux (hash (make-hash-table)))
  "Detect rivers in PARAGRAPH.
- MOUTH-ANGLE is the river's initial direction angle threshold,
  relative to the downward vertical direction, in absolute value.
- BED-ANGLE is the river's continuing direction angle threshold, relative to
  the previous one, in absolute value.

The return value is a hash table in which each key is a river bed, and each
value is a list of other rivers beds connected to the key by a river arm."
  (loop :for line1 :in (pinned-lines paragraph)
	:for line2 :in (cdr (pinned-lines paragraph))
	:for beds1 := (remove-if-not #'bedp (pinned-objects (line line1)))
	:for beds2 := (remove-if-not #'bedp (pinned-objects (line line2)))
	;; Silly visualization pre-test.
	:when beds1
	  :do (mapc (lambda (bed1) (setf (gethash bed1 hash) beds2)) beds1))
  hash)
