(in-package :etap)


(defun arms (bed beds &aux (bed-x (+ (x (board bed)) (x bed))))
  "Return BED's river arms from next line's BEDS.
There might be at most three next beds constituting a river arm:
the closest to BED's left, one directly below it, and the closest to BED's
right X-wise."
  (loop :with left :with below :with right
	;; #### TODO: there is of course a more efficient way of computing all
	;; river arms from one line to the next, than traversing BEDS multiple
	;; times like this. For now, we don't care much.
	:for bed2 :in beds
	:for bed2-x := (+ (x (board bed2)) (x bed2))
	:if (< bed2-x bed-x) :do (setq left (list bed2))
	:else :if (= bed2-x bed-x) :do (setq below (list bed2))
	:else :do (return (append left below (list bed2)))
	:finally (return (append left below))))

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
	  :do (mapc (lambda (bed1)
		      (setf (gethash bed1 hash) (arms bed1 beds2)))
		beds1))
  hash)
