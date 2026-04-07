(in-package :etap)
(in-readtable :etap)

(defstruct lwave amplitude ondulation propagation phase)

(defstruct rain densite speed hash)


;; ----------------------------------------
;;              Line Waves
;; ----------------------------------------

; Macro
(defmacro define-lwaves-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd lwaves caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber lwaves ,name ,min ,default ,max ,@keys))

(define-lwaves-caliber amplitude 0 0 10 :bounded t)
(define-lwaves-caliber ondulation 0 0 400 :bounded t)
(define-lwaves-caliber propagation 0 0 100 :bounded t)
(define-lwaves-caliber duration 1 3 100 :bounded t)



; Calcul
(defun lwaves-shift (y lwave)
  "Return an LWAVE shifting amount for Y position."
  (+ (lwave-amplitude lwave) ; preserve the paragraph's left border
    (* (lwave-amplitude lwave)
  (sin (+ (lwave-phase lwave)
    (/ (* 2 pi (lwave-ondulation lwave) y) 20000))))))

(defun lwaves-step (lwave-x lwave-y)
  (incf (lwave-phase lwave-x) (/ (lwave-propagation lwave-x) 100))
  (incf (lwave-phase lwave-y) (/ (lwave-propagation lwave-y) 100)))



;Instalation
(defmethod living-text-install-animation ((animation (eql :lines-waves)) view)
  (let ((lwave-x (capi-object-property view :lwave-x))
        (lwave-y (capi-object-property view :lwave-y)))
    (unless lwave-x
      (setq lwave-x (make-lwave
                    :phase 0
                    :amplitude   (caliber-default *lwaves-amplitude*)
                    :ondulation  (caliber-default *lwaves-ondulation*)
                    :propagation (caliber-default *lwaves-propagation*)))
      (setf (capi-object-property view :lwave-x) lwave-x))
    (unless lwave-y
      (setq lwave-y (make-lwave
                    :phase 0
                    :amplitude   (caliber-default *lwaves-amplitude*)
                    :ondulation  (caliber-default *lwaves-ondulation*)
                    :propagation (caliber-default *lwaves-propagation*)))
      (setf (capi-object-property view :lwave-y) lwave-y))
    (setf (capi-object-property view :line-x-shift)
          (lambda (line) (lwaves-shift (x line) lwave-x)))
    (setf (capi-object-property view :line-y-shift)
          (lambda (line) (lwaves-shift (y line) lwave-y)))
    (setf (capi-object-property view :living-text-step)
          (lambda () (lwaves-step lwave-x lwave-y)))))

;; ----------------------------------------
;;              Char Waves
;; ----------------------------------------


; Macro
(defmacro define-cwaves-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd cwaves caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber cwaves ,name ,min ,default ,max ,@keys))

(define-cwaves-caliber amplitude   0 0  10  :bounded t)
(define-cwaves-caliber ondulation  0 0  400 :bounded t)
(define-cwaves-caliber propagation 0 0  100 :bounded t)
(define-cwaves-caliber duration 1 3  100 :bounded t)



; Calcul
(defun cwaves-shift (x cwave)
  "Return an LWAVE shifting amount for X position."
  (+ (lwave-amplitude cwave)
    (* (lwave-amplitude cwave)
        (sin (+ (lwave-phase cwave)
                (/ (* 2 pi (lwave-ondulation cwave) x) 20000))))))

(defun cwaves-step (cwave-x cwave-y)
  (incf (lwave-phase cwave-x) (/ (lwave-propagation cwave-x) 100))
  (incf (lwave-phase cwave-y) (/ (lwave-propagation cwave-y) 100)))



; Installation
(defgeneric living-text-install-animation (animation view)
  (:documentation "Install ANIMATION in VIEW."))

(defmethod living-text-install-animation ((animation (eql :char-waves)) view)
  (let ((cwave-x (capi-object-property view :cwave-x))
        (cwave-y (capi-object-property view :cwave-y)))
    (unless cwave-x
      (setq cwave-x (make-lwave
                    :phase 0
                    :amplitude   (caliber-default *cwaves-amplitude*)
                    :ondulation  (caliber-default *cwaves-ondulation*)
                    :propagation (caliber-default *cwaves-propagation*)))
      (setf (capi-object-property view :cwave-x) cwave-x))
    (unless cwave-y
      (setq cwave-y (make-lwave
                    :phase 0
                    :amplitude   (caliber-default *cwaves-amplitude*)
                    :ondulation  (caliber-default *cwaves-ondulation*)
                    :propagation (caliber-default *cwaves-propagation*)))
      (setf (capi-object-property view :cwave-y) cwave-y))
    (setf (capi-object-property view :elt-x-shift)
          (lambda (elt) (cwaves-shift (x elt) cwave-x)))
    (setf (capi-object-property view :elt-y-shift)
          (lambda (elt) (cwaves-shift (x elt) cwave-y)))
    (setf (capi-object-property view :living-text-step)
          (lambda () (cwaves-step cwave-x cwave-y)))))


;; ----------------------------------------
;;               Rain
;; ----------------------------------------


; Macro
(defmacro define-rain-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd cwaves caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber rain ,name ,min ,default ,max ,@keys))

(define-rain-caliber densite   0 0  10  :bounded t)
(define-rain-caliber speed  0 2  20 :bounded t)
(define-rain-caliber duration 1 3  100 :bounded t)



; Calcul
; -------- A faire ----------

(defun rain-shift (elt hash)
"recupere la position y d'une lettre dans la hashmap et la renvoi "
  (gethash elt hash)
)

(defun rain-step (rain view)
  "avance chaque character a la vitesse du speed. Quand il touche le bas il se reste"
  (let* ((etap (top-level-interface view))
          (layout-# (layout etap))
          (layout (unless (zerop layout-#)
                    (get-layout (1- layout-#) (breakup etap)))))
    (when layout
      (let ((end (+ (height layout) (depth layout))))
        (maphash (lambda (key val)
                    (setf (gethash key (rain-hash rain))
                          (if (>= val end) 0 (+ val (rain-speed rain)))))
                  (rain-hash rain))))))



(defun populate (rain-hash layout densite)
  "Select des character de facon random et populate ma hashmap"
  (clrhash rain-hash)
  (loop :for line :in (lines layout)
        :for i :from 0
        :while (< i 3)
        :do (map nil
                  (lambda (item)
                    (when (typep (object item) 'tfm:character-metrics)
                      (when (zerop (random (max 1 densite)))
                        (setf (gethash item rain-hash) 0))))
                  (items line))))



; Installation
(defmethod living-text-install-animation ((animation (eql :rain)) view)
  (let ((rain (capi-object-property view :rain)))
    (unless rain
      (setq rain (make-rain
                    :densite  (caliber-default *rain-densite*)
                    :speed    (caliber-default *rain-speed*)
                    :hash     (make-hash-table)))
      (setf (capi-object-property view :rain) rain))
    (let* ((etap   (top-level-interface view))
            (layout-# (layout etap))
            (layout (unless (zerop layout-#)
                      (get-layout (1- layout-#) (breakup etap)))))
      (when layout
        (populate (rain-hash rain) layout (rain-densite rain))))
    (setf (capi-object-property view :elt-y-shift)
          (lambda (elt) (or (rain-shift elt (rain-hash rain)) 0)))
    (setf (capi-object-property view :living-text-step)
          (lambda () (rain-step rain view)))))