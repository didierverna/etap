(in-package :etap)
(in-readtable :etap)

(defstruct lwave amplitude ondulation propagation phase)

(defstruct rain densite max-speed hash wind)

(defstruct curtains speed offset direction)

(defstruct heart speed size wait hash phase counter)

(defstruct bomb phase hash tremble wait shockwave drop-speed obus counter)


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
(define-lwaves-caliber duration 0 3 100 :bounded t)


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
          (lambda (line) (lwaves-shift (y line) lwave-x)))
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
(define-cwaves-caliber duration 0 3  100 :bounded t)


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

(define-rain-caliber densite 0 2 10 :bounded t)
(define-rain-caliber max-speed  0 1  10 :bounded t)
(define-rain-caliber duration 0 3  100 :bounded t)
(define-rain-caliber wind -12 1 12 :bounded t) ;0-12 km/h


; Calcul
(defun rain-shift-y (elt hash)
"recupere la position y d'une lettre dans la hashmap et la renvoi "
  (let ((val (gethash elt hash)))
    (when val (nth 0 val))))
(defun rain-shift-x (elt hash)
"recupere la position x d'une lettre dans la hashmap et la renvoi "
  (let ((val (gethash elt hash)))
    (when val (nth 1 val))))

(defun rain-step (rain view)
  "avance chaque character a la vitesse du speed. Quand il touche le bas il se reste"
  (let* ((etap (top-level-interface view))
         (layout-# (layout etap))
         (layout (unless (zerop layout-#)
                   (get-layout (1- layout-#) (breakup etap))))) ; get the layout
    (when layout
      (let ((end-down (+ (height layout) (depth layout))) ;limit dow
            (end-side (paragraph-width (breakup etap)))) ;limite side
        (maphash (lambda (key val)
                   (let ((dy (nth 0 val));decalage y
                         (dx (nth 1 val));decalage x
                         (vy (nth 2 val)) ; vitesse vertical 
                         (vx (nth 3 val)) ; wind power
                         (Yorigin (nth 4 val)) ; origin position y
                         (Xorigin (nth 5 val))) ; origin position x
                        
                        (setf (gethash key (rain-hash rain))
                              (if (or (>= (+ dy Yorigin) end-down) (> (+ dx Xorigin) end-side) (< (+ dx Xorigin) 0))
                                (list 0 0 0 0 Yorigin Xorigin) ; put it back to origin
                                (list 
                                  (+ dy vy)
                                  (+ dx vx)
                                  (+ vy (* (- (rain-max-speed rain) vy) 0.1))
                                  (+ vx (* (- (rain-wind rain) vx) 0.1))
                                  Yorigin
                                  Xorigin)
                              ))))
                 (rain-hash rain))))))


(defun populate (rain-hash layout densite max-speed)
  (clrhash rain-hash)
  (let* ((par-y (height layout))
        (end (+ par-y (depth layout))))
    (loop :for line :in (lines layout)
          :for i :from 0
          :while (< i 3)
          :do (map nil
                    (lambda (item)
                      (when (typep (object item) 'tfm:character-metrics)
                        (when (< (random 10) densite)
                          (setf (gethash item rain-hash)
                                (list (- (random (floor end))) ; decalage y
                                      0 ; decalage x
                                      (+ 1 (random (max 1 (floor max-speed))))  ; vitesse vertical aléatoire
                                      0
                                      (+ par-y (y line)); Origin y
                                      (+ (x line) (x item))) ;Origin x
                          ))))  
                    (items line)))))


; Installation
(defmethod living-text-install-animation ((animation (eql :rain)) view)
  (let ((rain (capi-object-property view :rain)))
    (unless rain
      (setq rain (make-rain
                    :densite (caliber-default *rain-densite*)
                    :max-speed (caliber-default *rain-max-speed*)
                    :wind (caliber-default *rain-wind*)
                    :hash (make-hash-table)))
      (setf (capi-object-property view :rain) rain))
    (let* ((etap   (top-level-interface view))
            (layout-# (layout etap))
            (layout (unless (zerop layout-#)
                      (get-layout (1- layout-#) (breakup etap)))))
      (when layout
        (populate (rain-hash rain) layout (rain-densite rain) (rain-max-speed rain))))
    (setf (capi-object-property view :elt-y-shift)
          (lambda (elt) (or (rain-shift-y elt (rain-hash rain)) 0)))
    (setf (capi-object-property view :elt-x-shift)
          (lambda (elt) (or (rain-shift-x elt (rain-hash rain)) 0)))
    (setf (capi-object-property view :living-text-step)
          (lambda () (rain-step rain view)))))


;; ----------------------------------------
;;              Curtains
;; ----------------------------------------
; Macro
(defmacro define-curtains-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd curtains caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber curtains ,name ,min ,default ,max ,@keys))

(define-curtains-caliber speed   0 2  20  :bounded t)


;Calcul
(defun curtains-shift (elt par-width curtains)
  (let* ((center (/ par-width 2))
         (elt-x (x elt))
         (offset (curtains-offset curtains)))
    (if (< elt-x center)
        (- (min offset elt-x))
        (min offset (- par-width elt-x)))))



(defun curtains-step (curtains par-width)
  (ecase (curtains-direction curtains)
    (:open
     (if (>= (curtains-offset curtains) (/ par-width 2))
         :stop
         (progn
           (incf (curtains-offset curtains) (curtains-speed curtains))
           nil)))
    (:close
     (if (<= (curtains-offset curtains) 0)
         :stop
         (progn
           (decf (curtains-offset curtains) (curtains-speed curtains))
           (when (< (curtains-offset curtains) 0)
             (setf (curtains-offset curtains) 0))
           nil)))))


(defun curtains-reset (view)
  "Reset the curtains animation to its initial state."
  (let ((curtains (capi-object-property view :curtains)))
    (when curtains
      (setf (curtains-offset curtains) 0))))

  
; Installation
(defmethod living-text-install-animation ((animation (eql :curtains)) view)
  (let* ((curtains (capi-object-property view :curtains))
         (etap     (top-level-interface view))
         (layout-# (layout etap))
         (layout   (unless (zerop layout-#)
                     (get-layout (1- layout-#) (breakup etap)))))
    (unless curtains
      (setq curtains (make-curtains
                       :speed     (caliber-default *curtains-speed*)
                       :offset    0
                       :direction :open))
      (setf (capi-object-property view :curtains) curtains))
    (when layout
      (let ((par-width (paragraph-width (breakup etap))))
        (setf (capi-object-property view :elt-x-shift)
              (lambda (elt) (curtains-shift elt par-width curtains)))
        (setf (capi-object-property view :living-text-step)
              (lambda () (curtains-step curtains par-width)))))))




;; ----------------------------------------
;;               Heart
;; ----------------------------------------

(defmacro define-heart-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a NAMEd heart caliber with MIN, DEFAULT, and MAX values."
  (declare (ignore infinity bounded))
  `(define-caliber heart ,name ,min ,default ,max ,@keys))

(define-heart-caliber speed 1 3 20 :bounded t)
(define-heart-caliber size 1 5 20 :bounded t)
(define-heart-caliber wait 1 5 20 :bounded t)


; Calcul
(defun heart-point (t-param scale center-x center-y)
  "Retourne (x . y) sur la courbe cardiaque pour le parametre T-PARAM."
  (let* ((s  (sin t-param))
         (c  (cos t-param))
         (mx (* scale 16 s s s))
         (my (- (* 13 c)
                (* 5 (cos (* 2 t-param)))
                (* 2 (cos (* 3 t-param)))
                       (cos (* 4 t-param)))))
    (cons (+ center-x mx)
          (- center-y (* scale my)))))

(defun heart-populate (heart layout par-width)
  "Calcule la position cible sur le coeur pour chaque caractere.
Stocke (cur-dx cur-dy tgt-dx tgt-dy) dans le hash de HEART."
  (clrhash (heart-hash heart))
  (let* ((par-y (height layout))
         (par-h+d (+ par-y (depth layout)))
         (center-x (/ par-width 2))
         (center-y (/ par-h+d 2))
         (scale (heart-size heart))
         (pairs (let ((acc '()))
                     (dolist (line (lines layout) (nreverse acc))
                       (map nil
                            (lambda (item)
                              (when (typep (object item) 'tfm:character-metrics)
                                (push (cons item line) acc)))
                            (items line)))))
         (n (length pairs)))
    (loop :for (item . line) :in pairs
          :for i :from 0
          :for t-param := (/ (* 2 pi i) (max 1 n))
          :for target  := (heart-point t-param scale center-x center-y)
          :do (setf (gethash item (heart-hash heart))
                    (list 0.0 0.0
                          (- (car target) (+ (x line) (x item)))
                          (- (cdr target) (+ par-y (y line))))))))


(defun heart-reset-positions (heart)
  "Remet cur-dx et cur-dy a 0 pour chaque caractere."
  (maphash (lambda (key val)
             (setf (gethash key (heart-hash heart))
                   (list 0.0 0.0 (third val) (fourth val))))
           (heart-hash heart))
  (setf (heart-counter heart) -1))



(defun heart-step (heart)
  "Avance chaque caractere vers sa cible. Retourne :STOP quand tous sont arrives."
  (let ((speed (heart-speed heart))
        (donep t))
    (maphash
     (lambda (key val)
       (let* ((cur-dx (first  val))
              (cur-dy (second val))
              (tgt-dx (third  val))
              (tgt-dy (fourth val))
              (ddx    (- tgt-dx cur-dx))
              (ddy    (- tgt-dy cur-dy))
              (dist   (sqrt (+ (* ddx ddx) (* ddy ddy)))))
         (if (<= dist speed)
             (setf (gethash key (heart-hash heart))
                   (list tgt-dx tgt-dy tgt-dx tgt-dy))
             (progn
               (setq donep nil)
               (setf (gethash key (heart-hash heart))
                     (list (+ cur-dx (* speed (/ ddx dist)))
                           (+ cur-dy (* speed (/ ddy dist)))
                           tgt-dx tgt-dy))))))
     (heart-hash heart))
    (when donep
    (cond ((= (heart-counter heart) -1)
          ;; Premier passage : demarrer l'attente
          (setf (heart-counter heart) (* (heart-wait heart) 33))
          nil)
          ((> (heart-counter heart) 0)
          ;; Attente en cours
          (decf (heart-counter heart))
          nil)
          (t
          ;; Attente terminee : reset et stop
          (heart-reset-positions heart)
          :stop)))))



; Installation
(defmethod living-text-install-animation ((animation (eql :heart)) view)
  (let* ((etap      (top-level-interface view))
         (layout-#  (layout etap))
         (layout    (unless (zerop layout-#)
                      (get-layout (1- layout-#) (breakup etap))))
         (par-width (paragraph-width (breakup etap)))
         (heart (make-heart :speed   (caliber-default *heart-speed*)
                   :size    (caliber-default *heart-size*)
                   :wait    (caliber-default *heart-wait*)
                   :hash    (make-hash-table)
                   :phase   nil
                   :counter -1)))
    (setf (capi-object-property view :heart) heart)
    (when layout
      (heart-populate heart layout par-width))
    (setf (capi-object-property view :elt-x-shift)
          (lambda (elt)
            (let ((val (gethash elt (heart-hash heart))))
              (if val (first val) 0))))
    (setf (capi-object-property view :elt-y-shift)
          (lambda (elt)
            (let ((val (gethash elt (heart-hash heart))))
              (if val (second val) 0))))
    (setf (capi-object-property view :living-text-step)
          (lambda () (heart-step heart)))))



;; ----------------------------------------
;;               Bomb
;; ----------------------------------------


; Macro
(defmacro define-bomb-caliber
    (name min default max &rest keys &key infinity bounded)
  "Define a named bomb caliber with values."
  (declare (ignore infinity bounded))
  `(define-caliber bomb ,name ,min ,default ,max ,@keys))


(define-bomb-caliber tremble 0 2 9 :bounded t)
(define-bomb-caliber wait 1 2 10 :bounded t)
(define-bomb-caliber shockwave 0 10 30 :bounded t)
(define-bomb-caliber drop-speed 1 5 10 :bounded t)



; Calcul

;---------
; Utils
;---------
(defun bomb-shift-y (elt hash)
  "recupere la position y d'une lettre dans la hashmap et la renvoi "
  (let ((val (gethash elt hash)))
    (when val (nth 1 val)))
)

(defun bomb-shift-x (elt hash)
  "recupere la position x d'une lettre dans la hashmap et la renvoi "
  (let ((val (gethash elt hash)))
    (when val (nth 0 val)))
)

(defun bomb-reset-positions (bomb)
  "Reset position in the hashmap, garde l'origine"
  (maphash (lambda (key val)
             (setf (gethash key (bomb-hash bomb))
                   (list 0 0 (third val) (fourth val))))
           (bomb-hash bomb))
  (setf (bomb-counter bomb) -1)
)

(defun bomb-wait-time (bomb)
  "Decompte bomb-counter. Retourne :stop quand fini, initialise si besoin."
  (cond
    ((= (bomb-counter bomb) -1)
     (setf (bomb-counter bomb) (* (bomb-wait bomb) 33))
     nil)
    ((> (bomb-counter bomb) 0)
     (decf (bomb-counter bomb))
     nil)
    (t
     :stop))
)

(defun bomb-populate (bomb layout)
  "hashmap de bomb : (dx dy orig-x orig-y)"
  (clrhash (bomb-hash bomb))
  (let ((par-y (height layout)))
    (dolist (line (lines layout))
      (map nil
           (lambda (item)
             (when (typep (object item) 'tfm:character-metrics)
               (setf (gethash item (bomb-hash bomb))
                     (list 
                        0 ;decalage x
                        0 ;decalage y
                        (+ (x line) (x item));origin x
                        (+ par-y (y line));origin y
                      ))))
           (items line))))
)


;---------
; Tremble
;---------
(defun bomb-tremble-step (bomb)
  "Applique le tremblement a tous les caracteres, en preservant l'origine."
  (let ((tremble (bomb-tremble bomb)))
    (maphash
     (lambda (key val)
       (let ((orig-x (nth 2 val))
             (orig-y (nth 3 val)))
         (setf (gethash key (bomb-hash bomb))
               (list (+ (- tremble) (random (float tremble)))
                     (+ (- tremble) (random (float tremble)))
                     orig-x
                     orig-y))))
     (bomb-hash bomb)))
)

;---------
; Drop
;---------
(defun bomb-obus-step (bomb view)
  "Drop l'obus"
  (declare (ignore view))
  (unless (bomb-obus bomb) (return-from bomb-obus-step :stop))
  (let ((speed (/ (bomb-drop-speed bomb) 2))
        (donep t))
    (setf (bomb-obus bomb)
          (mapcar (lambda (entry)
                    (destructuring-bind (elt depart cible courant dx) entry
                      (let ((new-cur (+ courant speed)))
                        (when (< new-cur cible)
                          (setq donep nil))
                        (list elt depart cible (min new-cur cible) dx))))
                  (bomb-obus bomb)))
    (when donep :stop))
)

(defun bomb-obus-populate (bomb layout par-width)
  "l'obus tombe au centre du paragraphe.
   refaire une vraie forme d'obus plus tard."
  (let* ((par-y (height layout))
         (par-h+d (+ par-y (depth layout)))
         (target-y (/ par-h+d 2))
         (first-line (first (lines layout))))
    (when first-line
      (let ((all-chars (remove-if-not
                          (lambda (item)
                            (typep (object item) 'tfm:character-metrics))
                          (coerce (items first-line) 'list))))
        (setf (bomb-obus bomb)
              (mapcar (lambda (item)
                        (list item (- par-y 100) target-y 0.0 0))
                      all-chars)))))
)

;---------
; Shockwave
;---------
(defun bomb-shockwave-step (bomb layout par-width)
  " Create an shockwave starting from the center"
    (let* (
      (impact-x (+ (x (first (lines layout)))
        (/ (width (first (lines layout))) 2)))
      (par-y (height layout))
      (par-h+d (+ par-y (depth layout)))
      (impact-y (/ par-h+d 2));impact y
      (sigma 20.0) ; largeur de la crete
      (max-dist (sqrt (+ (* (/ par-width 2) (/ par-width 2))
                         (* (/ par-h+d 2) (/ par-h+d 2))))))
      
      (incf (bomb-counter bomb) (/ (bomb-shockwave bomb) 3.0))

      (let ((rayon (bomb-counter bomb)))
      (maphash
       (lambda (key val)
         (let* ((orig-x (nth 2 val))
                (orig-y (nth 3 val))
                (ddx    (- orig-x impact-x))
                (ddy    (- orig-y impact-y))
                (d      (sqrt (+ (* ddx ddx) (* ddy ddy))))
                (h      (/ (exp (- (/ (* (- d rayon) (- d rayon))
                                      (* 2 sigma sigma))))
                           (sqrt (max d 1.0))))
                (ux     (if (> d 0) (/ ddx d) 0))
                (uy     (if (> d 0) (/ ddy d) 0)))
           (setf (gethash key (bomb-hash bomb))
                 (list (* h ux 30) (* h uy 30) orig-x orig-y))))
       (bomb-hash bomb))

      (when (> rayon (+ max-dist (* 2 sigma)))
        (bomb-reset-positions bomb)
        :stop)))
)

;---------
; Disappear
;---------
(defun bomb-disappear-step (bomb layout par-width)
  "Fait disparaitre les caracteres du dehors vers le centre."
    (let* ((impact-x (/ (width (first (lines layout))) 2))
          (impact-y (/ (+ (height layout) (depth layout)) 2)))
      (decf (bomb-counter bomb) (bomb-shockwave bomb))
      (let ((seuil (bomb-counter bomb)))
        (maphash
        (lambda (key val)
          (let* ((orig-x (nth 2 val))
                  (orig-y (nth 3 val))
                  (ddx    (- orig-x impact-x))
                  (ddy    (- orig-y impact-y))
                  (d      (sqrt (+ (* ddx ddx) (* ddy ddy)))))
            (if (> d seuil)
              (setf (gethash key (bomb-hash bomb))
                    (list (* ddx 1000) (* ddy 1000) orig-x orig-y))
              (setf (gethash key (bomb-hash bomb))
                    (list (first val) (second val) orig-x orig-y)))))
        (bomb-hash bomb))
        (when (<= seuil 0)
          :stop))
    )
)

;---------
; MAIN BOMB
;---------
(defun bomb-step (bomb view)
  "Main step function for the bomb with different phase :
  - tremble 
  - drop
  - shockwave "
  (ecase (bomb-phase bomb)
    (:tremble
      (let ((result (bomb-wait-time bomb)))
        (if (eq result :stop)
          (progn
            (bomb-reset-positions bomb)
            (setf (bomb-phase bomb) :drop)
            (let* ((etap (top-level-interface view))
                    (layout-# (layout etap))
                    (layout (unless (zerop layout-#)
                              (get-layout (1- layout-#) (breakup etap))))
                    (par-width (paragraph-width (breakup etap))))
              (when layout
                (bomb-obus-populate bomb layout par-width))))
          (bomb-tremble-step bomb))
        nil))
    (:drop
     (let ((result (bomb-obus-step bomb view)))
       (when (eq result :stop)
         (setf (bomb-phase bomb) :shockwave))
       nil))
    (:shockwave
      (let* ((etap (top-level-interface view))
              (layout-# (layout etap))
              (layout (unless (zerop layout-#)
                        (get-layout (1- layout-#) (breakup etap))))
              (par-width (paragraph-width (breakup etap)))
              (result (when layout (bomb-shockwave-step bomb layout par-width))))
        (when (eq result :stop)
          (setf (bomb-phase bomb) :disappear)
          (setf (bomb-counter bomb)
                (sqrt (+ (* (/ par-width 2) (/ par-width 2))
                          (* (/ (+ (height layout) (depth layout)) 2)
                            (/ (+ (height layout) (depth layout)) 2))))))
        nil))

    (:disappear
      (let* ((etap (top-level-interface view))
              (layout-# (layout etap))
              (layout (unless (zerop layout-#)
                        (get-layout (1- layout-#) (breakup etap))))
              (par-width (paragraph-width (breakup etap)))
              (result (when layout (bomb-disappear-step bomb layout par-width))))
        (when (eq result :stop)
          (setf (bomb-phase bomb) :end-wait)
          (setf (bomb-counter bomb) -1))
        nil))

    (:end-wait
      (let ((result (bomb-wait-time bomb)))
        (when (eq result :stop)
          (bomb-reset-positions bomb)
          (setf (bomb-phase bomb) :tremble))
        result))
  )
)
             

; Installation
(defmethod living-text-install-animation ((animation (eql :bomb)) view)
  (let ((bomb (capi-object-property view :bomb)))
    (unless bomb
      (setq bomb (make-bomb
                    :phase :tremble
                    :tremble (caliber-default *bomb-tremble*)
                    :wait (caliber-default *bomb-wait*)
                    :shockwave (caliber-default *bomb-shockwave*)
                    :drop-speed (caliber-default *bomb-drop-speed*)
                    :obus nil
                    :counter -1
                    :hash (make-hash-table)))
      (setf (capi-object-property view :bomb) bomb))
    (let* ((etap (top-level-interface view))
            (layout-# (layout etap))
            (layout (unless (zerop layout-#)
                      (get-layout (1- layout-#) (breakup etap)))))
      (when layout
        (bomb-populate bomb layout)))
    (setf (capi-object-property view :elt-y-shift)
          (lambda (elt) (let ((bomb (capi-object-property view :bomb)))
          (if (and bomb (eq (bomb-phase bomb) :drop) (bomb-obus bomb))
            (let ((entry (find elt (bomb-obus bomb) :key #'first)))
              (if entry (fourth entry) (or (bomb-shift-y elt (bomb-hash bomb)) 0)))
            (or (bomb-shift-y elt (bomb-hash bomb)) 0)))))
    (setf (capi-object-property view :elt-x-shift)
          (lambda (elt)
        (let ((bomb (capi-object-property view :bomb)))
          (if (and bomb (eq (bomb-phase bomb) :drop) (bomb-obus bomb))
            (let ((entry (find elt (bomb-obus bomb) :key #'first)))
              (if entry (fifth entry) (or (bomb-shift-x elt (bomb-hash bomb)) 0)))
            (or (bomb-shift-x elt (bomb-hash bomb)) 0)))))
    (setf (capi-object-property view :living-text-step)
          (lambda () (bomb-step bomb view)))))