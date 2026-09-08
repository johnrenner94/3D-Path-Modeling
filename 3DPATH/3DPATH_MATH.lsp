;; ============================================================
;; 3DPATH_Math.lsp
;;
;; General vector/math helper functions for 3DPATH
;; ============================================================


;; ------------------------------------------------------------
;; VECTOR ADDITION
;; ------------------------------------------------------------

(defun 3dp-v+ (a b)
  (mapcar '+ a b)
)


;; ------------------------------------------------------------
;; VECTOR SUBTRACTION
;; ------------------------------------------------------------

(defun 3dp-v- (a b)
  (mapcar '- a b)
)


;; ------------------------------------------------------------
;; VECTOR * SCALAR
;; ------------------------------------------------------------

(defun 3dp-v* (v s)

  (mapcar
    '(lambda (x) (* x s))
    v
  )
)


;; ------------------------------------------------------------
;; DOT PRODUCT
;; ------------------------------------------------------------

(defun 3dp-vdot (a b)

  (+
    (* (car a)   (car b))
    (* (cadr a)  (cadr b))
    (* (caddr a) (caddr b))
  )
)


;; ------------------------------------------------------------
;; CROSS PRODUCT
;; ------------------------------------------------------------

(defun 3dp-vcross (a b)

  (list

    (-
      (* (cadr a) (caddr b))
      (* (caddr a) (cadr b))
    )

    (-
      (* (caddr a) (car b))
      (* (car a)   (caddr b))
    )

    (-
      (* (car a)  (cadr b))
      (* (cadr a) (car b))
    )
  )
)


;; ------------------------------------------------------------
;; VECTOR MAGNITUDE
;; ------------------------------------------------------------

(defun 3dp-vmag (v)

  (sqrt
    (3dp-vdot v v)
  )
)


;; ------------------------------------------------------------
;; UNIT VECTOR
;; ------------------------------------------------------------

(defun 3dp-vunit (v / m)

  (setq m
    (3dp-vmag v)
  )

  (if (< m 1.0e-12)

    nil

    (3dp-v*
      v
      (/ 1.0 m)
    )
  )
)


;; ------------------------------------------------------------
;; ANGLE BETWEEN VECTORS
;;
;; Returns radians from 0 to PI.
;;
;; Uses:
;;
;; atan(|A x B|, A dot B)
;; ------------------------------------------------------------

(defun 3dp-vang (a b / ua ub crossMag dotVal)

  (setq ua
    (3dp-vunit a)
  )

  (setq ub
    (3dp-vunit b)
  )

  (if
    (or
      (null ua)
      (null ub)
    )

    nil

    (progn

      (setq crossMag
        (3dp-vmag
          (3dp-vcross ua ub)
        )
      )

      (setq dotVal
        (3dp-vdot ua ub)
      )

      (atan crossMag dotVal)
    )
  )
)


;; ------------------------------------------------------------
;; TANGENT
;; ------------------------------------------------------------

(defun 3dp-tan (a)

  (/
    (sin a)
    (cos a)
  )
)


;; ------------------------------------------------------------
;; MIDPOINT BETWEEN TWO POINTS
;; ------------------------------------------------------------

(defun 3dp-midpoint (a b)

  (list

    (/ (+ (car a) (car b)) 2.0)

    (/ (+ (cadr a) (cadr b)) 2.0)

    (/ (+ (caddr a) (caddr b)) 2.0)
  )
)


;; ------------------------------------------------------------
;; RODRIGUES ROTATION
;;
;; Rotate vector V around unit vector K by angle A.
;; ------------------------------------------------------------

(defun 3dp-vrotate (v k a)

  (3dp-v+

    (3dp-v+

      (3dp-v*
        v
        (cos a)
      )

      (3dp-v*
        (3dp-vcross k v)
        (sin a)
      )
    )

    (3dp-v*
      k

      (*
        (3dp-vdot k v)
        (- 1.0 (cos a))
      )
    )
  )
)


;; ------------------------------------------------------------
;; DROP LAST ITEM FROM LIST
;; ------------------------------------------------------------

(defun 3dp-drop-last (lst)

  (if (> (length lst) 1)

    (reverse
      (cdr
        (reverse lst)
      )
    )

    nil
  )
)


;; ------------------------------------------------------------
;; GET VALUE FROM ASSOCIATION LIST
;;
;; Example:
;;
;; (3dp-get 'radius bend)
;; ------------------------------------------------------------

(defun 3dp-get (key data)

  (cdr
    (assoc key data)
  )
)


(princ)