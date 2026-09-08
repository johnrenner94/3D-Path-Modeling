;; ============================================================
;; 3DPATH_Draw.lsp
;;
;; AutoCAD drawing and labeling functions for 3DPATH
;; ============================================================


;; ------------------------------------------------------------
;; DRAW CENTERLINE AS 3D POLYLINE
;; ------------------------------------------------------------

(defun 3dp-draw-centerline (pathPts / p)

  (command "_.3DPOLY")

  (foreach p pathPts
    (command p)
  )

  (command "")
)


;; ------------------------------------------------------------
;; CREATE TEXT ENTITY
;;
;; Text is inserted at the supplied XYZ coordinate.
;;
;; NOTE:
;; The text itself lies in the WCS XY plane.
;; Its insertion POINT is still genuinely located in 3D space.
;; ------------------------------------------------------------

(defun 3dp-make-text
  (
    pt
    height
    textString
    /
    textStyle
  )

  (setq textStyle
    (getvar "TEXTSTYLE")
  )


  (entmakex

    (list

      '(0 . "TEXT")

      '(100 . "AcDbEntity")

      '(100 . "AcDbText")

      (cons 10 pt)

      (cons 40 height)

      (cons 1 textString)

      (cons 50 0.0)

      (cons 7 textStyle)

      '(210 0.0 0.0 1.0)
    )
  )
)


;; ------------------------------------------------------------
;; LABEL ALL ACTUAL STRAIGHT AND ARC SECTIONS
;; ------------------------------------------------------------

(defun 3dp-label-sections
  (
    straights
    bends
    textHeight
    /
    section
    bend
    arcNum
    pt
    label
  )

  ;; Skip if user selected zero-height labels
  (if (> textHeight 0.0)

    (progn


      ;; ========================================================
      ;; STRAIGHT SECTION LABELS
      ;; ========================================================

      (foreach section straights

        (setq pt
          (3dp-get 'midpoint section)
        )

        ;; Move text slightly upward in WCS Z
        ;; so it doesn't sit directly on the centerline.
        (setq pt

          (3dp-v+
            pt
            (list
              0.0
              0.0
              (* textHeight 0.5)
            )
          )
        )


        (setq label

          (strcat

            "S"
            (itoa
              (3dp-get 'number section)
            )

            "  L="
            (rtos
              (3dp-get 'length section)
              2
              4
            )
          )
        )


        (3dp-make-text
          pt
          textHeight
          label
        )
      )


      ;; ========================================================
      ;; ARC LABELS
      ;; ========================================================

      (setq arcNum 1)


      (foreach bend bends

        (if
          (= (3dp-get 'type bend) 'BEND)

          (progn

            (setq pt
              (3dp-get 'arc-midpoint bend)
            )


            ;; Slight Z offset
            (setq pt

              (3dp-v+
                pt
                (list
                  0.0
                  0.0
                  (* textHeight 0.5)
                )
              )
            )


            (setq label

              (strcat

                "A"
                (itoa arcNum)

                "  P"
                (itoa
                  (3dp-get 'point bend)
                )

                "  L="
                (rtos
                  (3dp-get 'arc-length bend)
                  2
                  4
                )

                "  R="
                (rtos
                  (3dp-get 'radius bend)
                  2
                  4
                )
              )
            )


            (3dp-make-text
              pt
              textHeight
              label
            )


            (setq arcNum
              (+ arcNum 1)
            )
          )
        )
      )
    )
  )
)


(princ)