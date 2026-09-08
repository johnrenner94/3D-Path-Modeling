;; ============================================================
;; 3DPATH.lsp
;;
;; Main 3DPATH application
;; ============================================================


;; ============================================================
;; LOAD SUPPORT FILES
;; ============================================================

(load "3DPATH_Math")
(load "3DPATH_Geometry")
(load "3DPATH_Draw")


;; ============================================================
;; MAIN COMMAND
;; ============================================================

(defun c:3DPATH
  (
    /
    defaultDia
    defaultRad
    arcSteps

    cableDia
    textHeight
    defaultTextHeight

    pts
    radii
    newpt
    rad
    finished
    pointNum

    i
    p0
    p1
    p2

    bend
    bends
    bendType

    pathPts
    straights
    warnings

    totalStraight
    totalArc
    totalLength
    sharpLength

    section
    arcNum
    warning
    validGeometry
  )


  ;; ==========================================================
  ;; DEFAULT SETTINGS
  ;; ==========================================================

  (setq defaultDia 0.141)

  ;; CENTERLINE bend radius
  (setq defaultRad 0.425)

  ;; Number of segments used to visually represent each arc
  (setq arcSteps 40)


  ;; ==========================================================
  ;; INTRO
  ;; ==========================================================

  (prompt "\n")
  (prompt "\n==================================================")
  (prompt "\n                     3DPATH")
  (prompt "\n==================================================")

  (prompt "\nStart point P0 is fixed at 0,0,0.")

  (prompt
    "\nFirst enter ALL theoretical XYZ intersection points."
  )

  (prompt
    "\nAfter Finish, 3DPATH will ask for all bend radii."
  )

  (prompt
    "\nCoordinates are entered as X,Y,Z."
  )

  (prompt
    "\nBend radius means CENTERLINE bend radius."
  )


  ;; ==========================================================
  ;; CABLE DIAMETER
  ;; ==========================================================

  (initget 6)

  (setq cableDia

    (getreal

      (strcat
        "\nCable diameter <"
        (rtos defaultDia 2 4)
        ">: "
      )
    )
  )


  (if (null cableDia)
    (setq cableDia defaultDia)
  )


  ;; ==========================================================
  ;; LABEL TEXT HEIGHT
  ;;
  ;; Default based somewhat on cable size.
  ;;
  ;; User may enter 0 to disable section labels.
  ;; ==========================================================

  (setq defaultTextHeight
    (max
      0.100
      (* cableDia 1.5)
    )
  )


  ;; 4 = disallow negative, allow zero
  (initget 4)

  (setq textHeight

    (getreal

      (strcat
        "\n3D section label text height <"
        (rtos defaultTextHeight 2 4)
        "> (0 for no labels): "
      )
    )
  )


  (if (null textHeight)
    (setq textHeight defaultTextHeight)
  )


  ;; ==========================================================
  ;; INITIALIZE POINT LIST
  ;; ==========================================================

  (setq pts

    (list
      (list 0.0 0.0 0.0)
    )
  )

  (setq finished nil)


  ;; ==========================================================
  ;; PHASE 1
  ;;
  ;; ENTER ALL POINTS
  ;; ==========================================================

  (prompt "\n")
  (prompt "\n---------------- POINT ENTRY ----------------")


  (while (not finished)

    (setq pointNum
      (length pts)
    )


    (initget "Finish Undo")


    (setq newpt

      (getpoint

        (strcat
          "\nNext point P"
          (itoa pointNum)
          " or [Finish/Undo]: "
        )
      )
    )


    (cond


      ;; --------------------------------------------------------
      ;; FINISH POINT ENTRY
      ;; --------------------------------------------------------

      (
        (equal newpt "Finish")

        (if (< (length pts) 2)

          (prompt
            "\nAt least one point after P0 is required."
          )

          (setq finished T)
        )
      )


      ;; --------------------------------------------------------
      ;; UNDO LAST POINT
      ;; --------------------------------------------------------

      (
        (equal newpt "Undo")

        (if (= (length pts) 1)

          (prompt
            "\nNothing to undo."
          )

          (progn

            (prompt
              (strcat
                "\nP"
                (itoa (- (length pts) 1))
                " removed."
              )
            )

            (setq pts
              (3dp-drop-last pts)
            )
          )
        )
      )


      ;; --------------------------------------------------------
      ;; XYZ POINT
      ;; --------------------------------------------------------

      (
        T

        ;; Reject duplicate point
        (if
          (<
            (distance
              (last pts)
              newpt
            )
            1.0e-10
          )

          (prompt
            "\nPoint is identical to previous point. Try again."
          )

          (progn

            (setq pts

              (append
                pts
                (list newpt)
              )
            )

            (prompt

              (strcat

                "\nP"
                (itoa (- (length pts) 1))
                " = "

                (rtos (car newpt) 2 4)
                ","

                (rtos (cadr newpt) 2 4)
                ","

                (rtos (caddr newpt) 2 4)
              )
            )
          )
        )
      )
    )
  )


  ;; ==========================================================
  ;; POINT SUMMARY
  ;; ==========================================================

  (prompt "\n")

  (prompt

    (strcat
      "\nPoint entry complete. "
      (itoa (length pts))
      " total points."
    )
  )


  ;; ==========================================================
  ;; PHASE 2
  ;;
  ;; ENTER ALL BEND RADII
  ;; ==========================================================

  (setq radii '())


  (if (> (length pts) 2)

    (progn

      (prompt "\n")
      (prompt "\n--------------- BEND RADII ----------------")


      ;; Interior points only:
      ;;
      ;; P1 through P(n-2)
      (setq i 1)


      (while (< i (- (length pts) 1))

        (initget 6)


        (setq rad

          (getreal

            (strcat
              "\nCenterline bend radius at P"
              (itoa i)
              " <"
              (rtos defaultRad 2 4)
              ">: "
            )
          )
        )


        ;; Enter = default
        (if (null rad)
          (setq rad defaultRad)
        )


        (setq radii

          (append
            radii
            (list rad)
          )
        )


        (setq i
          (+ i 1)
        )
      )
    )
  )


  ;; ==========================================================
  ;; PHASE 3
  ;;
  ;; CALCULATE BENDS
  ;; ==========================================================

  (setq bends '())

  (setq validGeometry T)


  (setq i 1)


  (while
    (and
      (< i (- (length pts) 1))
      validGeometry
    )

    (setq p0
      (nth (- i 1) pts)
    )

    (setq p1
      (nth i pts)
    )

    (setq p2
      (nth (+ i 1) pts)
    )


    (setq rad
      (nth (- i 1) radii)
    )


    (setq bend

      (3dp-calc-bend
        i
        p0
        p1
        p2
        rad
      )
    )


    (setq bendType
      (3dp-get 'type bend)
    )


    ;; Geometry error
    (if (= bendType 'ERROR)

      (progn

        (prompt "\n")

        (prompt

          (strcat
            "\nERROR at P"
            (itoa i)
            ": "
            (3dp-get 'message bend)
          )
        )

        (setq validGeometry nil)
      )


      ;; Valid bend
      (setq bends

        (append
          bends
          (list bend)
        )
      )
    )


    (setq i
      (+ i 1)
    )
  )


  ;; ==========================================================
  ;; CONTINUE ONLY IF GEOMETRY IS VALID
  ;; ==========================================================

  (if validGeometry

    (progn


      ;; ========================================================
      ;; BUILD ACTUAL STRAIGHT SECTIONS
      ;; ========================================================

      (setq straights

        (3dp-build-straights
          pts
          bends
        )
      )


      ;; ========================================================
      ;; BUILD DISPLAY CENTERLINE
      ;; ========================================================

      (setq pathPts

        (3dp-build-path
          pts
          bends
          arcSteps
        )
      )


      ;; ========================================================
      ;; FIT CHECKS
      ;; ========================================================

      (setq warnings

        (3dp-check-fit
          pts
          bends
        )
      )


      ;; ========================================================
      ;; SHARP THEORETICAL LENGTH
      ;; ========================================================

      (setq sharpLength 0.0)

      (setq i 0)


      (while (< i (- (length pts) 1))

        (setq sharpLength

          (+
            sharpLength

            (distance
              (nth i pts)
              (nth (+ i 1) pts)
            )
          )
        )

        (setq i (+ i 1))
      )


      ;; ========================================================
      ;; TOTAL ACTUAL STRAIGHT LENGTH
      ;; ========================================================

      (setq totalStraight 0.0)


      (foreach section straights

        (setq totalStraight

          (+
            totalStraight
            (3dp-get 'length section)
          )
        )
      )


      ;; ========================================================
      ;; TOTAL EXACT ARC LENGTH
      ;; ========================================================

      (setq totalArc 0.0)


      (foreach bend bends

        (setq totalArc

          (+
            totalArc
            (3dp-get 'arc-length bend)
          )
        )
      )


      ;; ========================================================
      ;; ACTUAL FINISHED CENTERLINE LENGTH
      ;; ========================================================

      (setq totalLength

        (+
          totalStraight
          totalArc
        )
      )


      ;; ========================================================
      ;; DRAW CENTERLINE
      ;; ========================================================

      (3dp-draw-centerline
        pathPts
      )


      ;; ========================================================
      ;; LABEL STRAIGHT / ARC SECTIONS IN MODEL SPACE
      ;; ========================================================

      (3dp-label-sections

        straights

        bends

        textHeight
      )


      ;; ========================================================
      ;; FULL REPORT
      ;; ========================================================

      (prompt "\n")
      (prompt "\n==================================================")
      (prompt "\n                  3DPATH REPORT")
      (prompt "\n==================================================")


      (prompt

        (strcat
          "\nCable diameter:             "
          (rtos cableDia 2 4)
        )
      )


      (prompt

        (strcat
          "\nTheoretical points:         "
          (itoa (length pts))
        )
      )


      (prompt

        (strcat
          "\nInterior turns:             "
          (itoa (length bends))
        )
      )


      ;; ========================================================
      ;; STRAIGHT SECTIONS
      ;; ========================================================

      (prompt "\n")
      (prompt "\nSTRAIGHT SECTIONS")
      (prompt "\n--------------------------------------------------")


      (foreach section straights

        (prompt

          (strcat

            "\nS"
            (itoa
              (3dp-get 'number section)
            )

            "   Length = "
            (rtos
              (3dp-get 'length section)
              2
              6
            )
          )
        )
      )


      ;; ========================================================
      ;; ARC SECTIONS
      ;; ========================================================

      (prompt "\n")
      (prompt "\nARC SECTIONS")
      (prompt "\n--------------------------------------------------")


      (setq arcNum 1)


      (foreach bend bends

        (if
          (= (3dp-get 'type bend) 'BEND)

          (progn

            (prompt

              (strcat

                "\nA"
                (itoa arcNum)

                "   P"
                (itoa
                  (3dp-get 'point bend)
                )

                "   R = "
                (rtos
                  (3dp-get 'radius bend)
                  2
                  4
                )

                "   Angle = "
                (rtos
                  (*
                    (3dp-get 'delta bend)
                    (/ 180.0 pi)
                  )
                  2
                  3
                )

                " deg"

                "   Length = "
                (rtos
                  (3dp-get 'arc-length bend)
                  2
                  6
                )
              )
            )


            (setq arcNum
              (+ arcNum 1)
            )
          )
        )
      )


      ;; ========================================================
      ;; TOTALS
      ;; ========================================================

      (prompt "\n")
      (prompt "\nLENGTH TOTALS")
      (prompt "\n--------------------------------------------------")


      (prompt

        (strcat
          "\nSharp theoretical path:     "
          (rtos sharpLength 2 6)
        )
      )


      (prompt

        (strcat
          "\nActual straight length:     "
          (rtos totalStraight 2 6)
        )
      )


      (prompt

        (strcat
          "\nActual circular arc length: "
          (rtos totalArc 2 6)
        )
      )


      (prompt
        "\n                               ------------"
      )


      (prompt

        (strcat
          "\nFINISHED CENTERLINE LENGTH: "
          (rtos totalLength 2 6)
        )
      )


      ;; ========================================================
      ;; GEOMETRY CHECK
      ;; ========================================================

      (prompt "\n")
      (prompt "\nGEOMETRY CHECK")
      (prompt "\n--------------------------------------------------")


      (if (null warnings)

        (prompt
          "\nPASS - No bend overlap detected."
        )

        (progn

          (prompt
            "\nWARNING - Geometry issues detected:"
          )

          (foreach warning warnings

            (prompt
              (strcat
                "\n  "
                warning
              )
            )
          )
        )
      )


      ;; ========================================================
      ;; COMPLETE
      ;; ========================================================

      (prompt "\n")
      (prompt "\n3D centerline created.")

      (if (> textHeight 0.0)

        (prompt
          "\nStraight and arc section labels created."
        )
      )

      (prompt "\n")
    )


    ;; Invalid geometry
    (progn

      (prompt "\n")

      (prompt
        "\n3DPATH stopped. No path was generated."
      )
    )
  )


  (princ)
)


;; ============================================================
;; LOAD MESSAGE
;; ============================================================

(princ "\n3DPATH loaded successfully.")
(princ "\nType 3DPATH to begin.")
(princ)