;; ============================================================
;; 3DPATH_Geometry.lsp
;;
;; Bend and path geometry for 3DPATH
;; ============================================================


;; ------------------------------------------------------------
;; CALCULATE ONE BEND
;;
;; INPUT:
;;
;; idx = theoretical point number
;; p0  = previous theoretical point
;; p1  = corner
;; p2  = next theoretical point
;; rad = centerline bend radius
;;
;; RETURNS:
;;
;; Association list containing all bend geometry.
;; ------------------------------------------------------------

(defun 3dp-calc-bend
  (
    idx
    p0
    p1
    p2
    rad
    /
    u1
    u2
    alpha
    delta
    setback
    tangentIn
    tangentOut
    bisector
    centerDist
    center
    rStart
    rEnd
    normal
    arcMid
  )


  ;; Directions pointing AWAY from theoretical corner
  (setq u1
    (3dp-vunit
      (3dp-v- p0 p1)
    )
  )

  (setq u2
    (3dp-vunit
      (3dp-v- p2 p1)
    )
  )


  ;; Invalid zero-length segment
  (if
    (or
      (null u1)
      (null u2)
    )

    ;; Return error record
    (list
      (cons 'type 'ERROR)
      (cons 'point idx)
      (cons 'message "Zero-length segment.")
    )


    ;; Continue
    (progn

      ;; Included angle
      (setq alpha
        (3dp-vang u1 u2)
      )


      ;; Deflection / bend angle
      ;;
      ;; delta = PI - alpha
      (setq delta
        (- pi alpha)
      )


      ;; --------------------------------------------------------
      ;; STRAIGHT POINT
      ;; --------------------------------------------------------

      (cond

        (
          (< (abs delta) 1.0e-8)

          (list

            (cons 'type 'STRAIGHT)

            (cons 'point idx)

            (cons 'radius rad)

            (cons 'alpha alpha)

            (cons 'delta 0.0)

            (cons 'setback 0.0)

            (cons 'tangent-in p1)

            (cons 'tangent-out p1)

            (cons 'arc-length 0.0)

            (cons 'arc-midpoint p1)
          )
        )


        ;; ------------------------------------------------------
        ;; APPROXIMATE 180-DEGREE REVERSAL
        ;; ------------------------------------------------------

        (
          (< alpha 1.0e-8)

          (list

            (cons 'type 'ERROR)

            (cons 'point idx)

            (cons
              'message
              "Approximately 180-degree reversal; finite tangent bend cannot be constructed."
            )
          )
        )


        ;; ------------------------------------------------------
        ;; NORMAL BEND
        ;; ------------------------------------------------------

        (
          T

          ;; Tangent setback
          ;;
          ;; T = R tan(delta / 2)
          (setq setback

            (*
              rad
              (3dp-tan
                (/ delta 2.0)
              )
            )
          )


          ;; Incoming tangent point
          (setq tangentIn

            (3dp-v+
              p1
              (3dp-v* u1 setback)
            )
          )


          ;; Outgoing tangent point
          (setq tangentOut

            (3dp-v+
              p1
              (3dp-v* u2 setback)
            )
          )


          ;; Angle bisector
          (setq bisector

            (3dp-vunit
              (3dp-v+ u1 u2)
            )
          )


          (if
            (null bisector)

            (list

              (cons 'type 'ERROR)

              (cons 'point idx)

              (cons
                'message
                "Could not determine bend bisector."
              )
            )


            ;; Bisector valid
            (progn

              ;; Circle center distance
              ;;
              ;; D = R / cos(delta/2)
              (setq centerDist

                (/
                  rad
                  (cos
                    (/ delta 2.0)
                  )
                )
              )


              ;; Circle center
              (setq center

                (3dp-v+
                  p1

                  (3dp-v*
                    bisector
                    centerDist
                  )
                )
              )


              ;; Radius vectors
              (setq rStart
                (3dp-v-
                  tangentIn
                  center
                )
              )

              (setq rEnd
                (3dp-v-
                  tangentOut
                  center
                )
              )


              ;; Plane normal
              (setq normal

                (3dp-vunit
                  (3dp-vcross rStart rEnd)
                )
              )


              (if
                (null normal)

                (list

                  (cons 'type 'ERROR)

                  (cons 'point idx)

                  (cons
                    'message
                    "Could not determine bend plane."
                  )
                )


                ;; Valid bend
                (progn

                  ;; Midpoint of exact circular arc
                  (setq arcMid

                    (3dp-v+
                      center

                      (3dp-vrotate
                        rStart
                        normal
                        (/ delta 2.0)
                      )
                    )
                  )


                  ;; Return complete bend record
                  (list

                    (cons 'type 'BEND)

                    (cons 'point idx)

                    (cons 'radius rad)

                    (cons 'alpha alpha)

                    (cons 'delta delta)

                    (cons 'setback setback)

                    (cons 'tangent-in tangentIn)

                    (cons 'tangent-out tangentOut)

                    (cons 'center center)

                    (cons 'r-start rStart)

                    (cons 'r-end rEnd)

                    (cons 'normal normal)

                    (cons
                      'arc-length
                      (* rad delta)
                    )

                    (cons
                      'arc-midpoint
                      arcMid
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


;; ============================================================
;; BUILD DISPLAY CENTERLINE POINTS
;;
;; Circular arcs are represented by small 3DPOLY segments.
;;
;; Calculated length still uses exact R * theta.
;; ============================================================

(defun 3dp-build-path
  (
    pts
    bends
    arcSteps
    /
    path
    bend
    bendType
    tangentIn
    tangentOut
    center
    rStart
    normal
    delta
    j
    theta
    p
  )

  (setq path
    (list
      (car pts)
    )
  )


  (foreach bend bends

    (setq bendType
      (3dp-get 'type bend)
    )


    ;; ----------------------------------------------------------
    ;; ACTUAL BEND
    ;; ----------------------------------------------------------

    (if (= bendType 'BEND)

      (progn

        (setq tangentIn
          (3dp-get 'tangent-in bend)
        )

        (setq tangentOut
          (3dp-get 'tangent-out bend)
        )

        (setq center
          (3dp-get 'center bend)
        )

        (setq rStart
          (3dp-get 'r-start bend)
        )

        (setq normal
          (3dp-get 'normal bend)
        )

        (setq delta
          (3dp-get 'delta bend)
        )


        ;; Add incoming tangent point
        (if
          (>
            (distance
              (last path)
              tangentIn
            )
            1.0e-9
          )

          (setq path
            (append path (list tangentIn))
          )
        )


        ;; Generate points along arc
        (setq j 1)

        (while (<= j arcSteps)

          (setq theta

            (*
              delta

              (/
                (float j)
                (float arcSteps)
              )
            )
          )


          (setq p

            (3dp-v+
              center

              (3dp-vrotate
                rStart
                normal
                theta
              )
            )
          )


          (if
            (>
              (distance
                (last path)
                p
              )
              1.0e-9
            )

            (setq path
              (append path (list p))
            )
          )


          (setq j (+ j 1))
        )
      )


      ;; --------------------------------------------------------
      ;; STRAIGHT THEORETICAL POINT
      ;; --------------------------------------------------------

      (progn

        (setq p
          (3dp-get 'tangent-in bend)
        )

        (if
          (>
            (distance
              (last path)
              p
            )
            1.0e-9
          )

          (setq path
            (append
              path
              (list p)
            )
          )
        )
      )
    )
  )


  ;; Final endpoint
  (if
    (>
      (distance
        (last path)
        (last pts)
      )
      1.0e-9
    )

    (setq path

      (append
        path
        (list (last pts))
      )
    )
  )


  path
)


;; ============================================================
;; BUILD ACTUAL STRAIGHT SECTIONS
;;
;; Each record contains:
;;
;; number
;; start
;; end
;; midpoint
;; length
;; ============================================================

(defun 3dp-build-straights
  (
    pts
    bends
    /
    result
    currentPoint
    endPoint
    bend
    len
    sectionNum
  )

  (setq result '())

  (setq currentPoint
    (car pts)
  )

  (setq sectionNum 1)


  ;; Each bend determines the end of the preceding straight
  (foreach bend bends

    (setq endPoint
      (3dp-get 'tangent-in bend)
    )

    (setq len
      (distance
        currentPoint
        endPoint
      )
    )


    ;; Ignore zero-length straight sections
    (if (> len 1.0e-9)

      (progn

        (setq result

          (append
            result

            (list

              (list

                (cons
                  'number
                  sectionNum
                )

                (cons
                  'start
                  currentPoint
                )

                (cons
                  'end
                  endPoint
                )

                (cons
                  'midpoint
                  (3dp-midpoint
                    currentPoint
                    endPoint
                  )
                )

                (cons
                  'length
                  len
                )
              )
            )
          )
        )

        (setq sectionNum
          (+ sectionNum 1)
        )
      )
    )


    ;; Next straight begins after this bend
    (setq currentPoint
      (3dp-get 'tangent-out bend)
    )
  )


  ;; Final straight section
  (setq endPoint
    (last pts)
  )

  (setq len
    (distance
      currentPoint
      endPoint
    )
  )


  (if (> len 1.0e-9)

    (setq result

      (append
        result

        (list

          (list

            (cons
              'number
              sectionNum
            )

            (cons
              'start
              currentPoint
            )

            (cons
              'end
              endPoint
            )

            (cons
              'midpoint
              (3dp-midpoint
                currentPoint
                endPoint
              )
            )

            (cons
              'length
              len
            )
          )
        )
      )
    )
  )


  result
)


;; ============================================================
;; GEOMETRY FIT CHECK
;;
;; Returns a list of warning strings.
;; ============================================================

(defun 3dp-check-fit
  (
    pts
    bends
    /
    warnings
    i
    bend
    setback
    incomingLength
    outgoingLength
    leftBend
    rightBend
    segmentLength
    required
  )

  (setq warnings '())


  ;; ----------------------------------------------------------
  ;; CHECK EACH INDIVIDUAL BEND
  ;; ----------------------------------------------------------

  (setq i 1)

  (foreach bend bends

    (setq setback
      (3dp-get 'setback bend)
    )

    (setq incomingLength

      (distance
        (nth (- i 1) pts)
        (nth i pts)
      )
    )

    (setq outgoingLength

      (distance
        (nth i pts)
        (nth (+ i 1) pts)
      )
    )


    (if
      (> setback (+ incomingLength 1.0e-9))

      (setq warnings

        (append
          warnings

          (list

            (strcat
              "Bend at P"
              (itoa i)
              " exceeds its incoming segment."
            )
          )
        )
      )
    )


    (if
      (> setback (+ outgoingLength 1.0e-9))

      (setq warnings

        (append
          warnings

          (list

            (strcat
              "Bend at P"
              (itoa i)
              " exceeds its outgoing segment."
            )
          )
        )
      )
    )


    (setq i (+ i 1))
  )


  ;; ----------------------------------------------------------
  ;; CHECK ADJACENT BENDS
  ;;
  ;; T_left + T_right <= segment length
  ;; ----------------------------------------------------------

  (setq i 0)

  (while (< i (- (length bends) 1))

    (setq leftBend
      (nth i bends)
    )

    (setq rightBend
      (nth (+ i 1) bends)
    )

    (setq segmentLength

      (distance
        (nth (+ i 1) pts)
        (nth (+ i 2) pts)
      )
    )

    (setq required

      (+
        (3dp-get 'setback leftBend)
        (3dp-get 'setback rightBend)
      )
    )


    (if
      (> required (+ segmentLength 1.0e-9))

      (setq warnings

        (append
          warnings

          (list

            (strcat
              "Bends at P"
              (itoa (+ i 1))
              " and P"
              (itoa (+ i 2))
              " overlap. Segment="
              (rtos segmentLength 2 4)
              ", required="
              (rtos required 2 4)
            )
          )
        )
      )
    )


    (setq i (+ i 1))
  )


  warnings
)


(princ)