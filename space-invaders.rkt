;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;;========================================================================================

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

;(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define GO (text "GAME OVER" 25 "black"))
(define MESSAGE (overlay GO
                         (rectangle (+ (image-width GO) 10)
                                    (+ (image-height GO) 10)
                                    "solid" "white")))
                                    
                    

;;========================================================================================

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfInvader is one of:
;; -empty
;; (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; ListOfMissile in one of:
;; - empty
;; - (cons Missile ListOfMissile)

(define LOM0 empty)
(define LOM1 (cons M1 (cons M2 empty)))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom)
              (fn-for-lom (rest lom)))]))

;;========================================================================================

;; Functions

;; Game -> Game
;; start the world with (main START)
;; 
(define (main g)
  (big-bang g                                ; Game
            (on-tick   next-state)           ; Game -> Game
            (to-draw   render)               ; Game -> Image
            (stop-when game-over? game-over) ; Game -> Boolean
            (on-key    action)))             ; Game KeyEvent -> Game

;;=========NEXT==================================================

(define (next-state g)
  (make-game (remove-another (game-missiles (next g)) (game-invaders (next g)))
             (remove-another (game-invaders (next g)) (game-missiles (next g)))
             (game-tank (next g))))

;; Game -> Game
;; produce the next position of Invaders, Missiles and Tank

(define (next g)
  (make-game (create-invader (next-invader (game-invaders g)))
             (next-missile (game-missiles g))
             (next-tank (game-tank g))))

;;===HIT===

;; ListOfMissiles ListOfInvader -> ListOfInvader / ListOfInvader ListOfMissiles -> ListOfMissiles
;; remove missile if it hit an invader
(check-expect (remove-another empty (list (make-missile 60 70))) (list (make-missile 60 70)))
(check-expect (remove-another (list (make-missile 60 70)) empty) empty)
(check-expect (remove-another (list (make-missile 90 120)
                                    (make-missile 60 40))
                              (list (make-invader 60 40 1)
                                    (make-invader 120 150 -1)))
              (list (make-invader 120 150 -1)))
(check-expect (remove-another (list (make-invader 60 40 1)
                                    (make-invader 120 150 -1))
                              (list (make-missile 90 120)
                                    (make-missile 60 40)))
              (list (make-missile 90 120)))                          

(define (remove-another list1 list2)
  (cond [(empty? list1) list2]
        [(empty? (rest list1)) (remove-one (first list1) list2)]
        [else
         (remove-one (first list1) (remove-another (rest list1) list2))]))

;; Missile ListOfInvader -> ListOfInvader / Invader ListOfMissile -> ListOfMissile
;; if missile hits an invader, take out that invader from the list or vice versa
(check-expect (remove-one (make-missile 100 300) (list (make-invader 50 50 1)))
              (list (make-invader 50 50 1)))
(check-expect (remove-one (make-missile 100 150) (list (make-invader 50 50 1)
                                                           (make-invader 100 150 1)))
              (list (make-invader 50 50 1)))
(check-expect (remove-one (make-invader 100 150 1) (list (make-missile 50 50)
                                                         (make-missile 100 150)))
              (list (make-missile 50 50)))

(define (remove-one one list)
  (cond [(empty? list) empty]
        [(missile? one)
         (if (reached? one (first list))
             (rest list)
             (cons (first list) (remove-one one (rest list))))]
        [(invader? one)
         (if (reached-inv? one (first list))
             (rest list)
             (cons (first list) (remove-one one (rest list))))]))

;; Missile Invader -> Boolean
;; true if missile reaches invader
(check-expect (reached? (make-missile 50 50) (make-invader 100 100 1)) false)
(check-expect (reached? (make-missile 50 50) (make-invader 50 50 -1)) true)
(check-expect (reached? (make-missile (+ 70 (/ (image-width INVADER) 2) (- (/ (image-width MISSILE) 2) 1))
                                      (+ 70 (/ (image-height INVADER) 2) (- (/ (image-height MISSILE) 2) 1)))
                        (make-invader 70 70 1))
              true)
(check-expect (reached? (make-missile (- 80 (/ (image-width INVADER) 2) (+ (/ (image-width MISSILE) 2) 1))
                                      (+ 90 1))
                        (make-invader 80 90 -1))
              true)

(define (reached? m i)
  (and (<= (missile-x m) (+ (invader-x i) (/ (image-width INVADER) 2) (- (/ (image-width MISSILE) 2) 1)))
       (>= (missile-x m) (- (invader-x i) (/ (image-width INVADER) 2) (+ (/ (image-width MISSILE) 2) 1)))
       (<= (missile-y m) (+ (invader-y i) (/ (image-height INVADER) 2) (- (/ (image-height MISSILE) 2) 1)))
       (>= (missile-y m) (invader-y i))))

;; Invader Missile -> Boolean
;; true if invader reaches missile
(check-expect (reached-inv? (make-invader 70 90 -1) (make-missile 120 120)) false)
(check-expect (reached-inv? (make-invader 80 90 -1)
                            (make-missile (- 80 (/ (image-width INVADER) 2) (+ (/ (image-width MISSILE) 2) 1))
                                          (+ 90 1)))
              true)
(check-expect (reached-inv? (make-invader 100 120 1) (make-missile 95 125)) true)
(define (reached-inv? i m)
  (and (<= (missile-x m) (+ (invader-x i) (/ (image-width INVADER) 2) (- (/ (image-width MISSILE) 2) 1)))
       (>= (missile-x m) (- (invader-x i) (/ (image-width INVADER) 2) (+ (/ (image-width MISSILE) 2) 1)))
       (<= (missile-y m) (+ (invader-y i) (/ (image-height INVADER) 2) (- (/ (image-height MISSILE) 2) 1)))
       (>= (missile-y m) (invader-y i))))

;;===INVADER===

;; ListOfInvader -> ListOfInvader
;; randomly generate new invader

(define (create-invader loi)
  (if (or (= (random INVADE-RATE) 60) (= (random INVADE-RATE) 20) (= (random INVADE-RATE) 80))
      (cons (make-invader (random WIDTH) 0 (gen-dir 1)) loi)
      loi))

(define (gen-dir a)
  (if (odd? (random INVADE-RATE))
      1
      -1))

;; ListOfInvader -> ListOfInvader
;; produce next position of invader, if reaches the edge bounces off and turns back
(check-expect (next-invader empty) empty)
(check-expect (next-invader (list (make-invader 30 70 1)))
              (list (make-invader (+ 30 (* INVADER-X-SPEED 1)) (+ 70 INVADER-Y-SPEED) 1)))
(check-expect (next-invader (list (make-invader (/ (image-width INVADER) 2) 70 -1)))
              (list (make-invader (+ (/ (image-width INVADER) 2) (* INVADER-X-SPEED 1))
                                  (+ 70 INVADER-Y-SPEED)
                                  1)))
(check-expect (next-invader (list (make-invader (- WIDTH (/ (image-width INVADER) 2)) 30 1)))
              (list (make-invader (+ (- WIDTH (/ (image-width INVADER) 2)) (* INVADER-X-SPEED -1))
                                  (+ 30 INVADER-X-SPEED)
                                  -1)))

(define (next-invader loi)
  (cond [(empty? loi) empty]
        [(inv-edge-reached? (first loi))
         (cons (make-invader (+ (invader-x (first loi)) (* INVADER-X-SPEED (- (invader-dx (first loi)))))
                             (+ (invader-y (first loi)) INVADER-Y-SPEED)
                             (- (invader-dx (first loi))))
               (next-invader (rest loi)))]
        [else
         (cons (make-invader (+ (invader-x (first loi)) (* INVADER-X-SPEED (invader-dx (first loi))))
                             (+ (invader-y (first loi)) INVADER-Y-SPEED)
                             (invader-dx (first loi)))
               (next-invader (rest loi)))]))
                                

;; Invader -> Boolean
;; gives true if the invader reached the edges of the scene
(check-expect (inv-edge-reached? (make-invader 60 30 1)) false)
(check-expect (inv-edge-reached? (make-invader (/ (image-width INVADER) 2) 60 -1)) true)
(check-expect (inv-edge-reached? (make-invader 0 60 -1)) true)
(check-expect (inv-edge-reached? (make-invader (/ (image-width INVADER) 2) 60 1)) false)
(check-expect (inv-edge-reached? (make-invader (- WIDTH (/ (image-width INVADER) 2)) 60 1)) true)
(check-expect (inv-edge-reached? (make-invader WIDTH 60 1)) true)
(check-expect (inv-edge-reached? (make-invader (- WIDTH (/ (image-width INVADER) 2)) 60 -1)) false)

(define (inv-edge-reached? i)
  (or (and (<= (invader-x i) (/ (image-width INVADER) 2))
           (= (invader-dx i) -1))
      (and (>= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))
           (= (invader-dx i) 1))))

;;===MISSILE====

;; ListOfMissile -> ListOfMissile
;; produce next position of missile
(check-expect (next-missile empty) empty)
(check-expect (next-missile (list (make-missile 50 30)
                                  (make-missile 100 100)))
              (list (make-missile 50 (- 30 MISSILE-SPEED))
                    (make-missile 100 (- 100 MISSILE-SPEED))))
(check-expect (next-missile (list (make-missile 90 80)
                                  (make-missile 60 (- 0 (image-height MISSILE)))
                                  (make-missile 110 150)))
              (list (make-missile 90 (- 80 MISSILE-SPEED))
                    (make-missile 110 (- 150 MISSILE-SPEED))))

(define (next-missile lom)
  (cond [(empty? lom) empty]
        [(top-reached? (first lom)) (next-missile (rest lom))]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (next-missile (rest lom)))]))

;; Missile -> Boolean
;; gives true if missile leaved scene
(check-expect (top-reached? (make-missile 90 60)) false)
(check-expect (top-reached? (make-missile 150 (image-height MISSILE))) false)
(check-expect (top-reached? (make-missile 60 (- 0 (image-height MISSILE)))) true)
(check-expect (top-reached? (make-missile 100 -50)) true)

(define (top-reached? m)
  (<= (missile-y m) (- 0 (image-height MISSILE))))

;;=====TANK=====

;; Tank -> Tank
;; produce next position of tank
(check-expect (next-tank (make-tank 60 1))
              (make-tank (+ 60 (* TANK-SPEED 1)) 1))
(check-expect (next-tank (make-tank 70 -1))
              (make-tank (+ 70 (* TANK-SPEED -1)) -1))
(check-expect (next-tank (make-tank (/ (image-width TANK) 2) -1))
              (make-tank (/ (image-width TANK) 2) -1))
(check-expect (next-tank (make-tank (- (/ (image-width TANK) 2) 1) -1))
              (make-tank (/ (image-width TANK) 2) -1))
(check-expect (next-tank (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))
              (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))
(check-expect (next-tank (make-tank (+ (- WIDTH (/ (image-width TANK) 2)) 1) 1))
              (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))           

(define (next-tank t)
  (cond [(left-edge-reached? t)  (make-tank (/ (image-width TANK) 2) (tank-dir t))]
        [(right-edge-reached? t) (make-tank (- WIDTH (/ (image-width TANK) 2)) (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; Tank -> Boolean
;; gives true if the tank reached the left edge of the scene and direction is right to left (-1)
(check-expect (left-edge-reached? (make-tank 100 1)) false)
(check-expect (left-edge-reached? (make-tank (/ (image-width TANK) 2) -1)) true)
(check-expect (left-edge-reached? (make-tank (/ (image-width TANK) 2) 1)) false)
(check-expect (left-edge-reached? (make-tank (- (/ (image-width TANK) 2) 1) -1)) true)
(check-expect (left-edge-reached? (make-tank 0 -1)) true)

(define (left-edge-reached? t)
  (and (<= (tank-x t) (/ (image-width TANK) 2))
       (= (tank-dir t) -1)))

;; Tank -> Boolean
;; gives true if the tank reached the right edge of the scene and direction is from left to right (1)
(check-expect (right-edge-reached? (make-tank (/ WIDTH 2) 1)) false)
(check-expect (right-edge-reached? (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)) true)
(check-expect (right-edge-reached? (make-tank (- WIDTH (/ (image-width TANK) 2)) -1)) false)
(check-expect (right-edge-reached? (make-tank (+ (- WIDTH (/ (image-width TANK) 2)) 1) 1)) true)
(check-expect (right-edge-reached? (make-tank WIDTH 1)) true)

(define (right-edge-reached? t)
  (and (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
       (= (tank-dir t) 1)))

;;===================RENDER======================================

;; Game -> Image
;; render game components onto MTS on the appropriate coordinates
(check-expect (render (make-game (list (make-invader 50 40 1))
                                 (list (make-missile 60 90))
                                 (make-tank 70 1)))
              (place-images (list INVADER
                                  MISSILE
                                  TANK)
                            (list (make-posn 50 40)
                                  (make-posn 60 90)
                                  (make-posn 70 (- HEIGHT (/ (image-height TANK) 2))))
                            MTS))
(check-expect (render (make-game (list (make-invader 50 40 1)
                                       (make-invader 90 100 -1))
                                 (list (make-missile 60 90)
                                       (make-missile 70 150))
                                 (make-tank 80 -1)))
               (place-images (list INVADER
                                   INVADER
                                   MISSILE
                                   MISSILE
                                   TANK)
                             (list (make-posn 50 40)
                                   (make-posn 90 100)
                                   (make-posn 60 90)
                                   (make-posn 70 150)
                                   (make-posn 80 (- HEIGHT (/ (image-height TANK) 2))))
                             MTS))

(define (render g)
  (place-images (image-list g)
                (posn-list g)
                MTS))

;; Game -> ListOfImages
;; make a lists of images out of the components of the game
(check-expect (image-list (make-game empty empty (make-tank 50 1)))
              (list TANK))
(check-expect (image-list (make-game (list (make-invader 50 40 1)
                                           (make-invader 90 100 -1))
                                     (list (make-missile 60 90)
                                           (make-missile 70 150))
                                     (make-tank 80 -1)))
              (list INVADER INVADER MISSILE MISSILE TANK))

(define (image-list g)
  (append (img-list-invader (game-invaders g))
          (img-list-missile (game-missiles g))
          (list TANK)))

;; ListOfInvader -> ListOfImage
;; make a list of images out of the components of ListOfInvaders
(check-expect (img-list-invader empty) empty)
(check-expect (img-list-invader (list (make-invader 60 90 1)
                                      (make-invader 150 200 -1)))
              (list INVADER INVADER))

(define (img-list-invader loi)
  (cond [(empty? loi) empty]
        [else
         (append (list INVADER) (img-list-invader (rest loi)))]))

;; ListOfMissile -> ListOfImages
;; make a list of images out of the components of ListOfMissiles
(check-expect (img-list-missile empty) empty)
(check-expect (img-list-missile (list (make-missile 100 120)
                                      (make-missile 150 60)))
              (list MISSILE MISSILE))

(define (img-list-missile lom) 
  (cond [(empty? lom) empty]
        [else
         (append (list MISSILE) (img-list-missile (rest lom)))]))

;; Game -> ListOfPosition
;; make a list of positions out of components of the game
(check-expect (posn-list (make-game empty empty (make-tank 50 1)))
              (list (make-posn 50 (- HEIGHT (/ (image-height TANK) 2)))))
(check-expect (posn-list (make-game (list (make-invader 50 40 1)
                                          (make-invader 90 100 -1))
                                    (list (make-missile 60 90)
                                          (make-missile 70 150))
                                    (make-tank 80 -1)))
              (list (make-posn 50 40)
                    (make-posn 90 100)
                    (make-posn 60 90)
                    (make-posn 70 150)
                    (make-posn 80 (- HEIGHT (/ (image-height TANK) 2)))))
              
(define (posn-list g)
  (append (posn-list-invader (game-invaders g))
          (posn-list-missile (game-missiles g))
          (list (make-posn (tank-x (game-tank g)) (- HEIGHT (/ (image-height TANK) 2))))))

;; ListOfInvader -> ListOfPosn
;; make list of positions from the values of ListOfInvaders
(check-expect (posn-list-invader empty) empty)
(check-expect (posn-list-invader (list (make-invader 60 90 1)
                                       (make-invader 150 120 -1)))
              (list (make-posn 60 90)
                    (make-posn 150 120)))

(define (posn-list-invader loi)
  (cond [(empty? loi) empty]
        [else
         (append (list (make-posn (invader-x (first loi)) (invader-y (first loi))))
                 (posn-list-invader (rest loi)))]))

;; ListOfMissile -> ListOfPosn
;; make list of positions from the values of ListOfMissiles
(check-expect (posn-list-missile empty) empty)
(check-expect (posn-list-missile (list (make-missile 60 90)
                                       (make-missile 70 150)))
              (list (make-posn 60 90)
                    (make-posn 70 150)))

(define (posn-list-missile lom)
  (cond [(empty? lom) empty]
        [else
         (append (list (make-posn (missile-x (first lom)) (missile-y (first lom))))
                 (posn-list-missile (rest lom)))]))  


;;==================ACTION========================================
;; Game KeyEvent -> Game
;; make the tank change direction and fire
(check-expect (action (make-game empty empty (make-tank 60 1)) "left")
              (make-game empty empty (make-tank 60 -1)))
(check-expect (action (make-game empty empty (make-tank 90 -1)) "left")
              (make-game empty empty (make-tank 90 -1)))
(check-expect (action (make-game empty empty (make-tank 90 -1)) "right")
              (make-game empty empty (make-tank 90 1)))
(check-expect (action (make-game empty empty (make-tank 80 1)) "right")
              (make-game empty empty (make-tank 80 1)))
(check-expect (action (make-game empty empty (make-tank 80 1)) " ")
              (make-game empty
                         (list (make-missile 80 (- HEIGHT (image-height TANK))))
                         (make-tank 80 1)))
(check-expect (action (make-game empty
                                 (list (make-missile 100 150))
                                 (make-tank 160 1))
                      " ")
              (make-game empty
                         (list (make-missile 160 (- HEIGHT (image-height TANK)))
                               (make-missile 100 150))
                         (make-tank 160 1)))
(check-expect (action (make-game empty empty (make-tank 60 -1)) "d")
              (make-game empty empty (make-tank 60 -1)))

(define (action g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ") (make-game (game-invaders g)
                                   (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (image-height TANK)))
                                         (game-missiles g))
                                   (game-tank g))]
        [else g]))

;; ======================ENDGAME==================================

;; Game -> Boolean
;; give true when an invader ship reached land
(check-expect (game-over? (make-game (list (make-invader 50 40 1)
                                           (make-invader 90 100 -1)
                                           (make-invader 70 150 1))
                                     empty
                                     (make-tank 80 -1)))
              false)
(check-expect (game-over? (make-game (list (make-invader 50 40 1)
                                           (make-invader 90 (- HEIGHT (/ (image-height INVADER) 2)) -1))
                                     empty
                                     (make-tank 80 -1)))
              true)
                                           

(define (game-over? g)
  (invader-landed? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; give true when an invader ship reached land
(check-expect (invader-landed? empty) false)
(check-expect (invader-landed? (list (make-invader 50 40 1)
                                     (make-invader 90 100 -1)
                                     (make-invader 70 150 1)))
              false)
(check-expect (invader-landed? (list (make-invader 50 40 1)
                                     (make-invader 90 (- HEIGHT (/ (image-height INVADER) 2)) -1)))
              true)
(check-expect (invader-landed? (list (make-invader 50 40 1)
                                     (make-invader 90 (+ (- HEIGHT (/ (image-height INVADER) 2)) 3) -1)))
              true)

(define (invader-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) (- HEIGHT (/ (image-height INVADER) 2)))
             true
             (invader-landed? (rest loi)))]))

;; Game -> Image
;; render last game state with message that the game has ended
(check-expect (game-over (make-game (list (make-invader 60 80 1)
                                          (make-invader 150 200 -1))
                                    (list (make-missile 90 50)
                                          (make-missile 120 220))
                                    (make-tank 100 1)))
              (overlay/align "middle" "middle"
                             MESSAGE
                             (render (make-game (list (make-invader 60 80 1)
                                                      (make-invader 150 200 -1))
                                                (list (make-missile 90 50)
                                                      (make-missile 120 220))
                                                (make-tank 100 1)))))

(define (game-over g)
  (overlay/align "middle" "middle"
                 MESSAGE
                 (render g)))

;;========================START============================================

(define START (make-game (create-invader empty)
                         empty
                         (make-tank (random WIDTH) (gen-dir 1))))