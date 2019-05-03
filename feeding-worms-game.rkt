;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname feeding-worms-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Feeding worms

;; =================
;; Constants:

; WIDTH should be equal to HEIGHT
(define WIDTH 150)
(define HEIGHT 150)
(define BACKG (empty-scene WIDTH HEIGHT))                       

(define WORMHEAD (circle 7 "solid" "red"))
(define WORMTAIL (circle 7 "solid" "orange"))
(define D (image-height WORMHEAD))

(define FOOD (circle 7 "solid" "green"))

(define WALL-MESSAGE "Oh no, you hit the wall: ")
(define EAT-MESSAGE "Oh no, you can't eat yourself: ")

(define MAX-W (- WIDTH D))
(define MAX-H (- HEIGHT D))

;; =================
;; Data definitions:

(define-struct worm (body pos))
;; Worm is (make-worm [Image Posn])
;; interp. a piece of the worm (head or body) with its position

(define W1 (make-worm WORMHEAD (make-posn 10 20)))
(define W2 (make-worm WORMTAIL (make-posn 10 (+ 20 D))))
#;
(define (fn-for-worm w)
  (... (worm-body w)
       (worm-pos w)))

;; List-of-worm is one of:
;; - (cons Worm empty)
;; - (cons Worm List-of-worm)
;; interp. a worm that either consists of a head, or a head and the rest of its body

(define LOW1 (cons W1 empty))
(define LOW2 (cons W1 (cons W2 empty)))
(define LOW3 (cons W1 (cons W2 (cons (make-worm WORMTAIL (make-posn 10 (+ 20 D D))) empty))))
#;
(define (fn-for-low low)
  (cond [(empty? (rest low)) ...]
        [else
         (... (first low)
              (fn-for-low (rest low)))]))

(define-struct direction [dx dy])
;; Directioan is (make-direction Integer[-1,1] Integer[-1,1])
;; interp. the direction the worm is moving

(define L->R (make-direction 1 0))   ; left to right
(define R->L (make-direction -1 0))  ; right to left
(define B->T (make-direction 0 -1))  ; bottom to top
(define T->B (make-direction 0 1))   ; top to bottom

(define-struct game [wrm food dir])
;; Game is (make-game List-of-worm Posn)
;; interp. a game state with a worm and a piece of food

(define G1 (make-game LOW3 (make-posn 20 50) L->R))

                                               
;; ========================MAIN======================================================================
;; Functions:

;; Game -> Game
;; start the world with (main G)
;; 
(define (main g) 
  (big-bang g                                 ; Game
            (on-tick   next 0.8)                ; Game -> Game
            (to-draw   render)                ; Game -> Image
            (stop-when hit? last-image)  ; Game -> Boolean Image            
            (on-key    action)))              ; Game KeyEvent -> Game

;; ==============================NEXT================================================================

;; Game -> Game
;; produce the next state of thw worm and
;; if food was eaten produce next posn of food
(check-expect (next (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                     (make-worm WORMTAIL (make-posn 30 (- 50 D))))
                               (make-posn 100 100)
                               T->B))
              (make-game (list (make-worm WORMHEAD (make-posn 30 (+ 50 D)))
                               (make-worm WORMTAIL (make-posn 30 50)))
                         (make-posn 100 100)
                         T->B))
(check-expect (next (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                     (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                     (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                               (make-posn 100 100)
                               L->R))
              (make-game (list (make-worm WORMHEAD (make-posn (+ 30 D) 50))
                               (make-worm WORMTAIL (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50)))
                         (make-posn 100 100)
                         L->R))
(check-random (next (make-game (list (make-worm WORMHEAD (make-posn 30 50)))
                               (make-posn 30 50)
                               T->B))
              (make-game (list (make-worm WORMHEAD (make-posn 30 (+ 50 D)))
                               (make-worm WORMTAIL (make-posn 30 50)))
                         (food-create (make-posn 30 50))
                         T->B))
               
(define (next g)
  (cond [(reached? g) (make-game (longer-worm (game-wrm g) (game-dir g))
                                 (food-create (game-food g))
                                 (game-dir g))]
        [else (make-game (move-worm (game-wrm g) (game-dir g))
                         (game-food g)
                         (game-dir g))]))

;; Game -> Boolean
;; give true when the worms head reaches the food
(check-expect (reached? (make-game (list (make-worm WORMHEAD (make-posn 50 70)))
                                   (make-posn 50 70)
                                   T->B))
              true)
(check-expect (reached? (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                         (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                         (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                                   (make-posn 100 100)
                                   L->R))
              false)

(define (reached? g) 
  (and (close? (worm-pos-x (game-wrm g)) (posn-x (game-food g)))
       (close? (worm-pos-y (game-wrm g)) (posn-y (game-food g)))))

;; Number Number -> Boolean
;; give true if the two numbers are close enough to each other
(check-expect (close? 10 50) false)
(check-expect (close? 30 30) true)
(check-expect (close? 50 (+ 50 (- (/ D 2) 1))) true)
(check-expect (close? 60 (- 60 (- (/ D 2) 3))) true)

(define (close? n m)
  (<= (- m (+ (/ D 2) 1)) n (+ m (- (/ D 2) 1))))  

;; List-of-worm -> Number
;; extract position x of the first worm on the list
(check-expect (worm-pos-x (list (make-worm WORMHEAD (make-posn 60 50))
                                (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50))))
              60)

(define (worm-pos-x low)
  (posn-x (worm-pos (first low))))

;; List-of-worm -> Number
;; extract position y of the first worm on the list
(check-expect (worm-pos-y (list (make-worm WORMHEAD (make-posn 60 100))
                                (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50))))
              100)

(define (worm-pos-y low)
  (posn-y (worm-pos (first low))))

;; List-of-worm Direction -> List-of-worm
;; move the worm in appropriate direction and make it longer
(check-expect (longer-worm (list (make-worm WORMHEAD (make-posn 30 50))
                                 (make-worm WORMTAIL (make-posn 30 (- 50 D))))
                           (make-direction 0 1))
              (list (make-worm WORMHEAD (make-posn 30 (+ 50 (* D 1))))
                    (make-worm WORMTAIL (make-posn 30 50))
                    (make-worm WORMTAIL (make-posn 30 (- 50 D)))))
              
(define (longer-worm low d)
  (append (list (move-head low d)
                (make-worm WORMTAIL (make-posn (posn-x (worm-pos (first low))) (posn-y (worm-pos (first low))))))
          (rest low)))
 

;; ;; List-of-worm Direction -> List-of-worm
;; move the worm in appropriate direction
(check-expect (move-worm (list (make-worm WORMHEAD (make-posn 30 50))) (make-direction 0 1))
              (list (make-worm WORMHEAD (make-posn 30 (+ 50 (* 1 D))))))
(check-expect (move-worm (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                    (make-direction 1 0))
              (list (make-worm WORMHEAD (make-posn (+ 30 D) 50))
                    (make-worm WORMTAIL (make-posn 30 50))
                    (make-worm WORMTAIL (make-posn (- 30 D) 50))))
                               
(define (move-worm low d)
  (cons (move-head low d)
        (move-tail low)))      

;; List-of-worm Direction -> Worm
;; move the head of the worm
(check-expect (move-head (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                         (make-direction 1 0))
              (make-worm WORMHEAD (make-posn (+ 30 (* D 1)) 50)))

(define (move-head low d)
  (make-worm WORMHEAD (make-posn (+ (posn-x (worm-pos (first low))) (* D (direction-dx d)))
                                 (+ (posn-y (worm-pos (first low))) (* D (direction-dy d))))))
;; List-of-worm -> List-of-worm
;; move the tail of the worm
(check-expect (move-tail (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50))))
              (list (make-worm WORMTAIL (make-posn 30 50))
                    (make-worm WORMTAIL (make-posn (- 30 D) 50))))

(define (move-tail low)
  (cond [(empty? (rest low)) empty]
        [else
         (cons (make-worm WORMTAIL (make-posn (posn-x (worm-pos (first low))) (posn-y (worm-pos (first low)))))
               (move-tail (rest low)))]))

;;================FOODCREATE======================


;; Posn -> Posn
;; generate new position of food
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(check-random (food-create (make-posn 30 50)) (make-posn (+ (* (quotient (random MAX-W) D) D) (/ D 2))
                                                         (+ (* (quotient (random MAX-H) D) D) (/ D 2))))
 
(define (food-create p)
  (food-check-create p (make-posn (+ (* (quotient (random MAX-W) D) D) (/ D 2))
                                  (+ (* (quotient (random MAX-H) D) D) (/ D 2)))))

;; Posn Posn -> Posn
;; generate recursion

(define (food-check-create p candidate)
  (if (equal? p candidate)
      (food-create p)
      candidate))

;; Posn -> Boolean
;; use for testing only

(define (not=-1-1? p)
  (not (and (= (posn-x p) 1)
            (= (posn-y p) 1))))


;;====================================RENDER========================================

;; Game -> Image
;; render worm and food on appropriate positions 
(check-expect (render (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                 (make-posn 100 100)
                                 T->B))
              (place-images (list WORMHEAD
                                  WORMTAIL
                                  FOOD)
                            (list (make-posn 30 50)
                                  (make-posn 30 (+ 50 D))
                                  (make-posn 100 100))
                            BACKG))
                      
(define (render g)
  (place-images (list-images g)
                (list-positions g)
                BACKG))

;; Game -> List-of-image
;; make a list images of the worm parts and food in appropriate order
(check-expect (list-images (make-game (list (make-worm WORMHEAD (make-posn 30 50)))
                                      (make-posn 100 100)
                                      T->B))
              (list WORMHEAD FOOD))
(check-expect (list-images (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                            (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                      (make-posn 100 100)
                                      T->B))
              (list WORMHEAD WORMTAIL FOOD))

(define (list-images g)
  (append (list-worm (game-wrm g))
          (if (posn? (game-food g))
              (list FOOD)
              empty)))       

;; List-of-worm -> List-of-images
;; make a list of images of the worm parts
(check-expect (list-worm (list (make-worm WORMHEAD (make-posn 30 60))
                               (make-worm WORMTAIL (make-posn 30 (- 60 D)))))
              (list WORMHEAD WORMTAIL))

(define (list-worm low)
  (cond [(empty? low) empty]
        [else
         (cons (worm-body (first low))
               (list-worm (rest low)))]))  

;; Game -> List-of-Posn
;; make a list of positions of the worm parts and food in appropriate order
(check-expect (list-positions (make-game (list (make-worm WORMHEAD (make-posn 30 50)))
                                         (make-posn 100 100)
                                         T->B))
              (list (make-posn 30 50)
                    (make-posn 100 100)))
(check-expect (list-positions (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                               (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                         (make-posn 100 100)
                                         T->B))
              (list (make-posn 30 50)
                    (make-posn 30 (+ 50 D))
                    (make-posn 100 100)))

(define (list-positions g)
  (append (list-posn-worm (game-wrm g))
          (if (posn? (game-food g))
              (list (game-food g))
              empty)))

;; List-of-worm -> List-of-Posn
;; make a list of the positions of the worm parts
(check-expect (list-posn-worm (list (make-worm WORMHEAD (make-posn 30 60))
                                    (make-worm WORMTAIL (make-posn 30 (- 60 D)))))
              (list (make-posn 30 60)
                    (make-posn 30 (- 60 D))))

(define (list-posn-worm low)
  (cond [(empty? low) empty]
        [else
         (cons (worm-pos (first low))
               (list-posn-worm (rest low)))]))

;;=======================================ACTION================================== 

;; Game KeyEvent -> Game
;; generate new game state if the proper keys are pressed
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                 (make-posn 100 100)
                                 T->B)
                      "right")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                         (make-posn 100 100)
                         L->R))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                 (make-posn 100 100)
                                 T->B)
                      "left")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                         (make-posn 100 100)
                         R->L))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                 (make-posn 100 100)
                                 T->B)
                      "up")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                         (make-posn 100 100)
                         T->B))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                       (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                                 (make-posn 100 100)
                                 L->R)
                      "up")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                         (make-posn 100 100)
                         B->T))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                       (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                                 (make-posn 100 100)
                                 L->R)
                      "down")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                         (make-posn 100 100)
                         T->B))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                       (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                       (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                                 (make-posn 100 100)
                                 L->R)
                      "c")
              (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                               (make-worm WORMTAIL (make-posn (- 30 D) 50))
                               (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                         (make-posn 100 100)
                         L->R))
(check-expect (action (make-game (list (make-worm WORMHEAD (make-posn 60 60)))
                                 (make-posn 100 100)
                                 (make-direction 0 0))
                      "left")
              (make-game (list (make-worm WORMHEAD (make-posn 60 60)))
                                 (make-posn 100 100)
                                 R->L))
              
(define (action g ke)
  (cond [(or (posn=? (game-dir g) L->R)
             (posn=? (game-dir g) R->L))
         (cond [(key=? ke "up")   (make-game (game-wrm g) (game-food g) B->T)]
               [(key=? ke "down") (make-game (game-wrm g) (game-food g) T->B)]
               [else g])]
        [(or (posn=? (game-dir g) T->B)
             (posn=? (game-dir g) B->T))
         (cond [(key=? ke "right") (make-game (game-wrm g) (game-food g) L->R)]
               [(key=? ke "left")  (make-game (game-wrm g) (game-food g) R->L)]
               [else g])]
        [(posn=? (game-dir g) (make-direction 0 0))
         (cond [(key=? ke "up")   (make-game (game-wrm g) (game-food g) B->T)]
               [(key=? ke "down") (make-game (game-wrm g) (game-food g) T->B)]
               [(key=? ke "right") (make-game (game-wrm g) (game-food g) L->R)]
               [(key=? ke "left")  (make-game (game-wrm g) (game-food g) R->L)])]
        [else g]))

;;==============================ENDGAME===============================================
;; Game -> Boolean
;; give true if worm hit wall or hit itself

(define (hit? g)
  (or (hit-wall? g) (hit-itself? (game-wrm g) (game-dir g))))

;; Game -> Boolean
;; give true if worm hit wall 
(check-expect (hit-wall? (make-game (list (make-worm WORMHEAD (make-posn 30 50))
                                          (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                    (make-posn 100 100)
                                    T->B))
              false)
(check-expect (hit-wall? (make-game (list (make-worm WORMHEAD (make-posn 30 (- HEIGHT (- (/ D 2) 3))))
                                          (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                    (make-posn 100 100)
                                    T->B))
              true)
(check-expect (hit-wall? (make-game (list (make-worm WORMHEAD (make-posn 30 (- (/ D 2) 3)))
                                          (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                    (make-posn 100 100)
                                    B->T))
              true)
(check-expect (hit-wall? (make-game (list (make-worm WORMHEAD (make-posn (- WIDTH (- (/ D 2) 1)) 50))
                                          (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                    (make-posn 100 100)
                                    L->R))
              true)
(check-expect (hit-wall? (make-game (list (make-worm WORMHEAD (make-posn (- (/ D 2) 2) 50))
                                          (make-worm WORMTAIL (make-posn 30 (+ 50 D))))
                                    (make-posn 100 100)
                                    R->L))
              true)

(define (hit-wall? g)
  (cond [(posn=? (game-dir g) L->R) (>= (worm-pos-x (game-wrm g)) (- WIDTH (- (/ D 2) 1)))]
        [(posn=? (game-dir g) R->L) (<= (worm-pos-x (game-wrm g)) (- (/ D 2) 1))]
        [(posn=? (game-dir g) T->B) (>= (worm-pos-y (game-wrm g)) (- HEIGHT (- (/ D 2) 1)))]
        [(posn=? (game-dir g) B->T) (<= (worm-pos-y (game-wrm g)) (- (/ D 2) 1))]
        [else false]))

;; List-of-worm Direction -> Boolean
;; give true if worm hit itself
(check-expect (hit-itself? (list (make-worm WORMHEAD (make-posn 50 50))
                                 (make-worm WORMTAIL (make-posn (+ 50 D) 50))
                                 (make-worm WORMTAIL (make-posn (+ 50 D) (- 50 D)))
                                 (make-worm WORMTAIL (make-posn 50 (- 50 D))))                                   
                           B->T)
              true)
(check-expect (hit-itself? (list (make-worm WORMHEAD (make-posn 30 50))
                                 (make-worm WORMTAIL (make-posn (- 30 D) 50))
                                 (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))                              
                               L->R)
              false)

(define (hit-itself? low d)
  (cond [(posn=? d B->T) (eat1? (first low) (rest low) 0 0 -1 0)]
        [(posn=? d T->B) (eat1? (first low) (rest low) 0 0 1 0)]
        [(posn=? d L->R) (eat1? (first low) (rest low) 1 0 0 0)]
        [(posn=? d R->L) (eat1? (first low) (rest low) -1 0 0 0)]
        [else false]))

;; Worm List-of-worm -> Boolean
;; give true if one of the worms on the list is in a position that the (next w) would have its position
(check-expect (eat1? (make-worm WORMHEAD (make-posn 50 50))
                     (list (make-worm WORMTAIL (make-posn (+ 50 D) 50))
                           (make-worm WORMTAIL (make-posn (+ 50 D) (- 50 D)))
                           (make-worm WORMTAIL (make-posn 50 (- 50 D))))
                     0 0 -1 0)
              true)
(check-expect (eat1? (make-worm WORMHEAD (make-posn 30 50))
                     (list (make-worm WORMTAIL (make-posn (- 30 D) 50))
                           (make-worm WORMTAIL (make-posn (- 30 (* 2 D)) 50)))
                     0 0 -1 0)
              false)
(check-expect (eat1? (make-worm WORMHEAD (make-posn 30 30))
                     (list (make-worm WORMTAIL (make-posn (+ 30 D) 30))
                           (make-worm WORMTAIL (make-posn (+ 30 D) (+ 30 D)))
                           (make-worm WORMTAIL (make-posn 30 (+ 30 D))))
                     0 0 1 0)
              true)

(define (eat1? w low x1 x2 y1 y2)
  (cond [(empty? low) false]
        [else (if (and (= (+ (posn-x (worm-pos w)) (* D x1)) (+ (posn-x (worm-pos (first low))) (* D x2)))
                       (= (+ (posn-y (worm-pos w)) (* D y1)) (+ (posn-y (worm-pos (first low))) (* D y2))))
                  true
                  (eat1? w (rest low) x1 x2 y1 y2))]))
        
        

;; Direction Direction -> Boolean
;; give true if the two directions coincide
(check-expect (posn=? (make-direction 0 1) (make-direction 1 0)) false)
(check-expect (posn=? (make-direction 0 1) (make-direction 0 1)) true)

(define (posn=? a b)
  (and (= (direction-dx a) (direction-dx b))
       (= (direction-dy a) (direction-dy b))))

;; Game -> Image
;; render last image of the game-state and appropriate "game-over" message
(check-expect (last-image (make-game (list (make-worm WORMHEAD (make-posn (- WIDTH (- (/ D 2) 1)) 50))
                                           (make-worm WORMTAIL (make-posn (- (- WIDTH (- (/ D 2) 1)) D) 50))
                                           (make-worm WORMTAIL (make-posn (- (- WIDTH (- (/ D 2) 1)) (* 2 D)) 50)))
                                     (make-posn 100 100)
                                     L->R))
              (overlay/align "middle" "bottom"
                             (beside (text WALL-MESSAGE 10 "black")
                                     (text (number->string 2) 10 "black"))
                             (render (make-game (list (make-worm WORMHEAD (make-posn (- WIDTH (- (/ D 2) 1)) 50))
                                            (make-worm WORMTAIL (make-posn (- (- WIDTH (- (/ D 2) 1)) D) 50))
                                            (make-worm WORMTAIL (make-posn (- (- WIDTH (- (/ D 2) 1)) (* 2 D)) 50)))
                                      (make-posn 100 100)
                                      L->R))))
(check-expect (last-image (make-game (list (make-worm WORMHEAD (make-posn 50 50))
                                           (make-worm WORMTAIL (make-posn (+ 50 D) 50))
                                           (make-worm WORMTAIL (make-posn (+ 50 D) (- 50 D)))
                                           (make-worm WORMTAIL (make-posn 50 (- 50 D))))
                                     (make-posn 100 100)
                                     B->T))
              (overlay/align "middle" "bottom"
                             (beside (text EAT-MESSAGE 10 "black")
                                     (text (number->string 3) 10 "black"))
                             (render (make-game (list (make-worm WORMHEAD (make-posn 50 50))
                                                      (make-worm WORMTAIL (make-posn (+ 50 D) 50))
                                                      (make-worm WORMTAIL (make-posn (+ 50 D) (- 50 D)))
                                                      (make-worm WORMTAIL (make-posn 50 (- 50 D))))
                                                (make-posn 100 100)
                                                B->T)))) 

(define (last-image g)
  (cond [(hit-wall? g) (overlay/align "middle" "bottom"
                                      (beside (text WALL-MESSAGE 10 "black")
                                              (text (number->string (- (length (game-wrm g)) 1)) 10 "black"))
                                      (render g))]
        [(hit-itself? (game-wrm g) (game-dir g)) (overlay/align "middle" "bottom"
                                      (beside (text EAT-MESSAGE 10 "black")
                                              (text (number->string (- (length (game-wrm g)) 1)) 10 "black"))
                                      (render g))]))

;; =======================START====================================================

(define G (make-game (list (make-worm WORMHEAD (make-posn (+ (* (quotient (random MAX-W) D) D) (/ D 2))
                                                          (+ (* (quotient (random MAX-H) D) D) (/ D 2)))))
                     (food-create (make-posn (+ (* (quotient (random MAX-W) D) D) (/ D 2))
                                             (+ (* (quotient (random MAX-H) D) D) (/ D 2))))
                     (make-direction 0 0)))
(main G)