;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Fireworks-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Fireworks

;; =================
;; Constants:
(define WIDTH 500)
(define HEIGHT 700)
(define MTS (rectangle WIDTH HEIGHT "solid" "darkslategrey"))

(define ROCKET (above (isosceles-triangle 3 60 "solid" "brown")
                      (rectangle 3 6 "solid" "brown")))
(define STAR (radial-star 8 8 64 "solid" "lemonchiffon"))

(define DISTANCE 300)

(define SC-FACTOR 0.1)
(define MAX-SC 1.3)

(define VELOCITY 5)

;; =================
;; Data definitions:

(define-struct firework (x y d s))
;; Firework is (make-firework Natural[0, WIDTH] Natural[0, HEIGHT] Natural[0, DISTANCE] Integer(0, MAX-SC]
;; interp. (make-firework x y d s) is a firework with x and y coordinates
;;          d is the distance traveled by the ROCKET
;;          s is the scaleing of STAR

(define FW1 (make-firework 60 70 50 0.1))
(define FW2 (make-firework 50 90 100 0.6))
#;
(define (fn-for-firework fw)
  (... (firework-x fw)
       (firework-y fw)
       (firework-d fw)
       (firework-s fw)))

;; Template rules used:
;; - compound: 4 fields

;; ListOfFirework is one of:
;; - empty
;; - (cons Firework ListOfFirework)
;; interp. a list of fireworks

(define LOF0 empty)
(define LOF1 (cons FW1 empty))
(define LOF2 (cons FW2 LOF1))
#;
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (fn-for-firework (first lof))
              (fn-for-lof (rest lof)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound (cons Firework ListOfForework)
;; - reference: (first lor) is Firework
;; - self-reference: (rest lor) is ListOfFirework

;; =================
;; Functions:

;; ListOfFirework -> ListOfFirework
;; start the world with (main empty)

(define (main lof)
  (big-bang lof                       ; ListOfFirework
           (on-mouse  mouse-event)    ; ListOfFirework Integer Integer MouseEvent -> ListOfFirework
           (on-tick   next-position)  ; ListOfFirework -> ListOfFirework
           (to-draw   render)))       ; ListOfFirework -> Image

;; ListOfFirework Integer Integer MouseEvent-> ListOfFirework
;; produce a dot on the screen 
(check-expect (mouse-event empty 100 150 "button-down") (cons (make-firework 100 150 0 0.1) empty))
(check-expect (mouse-event empty 100 150 "drag") empty)
(check-expect (mouse-event LOF1 50 60 "button-down") (cons (make-firework 50 60 0 0.1) LOF1))

(define (mouse-event lor a b me) 
  (cond [(mouse=? me "button-down") (cons (make-firework a b 0 0.1) lor)]         
        [else lor]))

;; ListOfFirework -> ListOfFirework
;; produce the next position of d or next value of s depending on position
(check-expect (next-position empty) empty)
(check-expect (next-position (cons (make-firework 50 60 10 0.1) empty))
              (cons (make-firework 50 (- 60 VELOCITY) (+ 10 VELOCITY) 0.1) empty))
(check-expect (next-position (cons (make-firework 50 200 DISTANCE 0.1) empty))
              (cons (make-firework 50 200 DISTANCE (+ 0.1 SC-FACTOR)) empty))
(check-expect (next-position (cons (make-firework 50 60 10 0.1)
                                   (cons (make-firework 50 200 DISTANCE 0.1) empty)))
              (cons (make-firework 50 (- 60 VELOCITY) (+ 10 VELOCITY) 0.1)
                    (cons (make-firework 50 200 DISTANCE (+ 0.1 SC-FACTOR)) empty)))

;(define (next-position lof) lof)  ;stub

(define (next-position lof)
  (cond [(empty? lof) empty]
        [else
         (if (and (distance-reached? (first lof))
                  (scale-reached? (first lof)))
             (next-position (rest lof))
             (if (distance-reached? (first lof))
                 (cons (advance-scale (first lof)) (next-position (rest lof)))
                 (cons (advance-distance (first lof)) (next-position (rest lof)))))]))

;; Firework -> Boolean
;; produce true if d reached DISTANCE
(check-expect (distance-reached? (make-firework 60 80 50 0.1)) false)
(check-expect (distance-reached? (make-firework 60 80 DISTANCE 0.1)) true)

;(define (distance-reached? fw) false)  ;stub

(define (distance-reached? fw)
  (>= (firework-d fw) DISTANCE))

;; Firework -> Firework
;; advance s with SC-FACTOR
(check-expect (advance-scale (make-firework 90 80 DISTANCE 0.3))
              (make-firework 90 80 DISTANCE (+ 0.3 SC-FACTOR)))

;(define (advance-scale fw) fw)  ;stub

(define (advance-scale fw)
  (make-firework (firework-x fw) (firework-y fw) (firework-d fw) (+ (firework-s fw) SC-FACTOR)))

;; Firework -> Firework
;; advance y and d with VELOCITY
(check-expect (advance-distance (make-firework 60 90 3 0.1))
              (make-firework 60 (- 90 VELOCITY) (+ 3 VELOCITY) 0.1))

;(define (advance-distance fw) fw)  ;stub

(define (advance-distance fw)
  (make-firework (firework-x fw) (- (firework-y fw) VELOCITY) (+ (firework-d fw) VELOCITY) (firework-s fw)))


;; ListOfFirework -> Image
;; render ROCKET and STAR onto MTS
(check-expect (render empty) MTS)
(check-expect (render (cons (make-firework 80 90 60 0.1) empty))
              (place-image ROCKET
                           80 90
                           MTS))
(check-expect (render (cons (make-firework 80 90 60 0.1)
                            (cons (make-firework 100 80 90 0.1) empty)))
              (place-image ROCKET
                           80 90
                           (place-image ROCKET
                                        100 80
                                        MTS)))
(check-expect (render (cons (make-firework 80 90 60 0.1)
                            (cons (make-firework 100 80 DISTANCE 0.1) empty)))
              (place-image ROCKET
                           80 90
                           (place-image (scale 0.1 STAR)
                                        100 80
                                        MTS)))
(check-expect (render (cons (make-firework 80 90 DISTANCE 0.1) empty))
              (place-image (scale 0.1 STAR)
                           80 90
                           MTS))
(check-expect (render (cons (make-firework 80 90 DISTANCE 0.1)
                            (cons (make-firework 100 150 DISTANCE 1.2) empty)))
              (place-image (scale 0.1 STAR)
                           80 90
                           (place-image (scale 1.2 STAR)
                                        100 150
                                        MTS)))
(check-expect (render (cons (make-firework 80 90 DISTANCE 0.1)
                            (cons (make-firework 100 150 DISTANCE MAX-SC) empty)))
              (place-image (scale 0.1 STAR)
                           80 90
                           MTS))
(check-expect (render (cons (make-firework 80 90 50 0.1)
                            (cons (make-firework 100 150 DISTANCE 0.5) empty)))
              (place-image ROCKET
                           80 90
                           (place-image (scale 0.5 STAR)
                                        100 150
                                        MTS)))

;(define (render lof) MTS)  ;stub

(define (render lof)
  (cond [(empty? lof) MTS]
        [else
         (if (and (distance-reached? (first lof))
                  (scale-reached? (first lof)))
             (render (rest lof))
             (if (distance-reached? (first lof))
                 (place-image (scale (firework-s (first lof)) STAR)
                              (firework-x (first lof))
                              (firework-y (first lof))
                              (render (rest lof)))
                 (place-image ROCKET
                              (firework-x (first lof))
                              (firework-y (first lof))
                              (render (rest lof)))))]))

;; Firework -> Boolean
;; produce true if s reached MAX-SCALE
(check-expect (scale-reached? (make-firework 50 40 DISTANCE 0.5)) false)
(check-expect (scale-reached? (make-firework 90 100 DISTANCE MAX-SC)) true)

;(define (scale-reached? fw) false)  ;stub

(define (scale-reached? fw)
  (>= (firework-s fw) MAX-SC))