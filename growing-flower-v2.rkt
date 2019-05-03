;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname growing-flower-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Growing flowers - version 2 

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 600)
(define MTS (overlay (rectangle WIDTH HEIGHT "solid" "mediumgoldenrod")
                     (empty-scene WIDTH HEIGHT)))
(define FLOWER (overlay
                (rotate 30 (radial-star 6 3 15 "solid" "orange"))
                (radial-star 6 3 15 "solid" "gold")
                (pulled-regular-polygon 40 6 1 70 "solid" "lightsalmon")
                (pulled-regular-polygon 60 6 1 100 "solid" "tomato")
                (rotate 30 (pulled-regular-polygon 40 6 1 70 "solid" "orangered"))))
(define SC-FACTOR 0.015)
(define MAX-SC 2)

;; =================
;; Data definitions:

(define-struct flower (x y r dr sc))
;; Flower is (make-flower Natural[0, WIDTH] Natural[0, HEIGHT] Natural[0, 360) Integer Number)
;; interp. (make-flower (x y r) is a flower at x and y coordinates, rotated with angle r
;;          dr repr. angular velocity
;;          sc repr. the scale of the flower

(define F1 (make-flower (/ WIDTH 2) (/ HEIGHT 2) 0 3 0.1))
(define F2 (make-flower 200 300 27 5 1.2))
#;
(define (fn-for-flower f)
  (... (flower-x f)    ;Natural[0, WIDTH]   
       (flower-y f)    ;Natural[0, HEIGHT]
       (flower-r f)    ;Natural[0, 360)
       (flower-dr f)   ;Integer
       (flower-sc f))) ;Number

;; Template rules used:
;; - compound: 5 fields

;; ListOfFlower is one of:
;; - empty
;; - (cons Flower ListOfFlower)
;; interp. a list of flowers

(define LOF0 empty)
(define LOF1 (cons F1 (cons F2 empty)))
#;
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (fn-for-flower (first lof))
              (fn-for-lof (rest lof)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Flower ListOfFlower)
;; - reference: (first lof) is Flower
;; - self-reference: (rest lof) is ListOfFlower

;; =================
;; Functions:

;; ListOfFlower -> ListOfFlower
;; start the world with (main empty)
;; 
(define (main lof)
  (big-bang lof                        ; ListOfFlower
            (on-tick   next-flower)    ; ListOfFlower -> ListOfFlower
            (to-draw   render-flower)  ; ListOfFlower -> Image
            (on-mouse  mouse-event)))  ; ListOfFlower Integer Integer MouseEvent -> ListOfFlower

;; ListOfFlower -> ListOfFlower
;; increase the scale of the flower and the angle of rotation of the flower
(check-expect (next-flower empty) empty)
(check-expect (next-flower (cons (make-flower 100 150 30 3 0.3) empty))
              (cons (make-flower 100 150 (+ 30 3) 3 (+ 0.3 SC-FACTOR)) empty))
(check-expect (next-flower (cons (make-flower 100 150 30 3 0.3)
                                 (cons (make-flower 60 50 0 3 0.1) empty)))
              (cons (make-flower 100 150 (+ 30 3) 3 (+ 0.3 SC-FACTOR))
                    (cons (make-flower 60 50 (+ 0 3) 3 (+ 0.1 SC-FACTOR)) empty)))

;(define (next-flower lof) lof)  ;stub

(define (next-flower lof)
  (cond [(empty? lof) empty]
        [else
         (cons (flower-maker (first lof))
               (next-flower (rest lof)))]))

;; Flower -> Flower
;; make a Flower with the increased angle and scale
(check-expect (flower-maker (make-flower 96 69 0 3 1.5)) (make-flower 96 69 (+ 0 3) 3 (+ 1.5 SC-FACTOR)))

;(define (flower-maker f) f)  ;stub

(define (flower-maker f)
   (make-flower (flower-x f) (flower-y f) (next-angle f) (flower-dr f) (max-scale f)))

;; Flower -> Integer[0, 360)
;; increase the rotation angle of the flower with the rotation velocity
(check-expect (next-angle (make-flower 99 144 25 3 1)) (+ 25 3))
(check-expect (next-angle (make-flower 66 92 358 3 1.5)) (- 3 (modulo 360 358)))

;(define (next-angle f) 0)  ;stub

(define (next-angle f)
  (if (>= (+ (flower-r f) (flower-dr f)) 360)
      (- (flower-dr f) (modulo 360 (flower-r f)))
      (+ (flower-r f)(flower-dr f))))

;; Flower -> Number
;; increase scaleing factor until it reaches MAX-SC
(check-expect (max-scale (make-flower 95 59 33 3 0.5)) (+ 0.5 SC-FACTOR))
(check-expect (max-scale (make-flower 150 160 54 3 MAX-SC)) MAX-SC)

;(define (max-scale f) 0)  ;stub

(define (max-scale f)
  (if (>= (+ (flower-sc f) SC-FACTOR) MAX-SC)
      MAX-SC
      (+ (flower-sc f) SC-FACTOR)))

;; ListOfFlower -> Image
;; place flower image on MTS with the appropriate angle and scale
(check-expect (render-flower empty) MTS)
(check-expect (render-flower (cons (make-flower 56 65 33 3 2.1) empty))
              (place-image (scale 2.1 (rotate 33 FLOWER))
                           56 65
                           MTS))
(check-expect (render-flower (cons (make-flower 100 150 30 3 0.3)
                                 (cons (make-flower 60 50 0 3 0.1) empty)))
              (place-image (scale 0.3 (rotate 30 FLOWER))
                           100 150
                           (place-image (scale 0.1 (rotate 0 FLOWER))
                            60 50
                            MTS)))

;(define (render-flower f) MTS)

(define (render-flower lof)
  (cond [(empty? lof) MTS]
        [else
         (place-image (scale (flower-sc (first lof)) (rotate (flower-r (first lof)) FLOWER))
                      (flower-x (first lof))
                      (flower-y (first lof))
                      (render-flower (rest lof)))]))

;; Flower Integer Integer MouseEvent -> Flower
;; start another growing flower on the position where it was clicked rotating the flower the other way than before
(check-expect (mouse-event empty 100 150 "button-down")
              (cons (make-flower 100 150 0 3 0.1) empty))
(check-expect (mouse-event (cons (make-flower 135 154 300 3 1.2) empty) 189 96 "button-down")
              (cons (make-flower 189 96 0 -3 0.1)
               (cons (make-flower 135 154 300 3 1.2) empty)))

;(define (mouse-event lof a b me) lof) ;stub

(define (mouse-event lof a b me)
  (cond [(and (mouse=? me "button-down")
              (empty? lof))
         (cons (make-flower a b 0 3 0.1) empty)]
        [(mouse=? me "button-down")
         (cons (make-flower a b 0 (- (flower-dr (first lof))) 0.1) lof)]
        [else lof]))