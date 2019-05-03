;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname door) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; =================
;; Constants:

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

(define SIZE 30)
(define COLOR "tomato")
(define BACKGROUND (rectangle (* SIZE 6) (* SIZE 1.5) "solid" "brown"))

;; =================
;; Data definitions:

;; DoorState is one of:
;; - LOCKED
;; - CLOSED
;; - OPEN


;; =================
;; Functions:

;; DoorState -> DoorState
;; start the world with (main LOCKED)

(define (main ds)
  (big-bang ds                        ; DoorState
            (on-tick   door-closer 3) ; DoorState -> DoorState
            (to-draw   door-render)   ; DoorState -> Image                      
            (on-key    door-action))) ; DoorState KeyEvent -> DoorState

;; DoorState -> DoorState
;; close the door if it is open
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

(define (door-closer ds)
  (if (string=? ds OPEN)
      CLOSED
      ds))

;; DoorState -> Image
;; render door state 
(check-expect (door-render LOCKED) (overlay (text LOCKED SIZE COLOR) BACKGROUND))
(check-expect (door-render CLOSED) (overlay (text CLOSED SIZE COLOR) BACKGROUND))
(check-expect (door-render OPEN) (overlay (text OPEN SIZE COLOR) BACKGROUND))
               
(define (door-render ds)
  (overlay (text ds SIZE COLOR)
           BACKGROUND))

;; Doorstate KeyEvent -> DoorState
;; change door state if proper keys are pressed
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action LOCKED " ") LOCKED)
(check-expect (door-action LOCKED "l") LOCKED)
(check-expect (door-action LOCKED "g") LOCKED)
(check-expect (door-action CLOSED "u") CLOSED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED "g") CLOSED)
(check-expect (door-action OPEN "u") OPEN)
(check-expect (door-action OPEN " ") OPEN)
(check-expect (door-action OPEN "l") OPEN)
(check-expect (door-action OPEN "g") OPEN)

(define (door-action ds ke)
  (cond [(and (string=? ds LOCKED) (string=? ke "u")) CLOSED]
        [(and (string=? ds CLOSED) (string=? ke " ")) OPEN]
        [(and (string=? ds CLOSED) (string=? ke "l")) LOCKED]
        [else ds]))