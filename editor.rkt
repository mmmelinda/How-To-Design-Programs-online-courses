;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct time1 [hours minutes seconds])
;; Time1 is (make-time Natural Natural Natural)
;; interp. a point in time that consists of three numbers:
;;         hours minutes and seconds

;; Time1 -> Natural
;; produce a number of seconds that have passed since midnight
(check-expect (time->second (make-time1 1 30 0)) (+ (* 1 60 60) (* 30 60) 0))
(check-expect (time->second (make-time1 12 30 2)) 45002)

(define (time->second t)
  (+ (* (time1-hours t) 3600) (* (time1-minutes t) 60) (time1-seconds t)))


;;=====================================================================================

;; =================
;; Constants:
(define MTS (rectangle 200 20 "solid" "white"))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define SIZE 16)
(define COLOR "black")
(define CENTER (/ (image-height CURSOR) 2))

;; =================
;; Data definitions:

(define-struct editor [pre post])
;; Editor is (make-editor String String)
;; interp. describes an editor whose visible text is (string-append s t)
;; with the cursor displayed between s and t

;; =================
;; Functions:

;; Editor -> Editor
;; start the world with (main (make-editor "" ""))

(define (main e)
  (big-bang e            ; Editor
    (to-draw   render)   ; Editor -> Image
    (on-key    edit)     ; Editor KeyEvent -> Editor
    (check-with editor?)))    ; Editor -> Boolean

;; Editor -> Image
;; produce an image of Editor
(check-expect (render (make-editor "hello" "world"))
              (place-image CURSOR
                           (image-width (text "hello" SIZE COLOR)) CENTER
                           (overlay/align "left" "center"                             
                                          (text "helloworld" SIZE COLOR)
                                          MTS)))
                             

(define (render e)
  (place-image CURSOR
               (image-width (text (editor-pre e) SIZE COLOR)) CENTER
               (overlay/align "left" "center"
                              (text (attach e) SIZE COLOR)
                              MTS)))
;; Editor -> String
;; make one string drom pre and post words of e
(check-expect (attach (make-editor "this" "is")) "thisis")

(define (attach e) (string-append (editor-pre e) (editor-post e)))

;; Editor -> Editor
;; change current state of Editor, add or remove 1String
(check-expect (edit (make-editor "hello" "world") " ") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello" "world") "a") (make-editor "helloa" "world"))
(check-expect (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "" "world") "left") (make-editor "" "world"))
(check-expect (edit (make-editor "" "world") "right") (make-editor "w" "orld"))
(check-expect (edit (make-editor "hello" "") "left") (make-editor "hell" "o"))
(check-expect (edit (make-editor "hello" "") "right") (make-editor "hello" ""))
(check-expect (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world"))
(check-expect (edit (make-editor "" "world") "\b") (make-editor "" "world"))
(check-expect (edit (make-editor "h" "world") "\b") (make-editor "" "world"))
(check-expect (edit (make-editor "helloworld" "") "\b") (make-editor "helloworl" ""))
(check-expect (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\r") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "w") "up") (make-editor "hello" "w"))

(define (edit e ke)
  (cond [(string=? ke " ") (make-editor (string-append (editor-pre e) " ") (editor-post e))]
        [(string=? ke "left") (move-left e)]
        [(string=? ke "right") (move-right e)]
        [(string=? ke "\b") (make-editor (remove-last (editor-pre e)) (editor-post e))]
        [(or (string=? ke "\t") (string=? ke "\r")) e]
        [(= (string-length ke) 1) (make-editor (string-append (editor-pre e) ke) (editor-post e))]        
        [else e]
        ))

;; String -> String
;; remove last character of string if the string is not empty
(check-expect (remove-last "") "")
(check-expect (remove-last "o") "")
(check-expect (remove-last "add") "ad")
(check-expect (remove-last "vinegar") "vinega")

(define (remove-last s)
  (cond [(or (string=? s "") (= (string-length s) 1)) ""]
        [else (substring s 0 (- (string-length s) 1))]))

;; Editor -> Editor
;; moves last character of the first string to the beginning of the second string
(check-expect (move-left (make-editor "hello" "world")) (make-editor "hell" "oworld"))
(check-expect (move-left (make-editor "h" "elloworld")) (make-editor "" "helloworld"))
(check-expect (move-left (make-editor "" "hello")) (make-editor "" "hello"))

(define (move-left e)
  (cond [(string=? (editor-pre e) "") e]       
        [else (make-editor (remove-last (editor-pre e))
                           (string-append (last-char (editor-pre e)) (editor-post e)))])) 

;; Editor -> Editor
;; moves first character of the second string to the end of the forst string
(check-expect (move-right (make-editor "hello" "world")) (make-editor "hellow" "orld"))
(check-expect (move-right (make-editor "hell" "o")) (make-editor "hello" ""))
(check-expect (move-right (make-editor "hello" "")) (make-editor "hello" ""))
(check-expect (move-right (make-editor "" "hello")) (make-editor "h" "ello"))
                           
(define (move-right e)
  (cond [(string=? (editor-post e) "") e]       
        [else (make-editor (string-append (editor-pre e) (first-char (editor-post e)))
                           (substring (editor-post e) 1))]))

;; String -> String
;; produce last character of the String
(check-expect (last-char "hello") "o")
(check-expect (last-char "s") "s")
(check-expect (last-char "") "")

(define (last-char s)
  (if (string=? s "")
      s
      (substring s (- (string-length s) 1))))

;; String -> String
;; produce first character of String
(check-expect (first-char "hello") "h")
(check-expect (first-char "h") "h")
(check-expect (first-char "") "")

(define (first-char s)
  (if (string=? s "")
      s
      (substring s 0 1)))