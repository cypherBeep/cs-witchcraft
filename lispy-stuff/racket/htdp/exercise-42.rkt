;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-42) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define WHEEL-RADIUS 9)
(define WHEEL-DISTANCE (* 3 WHEEL-RADIUS))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
(define WHEEL (circle WHEEL-RADIUS "solid" "blue"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR (above (rectangle (* WHEEL-RADIUS  2) (/ WHEEL-DISTANCE 2) "solid" "blue") BOTH-WHEELS))
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND (place-image TREE 340 20 (empty-scene 500 38)))
; State -> State
; moves the car 3 pixels ahead every clock pulse.
(define (tock cw)
  (+ cw 3))
; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image
(define (render cw)
  (place-image CAR cw 20 BACKGROUND))                    
; State -> State
; Launces the program
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     )) 