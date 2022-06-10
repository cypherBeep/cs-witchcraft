;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define FRAME (empty-scene 600 45))
(define (BAR cw)
  (rectangle cw 40 "solid" "red"))
(define (render cw)
  (place-image (BAR cw) 0 22 FRAME))
(define (tock cw)
  (cond
  [(not (equal? 0 cw)) (- cw 0.1)]
  [else (= cw 10)]))
(define (arrow cw key)
  (cond
    [(key=? "down" key) (- cw 1/5)]
    [(key=? "up" key) (+ cw 1/3)]
    [else cw]))
(define (main ws)
  (big-bang ws
     [on-tick tock]
     [to-draw render]
     [on-key arrow]))
