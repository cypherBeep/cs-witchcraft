;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define in 4)
(define (area in) (* (image-width in) (image-height in)))
(define (inc in) (+ in 1))
(define (expr in) (cond
                    [(string? in) (string-length in)]
                    [(image? in) (area in)]
                    [(integer? in) (inc in)]
                    [(false? in) 20]
                    [else 10]))
(expr "so dana")

                  