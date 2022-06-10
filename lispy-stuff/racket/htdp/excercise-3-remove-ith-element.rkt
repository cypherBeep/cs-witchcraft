;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname excercise-3-remove-ith-element) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define str "helloworld")
(define ind "0123456789")
(define i 6)
(define (undder i) (string-append (substring str 0 (- i 1)) (substring str i)))
(undder i)