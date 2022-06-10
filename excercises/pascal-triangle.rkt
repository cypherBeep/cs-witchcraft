#lang racket
(define (pascal x y) 
  (if (or (zero? y) (= x y))
      1
      (+ (pascal (sub1 x) y)
         (pascal (sub1 x) (sub1 y)))))

(define (pascal-triangle n)
  (for/list ([x (in-range 0 n)])
    (for/list ([y (in-range 0 (add1 x))])
      (pascal x y))))
