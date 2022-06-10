#lang racket
(define insertR
  (λ (new old lat)
  (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))