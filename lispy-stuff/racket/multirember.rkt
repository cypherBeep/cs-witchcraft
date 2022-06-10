#lang racket
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (multirember (a (cdr lat))))))))
