#lang racket
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat)
                          (rember a (cdr lat)))))))))
