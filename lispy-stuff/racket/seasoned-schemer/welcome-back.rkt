#lang racket
(require racket/trace)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eleventh Commandment                                              ;;
;; Use additional arguments when a function needs to know what other ;;
;; arguments to the function have been so far.                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define two-in-a-row?
;;   (λ (lat)
;;     (cond
;;       ((or (null? (cdr lat)) (null? lat)) #f)
;;       ((eq? (car lat) (cadr lat)) #t)
;;       (else (two-in-a-row? (cdr lat))))))

(define two-in-a-row-b?
  (λ (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceding (car lat))
                (two-in-a-row-b? (car lat) (cdr lat)))))))
(define two-in-a-row?
  (λ (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

;; (two-in-a-row? '(apple banana banana pineapple))
;; (two-in-a-row? '(apple banana pineapple))

;; sonssf -> sum of numbers seen so far
(define sum-of-prefixes-b
  (λ (sonssf tup)
    (cond
      ((null? tup) (quote ()))
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))))))
(define sum-of-prefixes
  (λ (tup)
    (cond
      ((null? tup) (quote ()))
      (else (cons (car tup)
                  (sum-of-prefixes-b (car tup) (cdr tup)))))))
;; (sum-of-prefixes '(1 1 1 1 1))

(define pick
  (λ (i lat)
    (cond
      ((eq? i 1) (car lat))
      (else (pick (sub1 i)
                  (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

; Examples of scramble
(scramble '(1 1 1 3 4 2 1 1 9 2))       ; '(1 1 1 1 1 4 1 1 1 9)
(scramble '(1 2 3 4 5 6 7 8 9))         ; '(1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))    ; '(1 1 1 1 1 1 1 1 2 8 2)
