#lang racket
(require racket/trace)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define member?
  (λ (item ls)
    (if (null? ls) #f
        (if (equal? (car ls) item) #t
            (member item (cdr ls))))))

(define intersect
  (λ (s1 s2)
    (letrec ((I (λ (s)
                  (cond
                    ((null? s) (quote ()))
                    ((member? (car s) s2) (cons (car s)
                                                (I (cdr s))))
                    (else (I (cdr s)))))))
      (I s1))))

;; (intersect '(a b c d) '(d e f c))

(define intersectall
  (λ (lset)
    (letrec
        ((A (λ (lset)
                        (cond
                          ((null? (cdr lset)) (car lset))
                          (else (intersect (car lset)
                                           (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))
;; (intersectall '((a b c d) (d e f c a) ()))
