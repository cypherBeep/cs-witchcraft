#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define evens-only*&co
  (λ (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l)) (cond
                         ((even? (car l)) (evens-only*&co (cdr l) (λ (newl p s)
                                                                    (col (cons (car l) newl)
                                                                         (* (car l) p)
                                                                         s))))
                         (else (evens-only*&co (cdr l) (λ (newl p s)
                                                         (col newl
                                                              p
                                                              (+ (car l) s)))))))
      (else (evens-only*&co (car l) (λ (newl p s)
                                      (evens-only*&co (cdr l) (λ (dnewl dp ds)
                                                                (col (cons dnewl newl)
                                                                     (* dp p)
                                                                     (+ ds s))))))))))
(define last-friend
  (λ (newl p s)
    (cons s
          (cons newl p))))

(evens-only*&co '(2 3 4 5 6) last-friend)
