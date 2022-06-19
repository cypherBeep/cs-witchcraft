#lang racket
(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (list)
     (cond
       ((null? list) 0)
       (else
        (add1 ((mk-length mk-length) (cdr list))))))))
 '(a b c d e f g h i j))


(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; ((Y (lambda (length)
;;      (lambda (list)
;;        (cond
;;          ((null? list) 0)
;;          (else
;;           (add1 (length (cdr list))))))))
;;  '(a b c d e f g h i j))
