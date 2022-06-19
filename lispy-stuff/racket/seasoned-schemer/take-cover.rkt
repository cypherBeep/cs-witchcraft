#lang racket
(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twelfth Commandment                                                       ;;
;; Use letrec to remove those arguments that don't change during recursion. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; (define multirember
;;   (lambda (a lat)
;;     ((Y (lambda (mr)
;;           (lambda (lat)
;;             (cond
;;               ((null? lat) '())
;;               ((eq? a (car lat)) (mr (cdr lat)))
;;               (else
;;                (cons (car lat) (mr (cdr lat))))))))
;;      lat)))

(define multirember-letrec
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else
                  (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))
;; (multirember-letrec 'tuna '(salad tuna and vegitables tuna))

(define multirember-f
  (λ (test?)
    (letrec
        ((m-f
          (λ (a lat)
            (cond
              ((null? lat) (quote ()))
              ((test? (car lat) a) (m-f a (cdr lat)))
              (else (cons (car lat)
                          (m-f a (cdr lat))))))))
      m-f)))
;; ((multirember-f eq?) 'apple '(pineapple apple banana kiwi apple))

(define member?
  (λ (a lat)
    ((letrec
         ((yes? (λ (l)
                  (cond
                    ((null? l) #f)
                    ((eq? (car l) a) #t)
                    (else (yes? (cdr l))))))) yes?)
     lat)))
;; (member? 'tuna '(apple banana tuna pineapple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thirteenth Commnadment                          ;;
;; Use (letrec ...) to hide and protect functions. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
