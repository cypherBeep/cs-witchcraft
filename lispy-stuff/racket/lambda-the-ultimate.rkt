#lang racket
(require racket/trace)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember-f
  (λ (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? a (car l)) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))
;; (rember-f eq? 'apple '(banana apple pineapple cherry apple))

;; ((λ (a)
;;    (λ (x)
;;      (eq? x a)) 'cat) 'hat)

(define myrember-f
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((myrember-f test?) a (cdr l))))))))
;; ((myrember-f equal?) 'apple '(apple banana pineapple))

(define insertleft
  (λ (new old l)
    (cons new
          (cons old
                (cdr l)))))
(define insertright
  (λ (new old l)
    (cons old
          (cons new
                (cdr l)))))

(define insert-g
  (λ (new old l f)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) old)
       (f new old l))
      (else (cons (car l)
                  (insert-g new old (cdr l) f))))))
;; (insert-g 'apple 'banana '(pineapple cherry banana pomogranate) insertleft)
;; (insert-g 'apple 'banana '(pineapple cherry banana pomogranate) insertright)

(define multirember&co
  (λ (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote ())))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (λ (newlat seen)
                                                       (col newlat (cons (car lat)
                                                                         seen)))))
      (else (multirember&co a (cdr lat) (λ (newlat seen)
                                          (col (cons (car lat)
                                                     newlat) seen)))))))
(define a-friend
  (λ (x y)
    (null? y)))

;; (multirember&co 'tuna '(tuna) list)
;; (multirember&co 'tuna '(and tuna) a-friend)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))
;; (multiinsertLR 'apple 'pineapple 'banana '(cherry pomogranate banana grapes pineapple watermelon))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat L R)
                                                   (col (cons new
                                                              (cons oldL newlat))
                                                        (add1 L)
                                                        R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat L R)
                                                   (col (cons oldR
                                                              (cons new newlat))
                                                        L
                                                        (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat L R)
                                                        (col (cons (car lat)
                                                                   newlat)
                                                             L
                                                             R)))))))
;; (multiinsertLR&co 'apple 'pineapple 'banana '(cherry pomogranate banana grapes pineapple watermelon) list)

(define evens-only*
  (λ (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((even? (car l)) (cons (car l)
                                                (evens-only* (cdr l))))

                         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))
;; (evens-only* '(1 2 3 (21 44 22 99) 8 9 6 4))

;; remove the odd
;; multiply the even
;; sum the removed odds
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

;; (evens-only*&co '(2 3 4 5 6) last-friend)

;; (((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (list)
;;      (cond
;;        ((null? list) 0)
;;        (else
;;         (add1 ((mk-length mk-length) (cdr list))))))))
;;  '(a b c d e f g h i j))

(define pick
  (λ (a lat)
    (cond
      ((eq? a 1) (car lat))
      (else (pick (sub1 a) (cdr lat))))))

(define looking (lambda (a lat)
                  (keep-looking a (pick 1 lat) lat)))
(define keep-looking (lambda (a sorn lat)
                       (cond
                         ((number? sorn)
                          (keep-looking a (pick sorn lat) lat))
                         (else (eq? sorn a)))))

(define C (lambda (n)
            (cond
              ((eq? n 1) 1)
              (else
               (cond
                 ((even? n) (C (+ n 2)))
                 (else (C (add1 (* 3 n)))))))))

(define A (lambda (n m)
            (cond
              ((zero? n) (add1 m)) ((zero? m) (A (sub1 n) 1)) (else (A (sub1 n)
                                                                       (A n (sub1 m)))))))
