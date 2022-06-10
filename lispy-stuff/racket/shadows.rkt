#lang racket
(require racket/trace)

(define atom?
  (lambda (x)
    (and (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 6 : Shadows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) 'o+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'ox)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'o^)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      (else #f))))
;; (numbered? 4)
;; (numbered? '(4 o+ 3))

(define value
  (λ (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (cadr aexp) 'o+)
       (+ (value (car aexp))
          (value (caddr aexp))))
      ((eq? (cadr aexp) 'o*)
       (* (value (car aexp))
          (value (caddr aexp))))
      ((eq? (cadr aexp) 'o^)
       (expt (value (car aexp))
             (value (caddr aexp))))
      (else #f))))

;; (value '(1 o+ (3 o^ 4)))

(define member?
  (λ (item ls)
    (if (null? ls) #f
        (if (equal? (car ls) item) #t
            (member item (cdr ls))))))

(define set?
  (λ (ls)
    (cond
      ((null? ls) #t)
      ((member? (car ls) (cdr ls)) #f)
      (else (set? (cdr ls))))))
;; (set? '(apple plum banana apple watermelon))
;; (set? '(apple banana watermelon))

(define makeset
  (λ (ls)
    (cond
      ((or (set? ls) (null? ls)) ls)
      ((member? (car ls) (cdr ls)) (makeset (cdr ls)))
      (else (cons (car ls)
                  (makeset (cdr ls)))))))
;; (makeset '(apple banana apple pineapple grapes oranges grapes))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (multirember (a (cdr lat))))))))
;; (multirember 'apple '(apple pie banana pineapple apple))

(define subset?
  (λ (s1 s2)
    (cond
      ((null? s1) #t)
      (else (cond
              ((member? (car s1) s2) (subset? (cdr s1) s2))
              (else #f))))))
;; (subset? '(4 apple pie) '(banana apple potato 4 pie))
;; (subset? '(5 apple pie) '(banana apple potato 4 pie))

(define eqset?
  (λ (s1 s2)
    (and (subset? s1 s2)
         (subset? s2 s1))))
;; (eqset? '(6 chicken with large wings) '(6 large chicken with wings))

(define first
  (λ (ls)
    ((car ls))))

(define second
  (λ (ls)
    ((car (cdr ls)))))

(define build
  (λ (s1 s2)
    (cons s1
          (cons s2 (quote ())))))

(define firsts
  (λ (ls)
    (cond
      ((null? ls) (quote ()))
      (else (cons
             (car (car ls))
             (firsts (cdr ls)))))))

(define fun?
  (λ (rel)
    (set? (firsts rel))))

(define revrel
  (λ (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (build
                    (second (car rel))
                    (first (car rel)))
                   (revrel (cdr rel)))))))
(revrel '((8 a) (4 b)))
