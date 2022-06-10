;; ;; ;; ;; ;; ;; ;; 1.1 ;; ;; ;; ;; ;;
;; 10
;; >10
;; (+ 5 3 4)
;; >12
;; (- 9 1)
;; >8
;; (/ 6 2)
;; >3
;; (+ (* 2 4) (- 4 6))
;; >16
;; (define a 3)
;; >a
;; (define b (+ a 1))
;; >b
;; (+ a b (* a b))
;; > a is undefined 
;; (= a b)
;; > a is undefined
;; (if (and (> b a) (< b (* a b)))
;;     b a)
;; > undefined
;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
;; >
;; (+ 2 (if (> b a) b a))
;; >
;; (* (cond ((> a b) a) ((< a b) b)
;;          (else -1)) (+ a 1))
;; >

;; ;; ;; ;; ;; ;; ;; 1.2 ;; ;; ;; ;; ;;
(/ (+ 5 4 (- 2 (- 3 (+ 6 54)))) (* 3 (- 6 2) (- 2 7)))


;; ;; ;; ;; ;; ;; ;; 1.3 ;; ;; ;; ;; ;;
(define sum-of-squares
  (lambda (a b c)
    (cond [(and (> a b) (> a c) (> c b)) (+ (* a a) (* c c))]
          [(and (> b a) (> b c) (> c a)) (+ (* b b) (* c c))]
          [(and (> c a) (> c b) (> a b)) (+ (* c c) (* a a))])))

