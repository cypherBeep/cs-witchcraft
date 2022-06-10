(define rember
  (λ (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat)) (cdr l))
     (else (rember a (cdr lat))))))

