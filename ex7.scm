(#%require schemeunit)

(define (belong? x xs)
  (>= (position x xs) 0))

(define (position x xs)
  (define (go x xs i)
    (cond ((null? xs) -1)
          ((equal? (car xs) x) i)
          (else (go x (cdr xs) (+ i 1)))))
  (go x xs 0))

(define (sorted-list xs)
  (if (null? xs) xs (sorted-ins (car xs) (sorted-list (cdr xs)))))

(define (sorted-ins x xs)
  (cond ((or (null? xs) (< x (car xs))) (cons x xs))
        ((> x (car xs)) (cons (car xs) (sorted-ins x (cdr xs))))
        (else xs)))

(check-false (belong? 18 (list)))
(check-true (belong? 18 (list 5 7 10 18 23)))
(check-false (belong? 18 (list 5 7 10 12 23)))

(check-equal? (position 7 (list 7 8 24 35 41)) 0)
(check-equal? (position 35 (list 7 8 24 35 41)) 3)
(check-equal? (position 41 (list 7 8 24 35 41)) 4)

(check-equal? (sorted-list (list 35 8 41 24 7)) (list 7 8 24 35 41))

(check-equal? (sorted-ins 24 (list)) (list 24))
(check-equal? (sorted-ins 5 (list 7 8 24 35 41)) (list 5 7 8 24 35 41))
(check-equal? (sorted-ins 24 (list 7 8 24 35 41)) (list 7 8 24 35 41))
(check-equal? (sorted-ins 27 (list 7 8 24 35 41)) (list 7 8 24 27 35 41))
