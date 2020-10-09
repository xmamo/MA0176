(#%require schemeunit)

(define (H f g)
  (define (h m n)
    (if (zero? n)
        (f m)
        (g m (h m (- n 1)))))
  h)

(define (succ m n) (+ n 1))
(define add (H (lambda (x) x) succ))
(define mul (H (lambda (x) 0) add))
(define pow (H (lambda (x) 1) mul))

(do ((a 0 (+ a 1)))
  ((> a 5))
  (do ((b 0 (+ b 1)))
    ((> b 5))
    (check-equal? (add a b) (+ a b))
    (check-equal? (mul a b) (* a b))
    (check-equal? (pow a b) (expt a b))))
