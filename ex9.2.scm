(#%require schemeunit)

(define (H f g)
  (define (h m n)
    (if (zero? n)
        (f m)
        (g m (h m (- n 1)))))
  h)

(let* ((succ (lambda (m n) (+ n 1)))
       (add (H (lambda (x) x) succ))
       (mul (H (lambda (x) 0) add))
       (pow (H (lambda (x) 1) mul)))
  (do ((a 0 (+ a 1)))
    ((> a 5))
    (do ((b 0 (+ b 1)))
      ((> b 5))
      (check-equal? (add a b) (+ a b))
      (check-equal? (mul a b) (* a b))
      (check-equal? (pow a b) (expt a b)))))