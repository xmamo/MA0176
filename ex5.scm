(#%require schemeunit)

(define (manhattan-3d i j k)
  (/ (factorial (+ i j k)) (* (factorial i) (factorial j) (factorial k))))

(define (manhattan-3d* i j k)
  (if (or (= j k 0) (= i k 0) (= i j 0))
      1
      (+ (if (positive? i) (manhattan-3d* (- i 1) j k) 0)
         (if (positive? j) (manhattan-3d* i (- j 1) k) 0)
         (if (positive? k) (manhattan-3d* i j (- k 1)) 0))))

(define (factorial n)
  (define (go n acc)
    (if (zero? n) acc (go (- n 1) (* acc n))))
  (go n 1))

(check-equal? (manhattan-3d 0 0 7) 1)
(check-equal? (manhattan-3d 2 0 2) 6)
(check-equal? (manhattan-3d 1 1 1) 6)
(check-equal? (manhattan-3d 1 1 5) 42)
(check-equal? (manhattan-3d 2 3 1) 60)
(check-equal? (manhattan-3d 2 3 3) 560)

(check-equal? (manhattan-3d* 0 0 7) 1)
(check-equal? (manhattan-3d* 2 0 2) 6)
(check-equal? (manhattan-3d* 1 1 1) 6)
(check-equal? (manhattan-3d* 1 1 5) 42)
(check-equal? (manhattan-3d* 2 3 1) 60)
(check-equal? (manhattan-3d* 2 3 3) 560)
