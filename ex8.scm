(#%require schemeunit)
(#%require "hanoi.scm")

(define (hanoi-disks n step)
  (vector-map length (hanoi-disks-placement n step)))

(define (hanoi-disks-placement n step)
  (define (add rods pos radius)
    (case pos
      ((1) (list (cons radius (car rods)) (cadr rods) (caddr rods)))
      ((2) (list (car rods) (cons radius (cadr rods)) (caddr rods)))
      ((3) (list (car rods) (cadr rods) (cons radius (caddr rods))))))
  (define (go n step from to spare rods)
    (if (zero? step)
        (if (zero? n) rods (go (- n 1) 0 from to spare (add rods from n)))
        (let ((mid (expt 2 (- n 1))))
          (if (< step mid)
              (go (- n 1) step from spare to (add rods from n))
              (go (- n 1) (- step mid) spare to from (add rods to n))))))
  (list->vector (map reverse (go n step 1 2 3 (list (list) (list) (list))))))

(define (hanoi-picture n step)
  (define (rod-picture radii pos height)
    (cond ((null? radii) #f)
          ((null? (cdr radii)) (disk-image (car radii) n pos height))
          (else (above (rod-picture (cdr radii) pos (+ height 1))
                       (disk-image (car radii) n pos height)))))
  (let* ((vec (hanoi-disks-placement n step))
         (picture1 (rod-picture (vector-ref vec 0) 1 0))
         (picture2 (rod-picture (vector-ref vec 1) 2 0))
         (picture3 (rod-picture (vector-ref vec 2) 3 0)))
    (above* (above* (above* picture3 picture2) picture1) (towers-background n))))

(define (above* top bottom)
  (if top (if bottom (above top bottom) top) bottom))

(define (vector-map f vec)
  (list->vector (map f (vector->list vec))))

(check-equal? (hanoi-disks 3 0) (vector 3 0 0))
(check-equal? (hanoi-disks 3 1) (vector 2 1 0))
(check-equal? (hanoi-disks 3 2) (vector 1 1 1))
(check-equal? (hanoi-disks 3 3) (vector 1 0 2))
(check-equal? (hanoi-disks 3 4) (vector 0 1 2))
(check-equal? (hanoi-disks 3 5) (vector 1 1 1))
(check-equal? (hanoi-disks 3 6) (vector 1 2 0))
(check-equal? (hanoi-disks 3 7) (vector 0 3 0))
(check-equal? (hanoi-disks 5 13) (vector 2 1 2))
(check-equal? (hanoi-disks 15 19705) (vector 2 9 4))
(check-equal? (hanoi-disks 15 32767) (vector 0 15 0))

(check-equal? (hanoi-disks-placement 5 0) (vector (list 5 4 3 2 1) (list) (list)))
(check-equal? (hanoi-disks-placement 5 13) (vector (list 5 2) (list 1) (list 4 3)))
(check-equal? (hanoi-disks-placement 5 22) (vector (list 3 2) (list 5) (list 4 1)))
(check-equal? (hanoi-disks-placement 5 31) (vector (list) (list 5 4 3 2 1) (list)))
(check-equal? (hanoi-disks-placement 15 19705)
              (vector (list 3 2) (list 15 12 11 8 7 6 5 4 1) (list 14 13 10 9)))
