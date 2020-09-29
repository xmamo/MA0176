(#%require schemeunit)

(define (encrypt str f)
  (define (g c)
    (let ((c1 (f c)))
      (if c1 (string c1) "")))
  (apply string-append (map g (string->list str))))

(define (caesar-cipher n)
  (lambda (c)
    (let* ((alphabet "ABCDEFGHILMNOPQRSTVX")
           (len (string-length alphabet))
           (i (string-index-of-ci alphabet c)))
      (if (= i -1) #f (string-ref alphabet (modulo (+ i n) len))))))

(define (string-index-of-ci str c)
  (define (go i)
    (cond ((>= i (string-length str)) -1)
          ((char-ci=? (string-ref str i) c) i)
          (else (go (+ i 1)))))
  (go 0))

(check-equal? (encrypt "ALEA IACTA EST IVLIVS CAESAR DIXIT" (caesar-cipher 3))
              "DOHDNDFADHXANBONBXFDHXDVGNCNA")
