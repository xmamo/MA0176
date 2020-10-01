(#%require schemeunit)

(define (encrypt str f)
  (string-map str f))

(define (caesar-cipher n)
  (lambda (c)
    (let* ((alphabet "ABCDEFGHILMNOPQRSTVX")
           (len (string-length alphabet))
           (i (string-index-of-ci alphabet c)))
      (if i (string-ref alphabet (modulo (+ i n) len)) #f))))

(define (string-index-of-ci str c)
  (define (go i)
    (cond ((>= i (string-length str)) #f)
          ((char-ci=? (string-ref str i) c) i)
          (else (go (+ i 1)))))
  (go 0))

(define (string-map str f)
  (define (g c)
    (let ((c1 (f c)))
      (if c1 (string c1) "")))
  (apply string-append (map g (string->list str))))

(check-equal? (encrypt "ALEA IACTA EST IVLIVS CAESAR DIXIT" (caesar-cipher 3))
              "DOHDNDFADHXANBONBXFDHXDVGNCNA")
