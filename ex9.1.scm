(#%require schemeunit)

(define alphabet "ABCDEFGHILMNOPQRSTVX")
(define alphabet-length (string-length alphabet))

(define (encrypt str n)
  (apply string-append (map (lambda (c) (encrypt-char c n)) (string->list str))))

(define (encrypt-char c n)
  (let ((i (string-index-of-ci alphabet c)))
    (if (= i -1)
        ""
        (string (string-ref alphabet (modulo (+ i (modulo n alphabet-length)) alphabet-length))))))

(define (string-index-of-ci str c)
  (define (go i)
    (cond ((>= i (string-length str)) -1)
          ((char-ci=? (string-ref str i) c) i)
          (else (go (+ i 1)))))
  (go 0))

(check-equal? (encrypt "ALEA IACTA EST IVLIVS CAESAR DIXIT" 3) "DOHDNDFADHXANBONBXFDHXDVGNCNA")
