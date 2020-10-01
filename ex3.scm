(#%require schemeunit)

(define (bin-rep->number numeral)
  (rep->number "01" numeral))

(define (rep->number digits numeral)
  (case (string-ref numeral 0)
    ((#\+) (positive-rep->number digits (string-tail numeral)))
    ((#\-) (- (positive-rep->number digits (string-tail numeral))))
    (else (positive-rep->number digits numeral))))

(define (positive-rep->number digits numeral)
  (let* ((parts (rep-parts numeral))
         (integer (car parts))
         (fractional (cdr parts)))
    (if fractional
        (let ((x (+ (integer-rep->number digits integer) (fractional-rep->number digits fractional))))
          (if (integer? x) (round x) (exact->inexact x)))
        (integer-rep->number digits integer))))

(define (integer-rep->number digits numeral)
  (let ((len (string-length numeral)))
    (if (= len 1)
        (string-index-of digits (string-head numeral))
        (+ (* (string-length digits) (integer-rep->number digits (substring numeral 0 (- len 1))))
           (string-index-of digits (string-ref numeral (- len 1)))))))

(define (fractional-rep->number digits numeral)
  (/ (if (= (string-length numeral) 1)
         (string-index-of digits (string-head numeral))
         (+ (string-index-of digits (string-head numeral))
            (fractional-rep->number digits (string-tail numeral))))
     (string-length digits)))

(define (rep-parts numeral)
  (let ((i (string-index-of numeral #\.)))
    (if i
        (cons (substring numeral 0 i) (substring numeral (+ i 1)))
        (cons numeral #f))))

(define (string-head str)
  (string-ref str 0))

(define (string-tail str)
  (substring str 1 (string-length str)))

(define (string-index-of str c)
  (define (go i)
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) c) i)
          (else (go (+ i 1)))))
  (go 0))

(check-equal? (bin-rep->number "+1101") 13)
(check-equal? (bin-rep->number "0") 0)
(check-equal? (bin-rep->number "10110.011") 22.375)
(check-equal? (bin-rep->number "-0.1101001") -0.8203125)
(check-equal? (rep->number "zu" "-uuzz") -12)
(check-equal? (rep->number "0123" "+21.1") 9.25)
(check-equal? (rep->number "01234" "-10.02") -5.08)
(check-equal? (rep->number "0123456789ABCDEF" "0.A") 0.625)
(check-equal? (rep->number "0123456789ABCDEF" "1CF.0") 463)
