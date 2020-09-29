(#%require schemeunit)

(define (frase soggetto verbo oggetto)
  (string-append (articolo soggetto) " " soggetto " "
                 (declina verbo (singolare? soggetto)) " "
                 (articolo oggetto) " " oggetto))

(define (articolo sostantivo)
  (if (singolare? sostantivo)
      (if (maschile? sostantivo) "il" "la")
      (if (maschile? sostantivo) "i" "le")))

(define (declina verbo singolare?)
  (let* ((len (string-length verbo))
         (str1 (substring verbo 0 (- len 3)))
         (str2 (substring verbo (- len 3) len)))
    (cond ((string=? str2 "are") (string-append str1 (if singolare? "a" "ano")))
          ((string=? str2 "ere") (string-append str1 (if singolare? "e" "ono")))
          ((string=? str2 "ire") (string-append str1 (if singolare? "e" "ono"))))))

(define (singolare? sostantivo)
  (case (string-ref sostantivo (- (string-length sostantivo) 1))
    ((#\o #\a) #t)
    ((#\i #\e) #f)))

(define (maschile? sostantivo)
  (case (string-ref sostantivo (- (string-length sostantivo) 1))
    ((#\o #\i) #t)
    ((#\a #\e) #f)))

(check-equal? (frase "gatto" "cacciare" "topi") "il gatto caccia i topi")
(check-equal? (frase "mucca" "mangiare" "fieno") "la mucca mangia il fieno")
(check-equal? (frase "sorelle" "leggere" "novella") "le sorelle leggono la novella")
(check-equal? (frase "bambini" "amare" "favole") "i bambini amano le favole")
(check-equal? (frase "musicisti" "suonare" "pianoforti") "i musicisti suonano i pianoforti")
(check-equal? (frase "cuoco" "friggere" "patate") "il cuoco frigge le patate")
(check-equal? (frase "camerieri" "servire" "clienti") "i camerieri servono i clienti")
(check-equal? (frase "mamma" "chiamare" "figlie") "la mamma chiama le figlie")
