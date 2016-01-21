
(define (str->list str)
  (define (helper pos res)
    (cond [(= pos (string-length str)) res]
          [else (helper (add1 pos) (append res (list (~a (string-ref str pos)))))]))
  (helper 0 '()))
  
  
(define (run-length-encode str)
  (define (helper lst res)
    (cond [(empty? lst) res]
          [else (if (= (length (first lst)) 1)
                    (helper (rest lst) (string-append res (first (first lst))))
                    (helper (rest lst) (string-append res (number->string (length (first lst))) (first (first lst)))))]))
  (helper (group (str->list str)) ""))


(define (run-length-decode str)
  (define (append-while count sym res)
    (if (zero? count) res (append-while (- count 1) sym (string-append sym res))))
  (define (helper count res lst)
    (cond [(empty? lst) res]
          [(string->number (first lst)) (helper (+ (* (- count 1) 10) (string->number (first lst))) res (rest lst))]
          [else (helper 1 (string-append res (append-while count (first lst) "")) (rest lst))]))
  (helper 1 "" (str->list str)))
