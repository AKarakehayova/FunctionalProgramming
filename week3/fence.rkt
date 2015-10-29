#lang racket
(provide string-repeat)
(define (string-repeat str n)
  (define (helper i new)
    (if (>= i n) new
        (helper (+ i 1) (string-append new str))
        )
    )
  (helper 1 str)
  )

(define(fence n)
 (define(helper start fence l n m end)
   (string-append start fence l n m fence end)
   )
(helper "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (number->string n) "<" "}")
  )
