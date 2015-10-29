#lang racket
(provide string-reverse)
(define(string-reverse str)
  (define length (- (string-length str) 1))
  (define (helper str i new)
    (if (> i length) new
         (helper str (+ i 1) (string-append new (~a (string-ref str (- length i)))))
     )
    )
  (helper str 0 "")
  )
(provide to-binary-string)
(define(to-binary-string n)
  (define(helper num n)
    (if (= n 0) (string-reverse (string-append num))
        (helper (string-append num (~a (remainder n 2))) (quotient n 2))))
     (helper "" n))
     
(define (from-binary-string binary-str)
  (define(helper num n base)
    (if (= n 0) num
        (helper (+ num (*(remainder n 10) base)) (quotient n 10) (* base 2))))
  (helper 0 (string->number binary-str) 1))
        
        
  
