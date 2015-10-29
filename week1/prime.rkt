#lang racket
(define (prime? n)
  (define (helper i)
    (cond
          [(>= i n) #t]
          [(= (remainder n i) 0)#f]
          [else (helper (+ i 1))])
    )
  (if (= n 1) #f
      (helper 2)
      )
  )
