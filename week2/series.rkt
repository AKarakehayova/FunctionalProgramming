#lang racket
(define (series a b n)
  (define (helper a b i n)
   (cond [(= n 1) a]
         [(= n 2) b]
         [(> i n) b]
         [else (helper b (+ a b) (+ i 1) n)]
    )
    )
  (helper a b 3 n)
  )

(define (lucas n)
  (series 2 1 n)
)

(define (fibonacci n)
  (series 1 1 n)
  )

(define (summed-member n)
  (+ (lucas n) (fibonacci n))
  )

(define (nth-lucas-sum n)
  (define (helper n i s)
    (if (> i n) s
        (helper n (+ i 1) (+ s (lucas i)))
    )
    )
  (helper n 1 0)
  )

(define (nth-fibonacci-sum n)
  (define (helper n i s)
    (if (> i n) s
        (helper n (+ i 1) (+ s (fibonacci i)))
        )
    )
  (helper n 1 0)
  )

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n))
  )
