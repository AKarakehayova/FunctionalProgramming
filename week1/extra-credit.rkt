#lang racket
(define (cube a)
  (* a a a)
  )
 (define (sum-cubes a b)
  (+ (cube a) (cube b))
  )

(define (cube-sums? n)
 
  (define (helper a b)
    (cond [(= (sum-cubes a b) n) #t]
          [(and (< (cube a) n) (< (sum-cubes a b) n)) (helper a (+ b 1))]
          [(and (> (sum-cubes a b) n) (< (cube a) n)) (helper (+ a 1) 1)]
          [else #f]))
    (helper 1 1)
  )

(define (count-cube-sums from to)
  (define (helper from to count)
    (cond [(> from to) count]
          [(cube-sums? from) (helper (+ 1 from) to (+ 1 count))]
          [else (helper (+ 1 from) to count)]))
  (helper from to 0))
        
