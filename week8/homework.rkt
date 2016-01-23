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

(define (truncatable-prime? n)
  (cond
    [(= n 0) #t]
    [(not (prime? n)) #f]
    [else(truncatable-prime? (quotient n 10))]))

(define (where elements predicates)
  (cond
    [(empty? (rest predicates)) (filter (first predicates) elements )]
    [else(where (filter (first predicates) elements ) (rest predicates))]))

(define (zero matrix)
(cond
  [(empty? (first matrix)) (map (lambda(x) '()) matrix) ]
  [(not(empty? (filter (lambda(x)(= x 0))(map first matrix)))) (map cons (map (lambda(x) 0)(map first matrix)) (zero (map rest matrix)))]
  [else(map cons (map first matrix) (zero (map rest matrix)))]))

(define (sum-rows matrix )
    (define l (map (lambda (x) (apply + x)) matrix ))
      (if  (eval (cons = l)) (first l) #f))

(define (transpose matrix ) 
  (cond
    [(empty? (first matrix)) '() ]
    [else( cons (reverse(map first matrix))(transpose (map rest matrix)))]))

(define (sum-columns matrix ) (sum-rows(transpose matrix)))

(define (list-diagonal-1 matrix ) 
  (cond
    [(empty? matrix) '() ]
    [else( cons (first (first matrix))(list-diagonal-1 (rest(map rest matrix))))]))

(define (list-diagonal-2 matrix )
    (list-diagonal-1 (transpose matrix)))

(define (sum-diagonal-1 matrix)
  (apply + (list-diagonal-1 matrix )))

(define (sum-diagonal-2 matrix)
  (apply + (list-diagonal-2 matrix )))

(define (magic-square? M)
(cond
  [(not(sum-rows M)) #f]
  [(not(sum-columns M)) #f]
  [else(=(sum-rows M) (sum-columns M) (sum-diagonal-1 M) (sum-diagonal-2 M))]))

(define (repeater str)
  (define (help x y)
    (cond
      [(= x 1) str]
      [(= x 0) "" ]
      [else (string-append str y (help (- x 1) y) )]
      ))
 (lambda (x y) (help x y)))
