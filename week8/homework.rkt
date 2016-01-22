(define (prime? n)
  (define (prime-iter i)
    (cond [(>= i n) #t]
          [(= (remainder n i) 0) #f]
          [else (prime-iter (+ i 1))]))
  (if (= n 1) #f (prime-iter 2)))


(define (truncatable-prime? n)
  (cond
    [(= n 0) #t]
    [(not (prime? n)) #f]
    [else(truncatable-prime? (quotient n 10))]))

(define (where list-elements list-predicates)
  (cond
    [(empty? (cdr list-predicates)) (filter (car list-predicates) list-elements )]
    [else(where (filter (car list-predicates) list-elements ) (cdr list-predicates))]))

(define (zero matrix)
(cond
  [(empty? (car matrix)) (map (lambda(x) '()) matrix) ]
  [(not(empty? (filter (lambda(x)(= x 0))(map car matrix)))) (map cons (map (lambda(x) 0)(map car matrix)) (zero (map cdr matrix)))]
  [else(map cons (map car matrix) (zero (map cdr matrix)))]))

(define (sum-rows matrix )
    (define l (map (lambda (x) (apply + x)) matrix ))
      (if  (eval (cons = l)) (car l) #f))

(define (transpose matrix ) 
  (cond
    [(empty? (car matrix)) '() ]
    [else( cons (reverse(map car matrix))(transpose (map cdr matrix)))]))

(define (sum-columns matrix ) (sum-rows(transpose matrix)))

(define (list-diagonal-1 matrix ) 
  (cond
    [(empty? matrix) '() ]
    [else( cons (car (car matrix))(list-diagonal-1 (cdr(map cdr matrix))))]))

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
