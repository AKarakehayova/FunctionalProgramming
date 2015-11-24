#lang racket
(define (sum numbers)
  (define (helper result number numbers)
    (if (empty? numbers)
        (+ result number)
    (helper (+ result number) (first numbers) (rest numbers))))
  (helper 0 (first numbers) (rest numbers)))

(define (member? x items)
  (define (helper x item items)
    (cond  [(equal? x item) #t]
           [(empty? items) (equal? x item)]
           [else (helper x (first items) (rest items))]))
  (helper x (first items) (rest items)))

(define (length2 items)
  (define (helper items result)
    (if (empty? items) result
        (helper (rest items) (+ result 1))))
  (helper (rest items) 0))

(define (list-ref2 items n)
  (define (helper x index items)
    (cond [(= index n) x]
          [else (helper (first items) (+ index 1) (rest items))]))
  (helper (first items) 0 (rest items)))

(define (range2 a b)
  (cond [(= a b) (list)]
        [else (cons a (range2 (+ a 1) b))]))

(define (build-list2 n f)
  (define (helper i)
    (if (= i n) (list)
        (cons (f i) (helper (+ i 1)))))
  (helper 0)
  )

(define (append2 l1 l2)
  (cond [(empty? l1) (cons (first l2) (append2 l1 (rest l2)))]
        [(empty? l2) (list)]
        [else (cons (first l1) (append (rest l1) l2))]))

(define (reverse2 items)
  (define (helper items result)
    (if (empty? items)
        result
        (helper (rest items) (cons (first items) result))))
    (helper items (list)))

(define (take2 n items)
  (cond
    [(= n 0) (list)]
    [else (cons (first items) (take2 (- n 1) (rest items)))]))

(define (drop2 n items)
  (cond
    [(> n (length items)) items]
    [else (drop2 n (rest items))]))

(define (take-while p items)
  (cond
    [(not (p (first items))) (list)]
    [else (cons (first items) (take-while p (rest items)))]))

(define (drop-while p items)
  (cond
    [(not (p (first items))) items]
    [else (drop-while p (rest items))]))

(define (number->list n)
  (define (helper n)
    (if (= n 0) `()
        (cons (remainder n 10) (helper (quotient n 10)))))
  (reverse(helper n)))
 
(define (list->number ns)
  (define (helper result ns)
    (cond [(empty? ns) result]
          [else (helper (+ (first ns) (* result 10)) (rest ns))]))
  (helper 0 ns))
    
