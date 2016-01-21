(define (group l)
(cond
[(empty? l) '()]
[else (cons  (take-while (lambda (x) (equal? x (car l))) l)   (group (drop-while (lambda (x) (equal? x (car l))) l)))]))
