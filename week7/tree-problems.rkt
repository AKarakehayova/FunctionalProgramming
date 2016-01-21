(define (atom? x)
  (not (or
        (pair? x)
        (null? x)
        (vector? x))))

(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (car tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (leaf? tree)
  (and (empty-tree? (right tree)) (empty-tree? (left tree))   ))

(define t
  (make-tree 1
    (make-tree 2
      (make-leaf 5)
      (make-leaf 6))
    (make-leaf 3)))

(define (deep-map f l)
  (cond
    [(null? l) (list)]
    [(atom? (first l)) (cons (f (first l)) (deep-map f (rest l)))]
    [else (cons (deep-map f (first l)) (deep-map f (rest l)))]))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (left tree)) (height (right tree))))]))

(define (tree-level level tree)
(define (tree-level-help  i tree)
  (cond
    [(= level i) (list (root tree)) ]
    [ (and (empty-tree? (right tree)) (empty-tree? (left tree))) '() ] 
    [(empty-tree? (right tree))  (tree-level-help (+ i 1) (left tree)) ]
    [(empty-tree? (left tree))  (tree-level-help  (+ i 1) (right tree)) ]
    [else(append  (tree-level-help  (+ i 1) (left tree)) (tree-level-help (+ i 1) (right tree)))]))
   (tree-level-help 1 tree))

(define (tree-levels tree)
  (define (tree-levels-help tree  i)
  (cond
    [(> i (height tree)) '()]
    [else(append  (list(tree-level i tree)) (tree-levels-help tree  (+ i 1)) )]
    ))
  (tree-levels-help tree  1)
  )

(define(tree-map f tree) (deep-map f tree) )
