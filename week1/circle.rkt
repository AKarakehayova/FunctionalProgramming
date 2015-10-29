#lang racket

(define (circle? circle-x circle-y radius point-x point-y)
(if (<= (+ (* (- point-x circle-x) (- point-x circle-x)) (* (- point-y circle-y) (- point-y circle-y))) (* radius radius))
    #t
    #f)
  )
