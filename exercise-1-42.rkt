#lang racket

(require "exercise-1-41.rkt")
(provide square)

(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))
  
((compose square inc) 6)

