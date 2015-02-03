#lang racket

(define (inc n) (+ n 1))

(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))
  
((compose square inc) 6)

