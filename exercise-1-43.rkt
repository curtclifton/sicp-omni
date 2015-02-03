#lang racket

(define (square n) (* n n))

(define (repeated f n)
  (define (repeated-iter count accum)
    (if (= count 1)
        accum
        (repeated-iter (- count 1) (compose f accum))))
  (repeated-iter n f))

((repeated square 2) 5)

