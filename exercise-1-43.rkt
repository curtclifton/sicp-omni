#lang racket

(require "exercise-1-41.rkt")
(require "exercise-1-42.rkt")

(provide repeated)

(define (repeated f n)
  (define (repeated-iter count accum)
    (if (= count 0)
        accum
        (repeated-iter (- count 1) (compose f accum))))
  (repeated-iter n identity))

((repeated square 2) 5)

