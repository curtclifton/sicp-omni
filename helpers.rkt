#lang racket

(provide println)

(define (println . xs)
  (define (print items)
    (if (null? items)
        (newline)
        (begin
          (display (car items))
          (print (cdr items)))))
  (print xs))
