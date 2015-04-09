#lang racket

(provide mlist)

(define (mlist . args)
  (define (helper remaining-args)
    (if (null? remaining-args)
        remaining-args
        (mcons (car remaining-args)
               (helper (cdr remaining-args)))))
  (helper args))
