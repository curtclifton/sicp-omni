#lang racket

(provide mlist list->mlist)

(define (mlist . args)
  (list->mlist args))

(define (list->mlist lst)
  (if (null? lst)
      lst
      (mcons (car lst)
             (list->mlist (cdr lst)))))
