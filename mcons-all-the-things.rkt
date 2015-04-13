#lang racket

(require "mlist.rkt")

(provide car cdr cons set-car! set-cdr! list caar)

(define car mcar)
(define cdr mcdr)
(define cons mcons)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define list mlist)
(define (caar thing)
  (mcar (mcar thing)))

