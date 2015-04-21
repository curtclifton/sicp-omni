#lang racket

(require "mlist.rkt")

(provide car cdr cons set-car! set-cdr! list caar mlist list->mlist mlist->list pair? length)

(define car mcar)
(define cdr mcdr)
(define cons mcons)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define list mlist)
(define (caar thing)
  (mcar (mcar thing)))
(define pair? mpair?)  
(define (length mlist)
  (define (helper mlist accum)
    (if (null? mlist)
        accum
        (helper (cdr mlist) (+ 1 accum))))
  (helper mlist 0))
