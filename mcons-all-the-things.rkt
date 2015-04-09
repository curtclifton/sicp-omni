#lang racket

(require "mlist.rkt")

(provide car cdr cons set-car! set-cdr! list)

(define car mcar)
(define cdr mcdr)
(define cons mcons)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define list mlist)
