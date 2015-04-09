#lang racket

(provide car cdr cons set-car! set-cdr!)

(define car mcar)
(define cdr mcdr)
(define cons mcons)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
