#lang racket

(provide put get)

(define lookup-table '())

;; With no concerns for efficiency, just put the new thing on the front of the look-up table:
(define (put op type item)
  (define new-table (cons (list op type item) lookup-table))
  (set! lookup-table new-table))

(define (get op type)
  (define (scan table)
    (if (null? table)
        #f
        (let ((first-entry (car table)))
          (if (and (equal? (car first-entry) op)
                   (equal? (cadr first-entry) type))
              (caddr first-entry)
              (scan (cdr table))))))
  (scan lookup-table))
