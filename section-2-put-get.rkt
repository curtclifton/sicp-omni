#lang racket

(provide put get)

(define lookup-table '())

;; With no concerns for efficiency, just put the new thing on the front of the look-up table:
(define (put-tuple tuple)
  (define-values (key item-list) (split-at tuple (- (length tuple) 1)))
  (define new-table (cons (cons key (car item-list)) lookup-table))
  (set! lookup-table new-table))

(define (put . args)
  (if (< (length args) 2)
      (error "must have at least two arguments to put, 1 or more keys, plus a value")
      (put-tuple args)))

(define (get . args)
  (define (scan table)
    (if (null? table)
        #f
        (let ((first-entry (car table)))
          (if (equal? (car first-entry) args)
              (cdr first-entry)
              (scan (cdr table))))))
  (if (null? args)
      (error "must have at least one or more keys to get")
      (scan lookup-table)))
