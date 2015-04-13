#lang racket

(require "helpers.rkt")
(require "mcons-all-the-things.rkt")

;;; Exercise 3.24

;; This 2-d table implementation is just a copy of the text's with three small changes:
;; • added the same-key? parameter to make-table as specified in the exercise
;; • embedded the assoc procedure (from the text) in the make-table procedure
;; • changed assoc to capture and use same-key? instead of using equal?

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'dump-table) local-table) ; debug helper to get at the embedded state
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table =))
(t 'dump-table)
((t 'insert-proc!) 0 1 'zero-one)
(t 'dump-table)
((t 'insert-proc!) 1 0 'one-zero)
(t 'dump-table)
((t 'lookup-proc) 0 1)
((t 'lookup-proc) 1 0)
((t 'lookup-proc) 0 0)
