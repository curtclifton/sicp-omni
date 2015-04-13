#lang racket

(require "helpers.rkt")
(require "mcons-all-the-things.rkt")

;;; Exercise 3.25

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (find-subtable! creating-if-needed rest-of-key-path subtable)
      (let ((next-subtable (assoc (car rest-of-key-path) subtable)))
        (cond ((null? (cdr rest-of-key-path)) subtable)
              (next-subtable (find-subtable! creating-if-needed
                              (cdr rest-of-key-path)
                              (next-subtable)))
              (creating-if-needed ; hit end of table along key-path without exhausting it
                (let ((new-subtable (list (car rest-of-key-path))))
                  ; splice new-subtable into the structure
                  (set-cdr! subtable
                            (cons new-subtable)
                            (cdr subtable))
                  ; and continue recursing to build any remaining subtables
                  (find-subtable! creating-if-needed
                    (cdr rest-of-key-path) new-subtable)))
              (else #f))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! value key-1 . other-keys)
      (define key-path (cons key-1 (list->mlist other-keys)))
      (define last-key
        (if (null? other-keys) ; we could just take last of key-path, but we've hacked cons to make mcons cells,
            key-1
            (last other-keys)))
      (let ((subtable (find-subtable! #t key-path (cdr local-table))))
        (let ((record (assoc last-key subtable)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons last-key value)
                              (cdr subtable)))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'find-subtable!) find-subtable!) 
            ((eq? m 'dump-table) local-table) ; debug helper to get at the embedded state
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table =))
(t 'dump-table)
;; NOTE: I changed the API of insert! to take value then keys
; ((t 'insert-proc!) 'zero 0)
; (t 'dump-table)
; ((t 'insert-proc!) 1 0 'one-zero)
; (t 'dump-table)
; ((t 'lookup-proc) 0 1)
; ((t 'lookup-proc) 1 0)
; ((t 'lookup-proc) 0 0)
