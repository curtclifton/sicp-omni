#lang racket

(require "helpers.rkt")
(require "mcons-all-the-things.rkt")

;;; Exercise 3.25

;; Let's get some well-named helpers instead of this caar bullshit the authors are fond of:
(define undefined-value '*undefined*)
(define (make-table-with-key key)
  (list key undefined-value))
(define (table-key table)
  (car table))
(define (table-root-value table)
  (car (cdr table)))
(define (table-subtable-list table)
  (cdr (cdr table)))
(define (set-table-root-value! table new-root-value)
  (let ((value-cell (cdr table)))
    (set-car! value-cell new-root-value)))
(define (add-subtable! table subtable)
  (let ((value-cell (cdr table)))
    (set-cdr! value-cell
              (cons subtable (table-subtable-list table)))
    'ok))
(define (table-has-root-value? table)
  (not (eq? (table-root-value table) undefined-value)))

(println "Testing table data structure:")
(define rt (make-table-with-key '*table*))
(println "raw table:")
rt
(println "(table-key rt): " (table-key rt))
(println "(table-root-value rt): " (table-root-value rt))
(println "(table-subtable-list rt): " (table-subtable-list rt))
(define rst (make-table-with-key 0))
(set-table-root-value! rst 'zero)
(add-subtable! rt rst)
(println "added <0> → 'zero")
rt
(println "(table-subtable-list rt): " (table-subtable-list rt))

(define (make-table same-key?)
  (let ((local-table 
          (make-table-with-key '*table*)))
    (define (assoc key subtables)
      (cond ((null? subtables) false)
            ((same-key? key (table-key (car subtables))) (car subtables))
            (else (assoc key (cdr subtables)))))
    (define (find-record! creating-if-needed rest-of-key-path subtable)
      (if (null? rest-of-key-path) 
          subtable
          (let ((nested-subtable (assoc (car rest-of-key-path)
                                      (table-subtable-list subtable))))
            (if nested-subtable
                (find-record! creating-if-needed 
                              (cdr rest-of-key-path)
                              nested-subtable)
                (if creating-if-needed
                    ; splice in new record/subtable and recurse into it
                    (let ((new-subtable/record 
                            (make-table-with-key (car rest-of-key-path))))
                      (add-subtable! subtable new-subtable/record)
                      (find-record! creating-if-needed
                                    (cdr rest-of-key-path)
                                    new-subtable/record))
                    ; not found
                    #f)))))
    (define (lookup key-1 . other-keys)
      (define key-path (cons key-1 (list->mlist other-keys)))
      (let ((subtable (find-record! #f key-path local-table)))
        (if (and subtable (table-has-root-value? subtable))
            (table-root-value subtable)
            false)))
    (define (insert! value key-1 . other-keys)
      (define key-path (cons key-1 (list->mlist other-keys)))
      (define last-key
        ; We could just take (last key-path), except it's a mlist thanks to our requires, so last would go “boom”.
        (if (null? other-keys)
            key-1
            (last other-keys)))
      (let ((subtable (find-record! #t key-path local-table)))
        (set-table-root-value! subtable value)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'find-record!) find-record!) 
            ((eq? m 'dump-table) local-table) ; debug helper to get at the embedded state
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table =))
(define lt (t 'dump-table))
(println "empty table: ")
lt
;; NOTE: I changed the API of insert! to take value then 1 or more keys
((t 'insert-proc!) 'zero 0)
(println "inserted <0> → 'zero")
lt
((t 'insert-proc!) 'one-zero 1 0)
(println "inserted <1 0> → 'one-zero")
lt
((t 'insert-proc!) 'zero-one 0 1)
(println "inserted <0 1> → 'zero-one")
lt
(println "expect to find <0>: " ((t 'lookup-proc) 0))
(println "don't expect to find <1>: " ((t 'lookup-proc) 1))
(println "expect to find <1 0>: " ((t 'lookup-proc) 1 0))
(println "expect to find <0 1>: " ((t 'lookup-proc) 0 1))
(println "don't expect to find <0 0>: " ((t 'lookup-proc) 0 0))
