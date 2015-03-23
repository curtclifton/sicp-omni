#lang racket

(require "section-2-put-get.rkt")

;;; Exercise 2.74

; Our global system needs to be able to get an employee-record-finding procedure given a division.
; Our global system needs to be able to get an item from an employee-record, given the devision and employee.

(define (acquire-vine)
  ;; lookup procedures
  (define (tag record)
    (cons 'vine record))
  (define (has-employee? employee-name)
    (eq? employee-name 'curt))
  (define (get-employee-record employee-name)
    ; Would use the employee-name to look up a record in the division's file, but we'll fake it:
    (if (has-employee? employee-name)
        (tag (list employee-name 'seattle 100000))
        #f))
  (define (get-employee-salary employee-record)
    (third employee-record))
  (define (get-employee-address employee-record)
    (second employee-record))
  ;; installation
  (put 'vine 'get-record get-employee-record)
  (put 'vine 'get-salary get-employee-salary)
  (put 'vine 'get-address get-employee-address)
  )

(define (acquire-periscope)
  ;; lookup procedures
  (define (tag record)
    (cons 'periscope record))
  (define (has-employee? employee-name)
    (eq? employee-name 'bob))
  (define (get-employee-record employee-name)
    ; Would use the employee-name to look up a record in the division's file, but we'll fake it:
    (if (has-employee? employee-name)
      (tag (cons employee-name (cons 'sanjose 120000)))
      #f))
  (define (get-employee-salary employee-record)
    (cddr employee-record))
  (define (get-employee-address employee-record)
    (cadr employee-record))
  ;; installation
  (put 'periscope 'get-record get-employee-record)
  (put 'periscope 'get-salary get-employee-salary)
  (put 'periscope 'get-address get-employee-address)
  )

(acquire-vine)
(acquire-periscope)

(define divisions '(vine periscope))

; (a) This question is horribly written. The problem description says to assume the files exist in different formats, then this question asks how they should be structured. They're structured however the fuck they're structured. The code above installs adapter procedures in the global scope that let us retrieve an employee's record for a given division. That record is meaningless without retrieving further division-specific interpretation procedures from the global scope. We tag the record with the division's name so we can look up the appropriate interpretation procedure. See part (b).

(define (get-record employee-name division)
  ((get division 'get-record) employee-name))

(get-record 'curt 'vine)
(get-record 'bob 'periscope)
(get-record 'bob 'vine)

; (b) The inanity continues.

(define (get-salary employee-record)
  ((get (car employee-record) 'get-salary) (cdr employee-record)))
  
(get-salary (get-record 'curt 'vine))
(get-salary (get-record 'bob 'periscope))

; (c)

(define (find-employee-record employee-name divisions)
  (if (null? divisions)
      #f
      (let ((maybe-record (get-record employee-name (car divisions))))
        (if maybe-record
            maybe-record
            (find-employee-record employee-name (cdr divisions))))))

(find-employee-record 'curt divisions)
(find-employee-record 'bob divisions)

; (d) With this implementation, we just need to define an installation procedure like acquire-vine or acquire-periscope above. (And magically assume the data itself is integrated.)
