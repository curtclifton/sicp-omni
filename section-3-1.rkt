#lang racket

(require "helpers.rkt")

;;; Exercise 3.1

(println "exercise 3.1")

(define (make-accumulator value)
  (lambda (increment)
    (begin
      (set! value (+ value increment))
      value)))

(define a (make-accumulator 10))
(a 0)
(a 10)
(a 0)
(a 15)

;;; Exercise 3.2

(println "exercise 3.2")

(define (make-monitored f)
  (define count 0)
  (define (monitored-f x)
    (cond ((eq? x 'reset-count) (set! count 0))
          ((eq? x 'how-many-calls?) count)
          (else
            (set! count (+ 1 count))
            (f x))))
  monitored-f)

(define monitored-sqrt (make-monitored sqrt))
(monitored-sqrt 10)
(monitored-sqrt 100)
(monitored-sqrt 'how-many-calls?)
(monitored-sqrt 'reset-count)
(monitored-sqrt 'how-many-calls?)

;;; Exercise 3.3

(println "exercise 3.3")

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (oops amount) "Incorrect password")
  (define (dispatch password-attempt m)
    (cond ((not (eq? password-attempt password)) oops)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

;;; Exercise 3.4

(println "exercise 3.4")

(define (call-the-cops)
  "busted")

(define (make-account-3-4 balance password)
  (define invalid-password-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (oops amount)
    (set! invalid-password-count (+ 1 invalid-password-count))
    (if (> invalid-password-count 7)
        (call-the-cops)
        "Incorrect password"))
  (define (dispatch password-attempt m)
    (if (not (eq? password-attempt password))
        oops
        (begin
          (set! invalid-password-count 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))))
  dispatch)

(define acc-3-4 (make-account-3-4 100 'secret-password))
((acc-3-4 'secret-password 'withdraw) 40)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'secret-password 'withdraw) 40)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
((acc-3-4 'some-other-password 'deposit) 50)
