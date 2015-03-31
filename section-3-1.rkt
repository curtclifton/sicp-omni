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

;;; Exercise 3.5

(println "exercise 3.5")

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; racket random-in-range due to Rachael Worthington
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (estimate-integral predicate top left bottom right trials)
  (define (experiment)
    (define dart-x (random-in-range left right))
    (define dart-y (random-in-range bottom top))
    (predicate dart-x dart-y))
  (define hit-percentage (monte-carlo trials experiment))
  (define rect-area (* (- right left) (- top bottom)))
  (exact->inexact (* hit-percentage rect-area)))
  
(define unit-square-area
  (estimate-integral
    (lambda (x y) (and (>= x 0) (<= x 1) (>= y 0) (<= y 1)))
    10 0 0 10
    100000))

(println "unit-square-area: " unit-square-area)

(define (unit-circle x y)
  (<= (+ (* x x) (* y y)) (/ 1 4)))
(define unit-circle-area
  (estimate-integral
    unit-circle
    1 -1 -1 1
    1000000))

(println "unit-circle-area: " unit-circle-area)

;; area = π*r^2
(define pi-ish
  (/ unit-circle-area (/ 1 4)))

(println "pi-ish: " pi-ish)

;;; Exercise 3.6

(println "exercise 3.6")

(define (rand x)
  (cond ((eq? x 'generate) (random))
        ((eq? x 'reset) (lambda (s) (random-seed s)))
        (else (error "unknown command: " x))))

((rand 'reset) 0)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(println "resetting")
((rand 'reset) 0)
(rand 'generate)
(rand 'generate)
(rand 'generate)

;;; Exercise 3.7

(println "exercise 3.7")

(define (make-jointable-account balance username password)
  (define secrets (list (cons username password)))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (enroll new-username new-password)
    (set! secrets (cons (cons new-username new-password) secrets)))
  (define (is-valid? username password secrets)
    (cond ((eq? secrets '()) #f)
          ((and (eq? username (caar secrets))
                (eq? password (cdar secrets))) #t)
          (else (is-valid? username password (cdr secrets)))))
  (define (oops . ignored) "Incorrect username or password")
  (define (dispatch username password-attempt m)
    (cond ((not (is-valid? username password-attempt secrets)) oops)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'enroll) enroll)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define 
  (make-joint account old-username old-password new-username new-password)
  (begin
    ((account old-username old-password 'enroll) new-username new-password)
    account))

(define peter-acct (make-jointable-account 100 'peter 'rosebud))
((peter-acct 'peter 'rosebud 'withdraw) 40)
((peter-acct 'peter 'rosebowl 'withdraw) 40)
((peter-acct 'paul 'rosebud 'withdraw) 40)

(define paul-acct (make-joint peter-acct 'peter 'rosebud 'paul 'open-sesame))
((paul-acct 'paul 'open-sesame 'deposit) 10)
((peter-acct 'peter 'rosebud 'withdraw) 40)

;;; Exercise 3.8

(println "exercise 3.8")

(define (f-gen)
  (define previous 0)
  (lambda (x)
    (let ((answer previous))
      (set! previous x)
      answer)))

(define left-to-right
  (let ((f (f-gen)))
    (let ((left (f 0))
          (right (f 1)))
      (+ left right))))

(define right-to-left
  (let ((f (f-gen)))
    (let ((right (f 1))
          (left (f 0)))
      (+ left right))))

(println "left-to-right: " left-to-right)
(println "right-to-left: " right-to-left)
