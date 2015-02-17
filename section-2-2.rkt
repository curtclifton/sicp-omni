#lang racket

;;; Exercise 2.17

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))
      
(last-pair (list 23 72 149 34))

;;; Exercise 2.18

(define (reverse lst)
  (define (reverse-helper rest accum)
    (if (null? rest)
        accum
        (reverse-helper (cdr rest) (cons (car rest) accum))))
  (reverse-helper lst '()))

(reverse (list 1 4 9 16 25))

;;; Reverse 2.19

(define no-more? null?)

(define first-denomination car)

(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

;;; Reverse 2.20

(define (same-parity head . lst)
  (define (same-parity a b)
    (eq? (even? a) (even? b)))
  (define (same-parity-helper lst accum)
    (if (null? lst)
        accum
        (if (same-parity head (car lst))
            (same-parity-helper (cdr lst) (cons (car lst) accum))
            (same-parity-helper (cdr lst) accum))))
  (reverse (same-parity-helper lst (list head))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
