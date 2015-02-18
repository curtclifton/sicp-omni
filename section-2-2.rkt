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

;;; Reverse 2.21

(define (square n) (* n n))

(define (square-list-recur items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-recur (cdr items)))))

(square-list-recur (list 1 2 3 4 5))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4 5))

;;; Exercise 2.22

; With car and cdr, we unpack a list from the beginning. Louis's iterative solution builds the accumulate by cons-ing values onto the beginning of an initially empty accumulator. Thus, the square of the first item becomes the last item of the result; the square of the second item becomes the second to last item of the result; and so on.

; Switching the arguments doesn't work, because it forms nested cons cells, not chained ones.

;;; Exercise 2.23

(define (for-each f lst)
  (map (lambda (n) (f n)) lst)
  #t)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;;; Exercise 2.24

(list 1 (list 2 (list 3 4)))
; ==> '(1 (2 (3 4)))

; See exercise-2-24.graffle for diagrams.

;;; Exercise 2.25

; Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(define list-2-25-a '(1 3 (5 7) 9))
(define list-2-25-b '((7)))
(define list-2-25-c '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list-2-25-a)))))
(car (car list-2-25-b))
(cadr (cadr (cadr (cadr (cadr (cadr list-2-25-c))))))

;;; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; ==> '(1 2 3 4 5 6)
(cons x y) ; ==> '((1 2 3) 4 5 6)
(list x y) ; ==> '((1 2 3) (4 5 6))

;;; Exercise 2.27

(define list-2-27 (list (list 1 2) (list 3 4)))

(define (deep-reverse arg)
  (define (deep-reverse-helper arg) 
    (if (list? arg)
      (deep-reverse-iter arg '())
      arg))
  (define (deep-reverse-iter lst accum)
    (if (null? lst)
        accum
        (deep-reverse-iter 
          (cdr lst)
          (cons (deep-reverse-helper (car lst)) accum))))
  (deep-reverse-helper arg))

(reverse list-2-27)
(deep-reverse list-2-27)

;;; Exercise 2.28

(define list-2-28 (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define (fringe-helper maybe-tree accum)
    (cond ((not (list? maybe-tree))
            (list maybe-tree))
          ((null? maybe-tree)
            accum)
          (else 
            (append (fringe-helper (car maybe-tree) '())
                    (fringe-helper (cdr maybe-tree) accum)))))
  (fringe-helper tree '()))

(fringe list-2-28)

;;; Exercise 2.29

; (define (make-mobile left right)
;   (list left right))
; (define (make-branch len structure)
;   (list len structure))
; (define (mobile? structure) ;; added for part (d):
;   (list? structure))

;        ___left.length____|___right.length_______
;       |                                         |
; left.structure                          right.structure

; (a)

; (define (left-branch mobile)
;   (car mobile))
; (define (right-branch mobile)
;   (car (cdr mobile)))
; (define (branch-length branch)
;   (car branch))
; (define (branch-structure branch)
;   (car (cdr branch)))

; (b)

(define (total-weight structure)
  (if (mobile? structure)
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))
      structure))

; (total-weight mobile-2-29-1)
; (total-weight mobile-2-29-2)

; (c)

(define (balanced? structure)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (mobile? structure)
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (and (balanced? (branch-structure left))
             (balanced? (branch-structure right))
             (eq? (branch-torque left)
                  (branch-torque right))))
      #t))

; (d)

(define (make-mobile left right)
  (cons left right))
(define (make-branch len structure)
  (cons len structure))
(define (mobile? structure)
  (pair? structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

; To make total-weight and balanced? work, I had to add a mobile? predicate and use it in the definitions of total-weight and balanced? I also had to make sure the accessor procedures were defined for the new represenation.

(define mobile-2-29-1
  (make-mobile
    (make-branch 10
                 (make-mobile
                   (make-branch 5 10)
                   (make-branch 2 12)))
    (make-branch 8 8)))

(define mobile-2-29-2
  (make-mobile (make-branch 5 2) (make-branch 10 1)))

