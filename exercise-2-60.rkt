#lang racket

;;; Exercise 2.60

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

(define test-set1 '(1 2 3 4))
(define test-set2 '(3 4 5 6))

(intersection-set test-set1 test-set2)
(union-set test-set1 test-set2)

; element-of-set? is O(m), where m is the total number of insertions done on the set, vs. O(n) for an n-element set in the non-duplicate representation.
; adjoin-set is O(1), versus O(n) for the non-duplicate representation.
; intersection-set is O(m^2), where m is the total number of insertions to the sets, vs. O(n^2) for n-element sets in the non-duplicate representation.
; union-set is O(m), unless there is an internal implementation of append that's more efficient. For the non-duplicate representation, union-set is O(n^2).

; In a use case where insertion and union dominated, the representation allowing duplicates would be more efficient. Particularly interesting in this scenario would be using the non-duplicate representation with periodic compaction to remove duplicates.

