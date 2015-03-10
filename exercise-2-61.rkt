#lang racket

(define (println x)
  (display x)
  (newline))

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define test-set1 '(1 2 3 4))
(define test-set2 '(3 4 5 6))

(intersection-set test-set1 test-set2)

; exercise 2.61

(println "exercise 2.61")

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 0 test-set1)
(adjoin-set 1 test-set1)
(adjoin-set 1.5 test-set1)
(adjoin-set 2 test-set1)
(adjoin-set 3 test-set1)
(adjoin-set 4 test-set1)
(adjoin-set 5 test-set1)

; exercise 2.62

(println "exercise 2.62")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2))
                    (r1 (cdr set1))
                    (r2 (cdr set2)))
                (cond ((= x1 x2) (cons x1 (union-set r1 r2)))
                      ((< x1 x2) (cons x1 (union-set r1 set2)))
                      ((> x1 x2) (cons x2 (union-set set1 r2))))))))

(union-set test-set1 test-set2)
