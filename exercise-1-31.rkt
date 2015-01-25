#lang racket

;;; Exercise 1.31

; (a)
(define (product multiplicand-gen next-gen from to)
  (if (> from to)
      1
      (* (multiplicand-gen from)
         (product multiplicand-gen next-gen (next-gen from) to))))

(define (factorial n)
  (product (lambda (x) x) (lambda (y) (+ y 1)) 1 n))

(define (square x) (* x x))

(define (pi-approx prod-f n)
  (define (term num)
    (/ (* num (+ 2.0 num)) ; 2.0 to force use of inexact numbers
       (square (+ 1 num))))
  (define (next num) (+ 2 num))
  (* 4 (prod-f term next 2 n)))

; > (pi-approx product 6)
; 3.343673469387755
; > (pi-approx product 100)
; 3.157030176455167
; > (pi-approx product 1000)
; 3.1431607055322752
; > (pi-approx product 100000)
; 3.1416083612780588

; (b)
(define (product-iter multiplicand-gen next-gen from to)
  (define (helper from result)
    (if (> from to)
        result
        (helper (next-gen from)
                (* (multiplicand-gen from) result))))
  (helper from 1))

; > (pi-approx product-iter 6)
; 3.343673469387755
; > (pi-approx product-iter 100)
; 3.1570301764551667
; > (pi-approx product-iter 1000)
; 3.1431607055322712
; > (pi-approx product-iter 100000)
; 3.141608361278168
; > (pi-approx product-iter 100000000)
; 3.1415926690744156


