#lang racket

;;; Exercise 1.32

; (a)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
                
(define (sum term a next b)
  (accumulate + 0 term a next b))
  
(define (product term a next b)
  (accumulate * 1 term a next b))
  
(define (ident x) x)
(define (inc x) (+ 1 x))
  
; > (sum ident 1 inc 10)
; 55
; > (sum ident 1 inc 2)
; 3
; > (sum ident 1 inc 3)
; 6
; > (product ident 1 inc 3)
; 6
; > (product ident 1 inc 4)
; 24
; > (product ident 1 inc 5)
; 120
; > (product ident 1 inc 6)
; 720

; (b)

(define (accumulate-iter combiner null-value term a next b)
  (define (helper a result)
    (if (> a b)
        result
        (helper (next a)
                (combiner result (term a)))))
  (helper a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))
  
(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))
  
; > (sum-iter ident 1 inc 10)
; 55
; > (product-iter ident 1 inc 3)
; 6
; > (product-iter ident 1 inc 4)
; 24
; > (product-iter ident 1 inc 5)
; 120
; > (product-iter ident 1 inc 6)
; 720
