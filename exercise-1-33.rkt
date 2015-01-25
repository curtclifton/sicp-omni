#lang racket

;;; prime? from section 1.2

(define (square n) (* n n))
(define (divides? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (define (next n) (+ n 1))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; gcd from section 1.2

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Exercise 1.33

(define (filtered-accumulate-recur combiner null-value term a next b include?)
  (if (> a b)
      null-value
      (if (include? a)
          (combiner (term a)
                    (filtered-accumulate-recur combiner null-value term (next a) next b include?))
          (filtered-accumulate-recur combiner null-value term (next a) next b include?))))

(define (ident x) x)
(define (inc x) (+ 1 x))

(define (sum-odds-recur a b)
  (filtered-accumulate-recur + 0 ident a inc b odd?))

(define (filtered-accumulate combiner null-value term a next b include?)
  (define (helper a result)
    (if (> a b)
        result
        (if (include? a)
            (helper (next a) (combiner result (term a)))
            (helper (next a) result))))
  (helper a null-value))

(define (sum-odds a b)
  (filtered-accumulate + 0 ident a inc b odd?))

; (a)

(define (sum-of-primes-squared a b)
  (filtered-accumulate + 0 square a inc b prime?))

; “b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).”

(define (ex-1-33-b n)
  (filtered-accumulate * 1 ident 1 inc (- n 1) (lambda (i) (= (gcd i n) 1))))

(define (check max-n)
  (define (helper n)
    (if (> n max-n)
        (newline)
        (begin
          (newline)
          (display (list n (ex-1-33-b n)))
          (helper (+ n 1)))))
  (helper 1))

; > (check 10)
; (1 1)
; (2 1)
; (3 2)
; (4 3)
; (5 24)
; (6 5)
; (7 720)
; (8 105)
; (9 2240)
; (10 189)
