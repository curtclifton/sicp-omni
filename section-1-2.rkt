#lang racket

;;; Exercise 1.9
; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))
;
; (+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
; looks recursive to me
; -----------------------------------------------------------------------------
; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))
;
; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9
;
; this is iterative

;;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; > (A 1 10)
; 1024
; > (A 2 4)
; 65536
; > (A 3 3)
; 65536

(define (f n) (A 0 n))
; 2*n

(define (g n) (A 1 n))
; 2^n

(define (h n) (A 2 n))
; expanding:
; (A 2 n)
; (A 1 (A 2 (- n 1)))
; 2^(A 2 (- n 1))
; 2^2^...^2 (n times)

; checking work for closed form of h
(define (pow2 n)
  (cond 
    ((< n 0) (error "pow2 undefined for negative n"))
    ((= n 0) 1)
    (else (expt 2 (pow2 (- n 1))))))

; and an iterative version
(define (pow2-iter n)
  (define (helper m accum)
    (cond
      ((< m 0) (error "pow2-iter undefined for negative n"))
      ((= m 0) accum)
      (else (helper (- m 1) (expt 2 accum)))))
  (helper n 1))
  
