#lang racket

;;; Exercise 1.29

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term x) ; 4*y_{x} + 2*y_{x+1}
          (+ (* 4 (y x)) (* 2 (y (+ x 1)))))
  (define (next x) (+ x 2))
  (if (or (<= n 0) (not (even? n)))
      (error "n must be positive and even")
      (* (/ h 3)
         (+ (y 0)
            (sum term 1 next n)
            (* -1 (y n))))))

; some functions for testing
(define (const-gen n)
  (lambda (x) n))
(define (ident x) x)

; Curiously, this version of integral yields the exact answer for the definite integral of cube over [0,1] for all values of n that I've tried.
; Let's try integrating sine from 0 to π:

(define (integral-text f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (comparison f a b n-max)
  (define (helper n)
    (if (> n n-max)
        (begin
          (newline)
          (display "done"))
        (letrec ((dx (/ (- b a) n))
                 (text-answer (integral-text f a b dx))
                 (my-answer (integral f a b n)))
                (newline)
                (display (list "n" n "mine" my-answer "dx" dx "text" text-answer))
                (helper (* 10 n)))))
  (helper 10))
  
; > (comparison cube 0 1 10000)
; 
; (n 10 mine 1/4 dx 1/10 text 0.24874999999999994)
; (n 100 mine 1/4 dx 1/100 text 0.24998750000000042)
; (n 1000 mine 1/4 dx 1/1000 text 0.249999875000001)
; (n 10000 mine 1/4 dx 1/10000 text 0.24999999874993412)
; done
; > (comparison sin 0 pi 10000)
; 
; (n 10 mine 2.0001095173150043 dx 0.3141592653589793 text 2.0082484079079745)
; (n 100 mine 2.0000000108245044 dx 0.031415926535897934 text 2.0000822490709877)
; (n 1000 mine 2.000000000001083 dx 0.0031415926535897933 text 2.000000822467291)
; (n 10000 mine 2.0000000000000036 dx 0.0003141592653589793 text 2.0000000082243754)
; done
