#lang racket

;;; from exercise 1.37

(define (cont-frac-iter n d k)
  (define (helper i result)
    (let ((prev-i (- i 1)))
      (if (= i 1)
          result
          (helper prev-i (+ (d prev-i) (/ (n i) result))))))
  (/ (n 1) (helper k (d k))))


;;; exercise 1.39

(define (tan-cf x k)
  (define (n i) (* -1 x x))
  (define (d i) (+ 1 (* 2 (- i 1))))
  (/ (cont-frac-iter n d k) (* -1.0 x)))
  
; > (tan 1)
; 1.557407724654902
; > (tan-cf 1 1)
; 1.0
; > (tan-cf 1 2)
; 1.5
; > (tan-cf 1 3)
; 1.5555555555555556
; > (tan-cf 1 4)
; 1.5573770491803278
; > (tan-cf 1 10)
; 1.5574077246549023
