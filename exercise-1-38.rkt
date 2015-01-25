#lang racket

;;; from exercise 1.37

(define (cont-frac-iter n d k)
  (define (helper i result)
    (let ((prev-i (- i 1)))
      (if (= i 1)
          result
          (helper prev-i (+ (d prev-i) (/ (n i) result))))))
  (/ (n 1) (helper k (d k))))

;;; exercise 1.38

;  i d_{i} i%3
;  1   1    1
;  2   2    2
;  3   1    0
;  4   1    1
;  5   4    2
;  6   1    0
;  7   1    1
;  8   6    2
;  9   1    0
; 10   1    1
; 11   8    2
; …

(define (n i) 1)
(define (d i) 
  (if (= (modulo i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

(define (test-d)
  (define (helper i)
    (if (= i 12)
        (newline)
        (begin
          (display i)
          (display ": ")
          (display (d i))
          (newline)
          (helper (+ i 1)))))
  (helper 1))

(define (approx-e k)
  (+ 2.0 (cont-frac-iter n d k)))
  
; > (exp 1)
; 2.718281828459045
; > (approx-e 7)
; 2.7183098591549295
