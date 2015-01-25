#lang racket

;;; exercise 1.37

; (a)
(define (cont-frac-recur n d k)
  (define (denom i)
    (let ((d_i (d i))
          (next-i (+ i 1)))
      (if (= i k)
          d_i
          (+ d_i (/ (n next-i)
                    (denom next-i))))))
  (/ (n 1) (denom 1)))

; From exercise 1.35:
; > phi
; 1.6180327868852458
; > (/ 1 phi)
; 0.6180344478216819

; > (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10)
; 0.6179775280898876

; (b)

(define (cont-frac-iter n d k)
  (define (helper i result)
    (let ((prev-i (- i 1)))
      (if (= i 1)
          result
          (helper prev-i (+ (d prev-i) (/ (n i) result))))))
  (/ (n 1) (helper k (d k))))

; > (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
; 0.6179775280898876

