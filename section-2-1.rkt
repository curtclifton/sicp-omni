#lang racket

;;; Exercise 2.1

(define (sign n)
  (if (< n 0) -1 1))

(define (make-rat num den)
  (let ((g (gcd num den))
        (s (sign (* num den))))
    (cons (* s (abs (/ num g)))
          (abs (/ den g)))))

;;; Exercise 2.2

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define (average a b) (/ (+ a b)))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (let ((mx (average x1 x2))
          (my (average y1 y2)))
      (make-point mx my))))



    
