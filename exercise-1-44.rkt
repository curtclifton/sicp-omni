#lang racket

(require "exercise-1-41.rkt")
(require "exercise-1-42.rkt")
(require "exercise-1-43.rkt")

(provide n-fold-smooth test)

(define (smoother dx)
  (lambda (f)
    (lambda (x)
      (let ((preceding-val (f (- x dx)))
            (val (f x))
            (following-val (f (+ x dx))))
        (/ (+ preceding-val val following-val)
           3.0)))))

(define smooth (smoother 0.001))

(define (n-fold-smooth f n dx)
  (repeated ((smoother dx) f) n))
  
(define (test a b step-f f)
  (define (test-iter a)
    (if (> a b)
        (newline)
        (begin
          (newline)
          (display a)
          (display "\t")
          (display (f a))
          (display "\t")
          (display ((smooth f) a))
          (test-iter (step-f a))
          )))
  (begin
    (display "a\tf(a)\tsf(a)")
    (test-iter a)))

          
  

