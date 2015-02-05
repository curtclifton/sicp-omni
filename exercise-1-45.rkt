#lang racket

(require "exercise-1-36.rkt")
(require "exercise-1-41.rkt")
(require "exercise-1-42.rkt")
(require "exercise-1-43.rkt")
(require "exercise-1-44.rkt")


; (define (nth-root n)
;   (lambda (x guess)
;     (let ((f ((repeated average-damp (round (/ n 2))) (/ x (
;       (fixed-point f guess)


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
               
(define (nth-root-a n x)
  (fixed-point (lambda (y) (/ x (expt y n)))
               1.0))

(define (nth-root-b n x)
  (define (m-damped-f m)
    ((repeated average-damp m) (lambda (y) (/ x (expt y (- n 1))))))
  (fixed-point (m-damped-f n) 1.0))

(define (limited-fixed-point f first-guess)
  (let ((tolerance 0.00001)
        (max-iterations 20))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess count)
      (if (> count max-iterations)
          (cons 0 #f)
          (begin
            (display count)
            (display ": ")
            (display guess)
            (newline)
            (let ((next (f guess)))
              (if (close-enough? guess next)
                  (cons next #t)
                  (try next (+ 1 count)))))))
    (try first-guess 1)))
  
(define (sqrt2 x)
  (limited-fixed-point (lambda (y) (/ x y)) 1.0))
; (sqrt2 4) ===> '(0 . #f)

(define (sqrt3 x)
  (limited-fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
; (sqrt3 4) ===> '(2.000000000000002 . #t)

(define (nth-root-c n x)
  (define (m-damped-f m)
    ((repeated average-damp m) (lambda (y) (/ x (expt y (- n 1))))))
  (define (helper p)
    (let ((maybe-answer (limited-fixed-point (m-damped-f p) 1.0)))
      (if (cdr maybe-answer)
          (begin
            (newline)
            (display "damped ")
            (display p)
            (display " times:")
            (newline)
            (car maybe-answer))
          (helper (+ p 1)))))
  (helper 0))



