#lang racket

;;; Exercise 1.1
; 10 ==> 10
; (+ 5 3 4) ==> 12
; (- 9 1) ==> 8
; (/ 6 2) ==> 3
; (+ (* 2 4) (- 4 6)) ==> (+ 8 -2) ==> 6
(define a 3)
(define b (+ a 1))
; (+ a b (* a b)) ==> (+ 3 4 (* 3 4)) ==> 19
; (= a b) ==> #f
; (if (and (> b a) (< b (* a b))) b a) ==> b ==> 4
; (cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25)) ==> (+ 6 7 a) ==> 16
; (+ 2 (if (> b a) b a)) ==> (+ 2 b) ==> 6
; (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1)) ==> (* b (+ a 1)) ==> 16

;;; Exercise 1.2
(define ex-1-2 (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                  (* 3 (- 6 2) (- 2 7))))


;;; Exercise 1-3
(define (ex-1-3 n m o)
  (define (sum-sq x y) (+ (* x x) (* y y)))
  (cond ((and (>= n o) (>= m o)) (sum-sq n m))
        ((and (>= n m) (>= o m)) (sum-sq n o))
        (else (sum-sq m o))))

(= (ex-1-3 1 2 3) 13)
(= (ex-1-3 1 3 2) 13)
(= (ex-1-3 2 1 3) 13)
(= (ex-1-3 2 3 1) 13)
(= (ex-1-3 3 1 2) 13)
(= (ex-1-3 3 2 1) 13)
(= (ex-1-3 1 2 2) 8)
(= (ex-1-3 2 1 2) 8)
(= (ex-1-3 2 2 1) 8)
(= (ex-1-3 1 1 2) 5)
(= (ex-1-3 1 2 1) 5)
(= (ex-1-3 1 1 2) 5)

;;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; The if expression results in the addition function if b is positive, 
; otherwise it results in the subtraction function. Subtracting a negative 
; valued b from a is the same as adding the absolute value of b to a.
(= (a-plus-abs-b 3 4) 7)
(= (a-plus-abs-b 3 -4) 7)

;;; Exercise 1.5
(define (p) (p)) ; infinite regress
(define (test x y)
  (if (= x 0)
      0
      y))
; (test 0 (p))
; With applicative order 0 and (p) are evaluated before evaluating the test 
; procedure. (p) is an infinite regress, so test is never evaluated.
; With normal order, test is evaluated first, then the condition of the if 
; expression. The condition is true, so the consequent is evaluated, yielding 0.

;;; Exercise 1.6
; Attempting to define the if special form as a procedure doesn't work because
; of applicative order. The recursive call to sqrt-iter is made every time; unlike
; the if special form, new-if's consequent and alternate are both evaluated.

; But we can come close if we make our new if take a condition and two procedure.
; Then we can choose to evaluate only the procedure that we need to based on the
; condition, like so:
(define (newer-if condition consequent-f alternate-f)
  (cond (condition (consequent-f))
        (else (alternate-f))))
(define (improve guess x)
  (inexact-average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))
(define (new-sqrt-iter guess x)
  (define (answer) guess)
  (define (recurse) (new-sqrt-iter (improve guess x)
                                   x))
  (newer-if (good-enough? guess x)
      answer
      recurse))
(define (sqrt x) (new-sqrt-iter 1 x))

;;; Exercise 1.7
; The actual sqrt of 0.0001 is 0.01. Our current procedure yields 
; 0.03230844833048122 because we quit too soon.
; For very large numbers, Racket uses rational numbers for division and quickly
; gets to numerators and denominators that are too large to deal with efficiently.
; We can re-define average to use inexact numbers (replacing the use in improve
; above):
(define (inexact-average x y)
  (exact->inexact (/ (+ x y) 2)))
; With this definition, some large values work:
; (sqrt 4000000000000) ==> 2000000.0
; But others seem to run forever, presumably because we can't improve on our guess
; sufficiently to terminate:
; (sqrt 8000000000000)
; Changing the maximum error from 0.001 to 1.0 causes this expression to terminate.
(define (better-sqrt n)
  (define (good-enough? guess prev-guess)
    (< (abs (/ (- guess prev-guess) guess)) 0.001))
  (define (inexact-average x y)
    (exact->inexact (/ (+ x y) 2)))
  (define (improve guess)
    (inexact-average guess (/ n guess)))
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1 0))
; This is better for small numbers:
; (better-sqrt 0.0001) ==> 0.010000000025490743
; For large numbers, it's less exact for a perfect square:
; (better-sqrt 4000000000000) ==> 2000000.206922484
; but it terminates for our previous bad example:
; (better-sqrt 8000000000000) ==> 2828427.1250269003

;;; Exercise 1.8
(define (approx-iter-gen improve-f)
  (define (good-enough? guess prev-guess)
    (< (abs (/ (- guess prev-guess) guess)) 0.001))
  (define (approx-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (approx-iter (improve-f guess) guess)))
  approx-iter)

(define (cube-root n)
  (define (improve guess)
    (exact->inexact (/ (+ (/ n (* guess guess)) (* 2 guess))
                       3)))
  (define cube-iter (approx-iter-gen improve))
  (cube-iter 1 0))








