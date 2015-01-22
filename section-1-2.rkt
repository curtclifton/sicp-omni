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
  
;;; Exercise 1.11
; “f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n > 3”

(define (ex-1-11-recur n)
  (if (< n 3)
      n
      (+ (ex-1-11-recur (- n 1))
         (* 2 (ex-1-11-recur (- n 2)))
         (* 3 (ex-1-11-recur (- n 3))))))


; f(3) = f(2) + 2*f(1) + 3*f(0)
; f(4) = (f(2) + 2*f(1) + 3*f(0)) + 2*f(2) + 3*f(1)
; f(5) = ((f(2) + 2*f(1) + 3*f(0)) + 2*f(2) + 3*f(1)) + 2*(f(2) + 2*f(1) + 3*f(0)) + 3*f(3)
(define (ex-1-11-iter n)
  (define (arith-helper a b c) (+ a (* 2 b) (* 3 c)))
  (define (helper count f1 f2 f3)
    (cond
      ((< count 3) count)
      ((= count 3) (arith-helper f1 f2 f3))
      (else (helper (- count 1) (arith-helper f1 f2 f3) f1 f2))))
  (helper n 2 1 0))
  
(define (all . values)
  (define (helper val-list accum)
    (cond 
      ((not accum) #f)
      ((eq? val-list '()) accum)
      (else (helper (cdr val-list) (car val-list)))))
  (helper values #t))
  
(define (test-1-11 count)
  (define (comparison n)
    (= (ex-1-11-recur n) (ex-1-11-iter n)))
  (apply all (map comparison (range count))))
  
;;; Exercise 1.12
(define (pascal row column)
  (cond 
    ((or (< row 0) (< column 0)) (error "only non-negative input please"))
    ((> column row) (error "column must be no greater than row"))
    ((= row 0) 1)
    ((= column 0) 1)
    ((> column (/ row 2)) (pascal row (- row column))) ; flip to left half of row
    (else (+ (pascal (- row 1) (- column 1))
             (pascal (- row 1) column))))) ; this will only work for left half of rows

(define (print-pascal count)
  (define (pascal-row row)
    (define (pascal-col column) (pascal row column))
    (map pascal-col (range (+ row 1))))
  (define (print-row n)
    (pretty-print (pascal-row n)))
  (map print-row (range (+ count 1)))
  #t)
  
;;; Exercise 1.13
; nope

;;; Exercise 1.14
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (count-change 11)
; (cc 11 5)             
; (+ (cc 11 4)                                                                                                                                                                                                                                                                                                                 (cc -39 5))
; (+ (+ (cc 11 3)                                                                                                                                                                                                                                                                                                  (cc -14 4)) 0         )
; (+ (+ (+ (cc 11 2)                                                                                                                                                                                                                                            (cc 1 3))                                          0         ) 0         )
; (+ (+ (+ (+ (cc 11 1)                                                                                             (cc 6 2))                                                                                                                                   (+ (cc 1 2)                            (cc -4 3))) 0         ) 0         )
; (+ (+ (+ (+ (+ (cc 11 0) (cc 10 1))                                                                               (+ (cc 6 1)                                                                               (cc 1 2)))                                        (+ (+ (cc 1 1)              (cc -4 2)) 0        )) 0         ) 0         )
; (+ (+ (+ (+ (+ 0         (+ (cc 10 0) (cc 9 1)                                                    ))              (+ (+ (cc 6 0) (cc 5 1)                                                    )              (+ (cc 1 1)              (cc -4 2))))             (+ (+ (+ (cc 1 0) (cc 0 0)) 0        ) 0        )) 0         ) 0         )
; (+ (+ (+ (+ (+ 0         (+ 0         (+ (cc 9 0) (cc 8 1)                                       )))              (+ (+ 0        (+ (cc 5 0) (cc 4 1)                                       ))              (+ (+ (cc 1 0) (cc 0 0)) 0        )))             (+ (+ (+ 0        1       ) 0        ) 0        )) 0         ) 0         )
; (+ (+ (+ (+ (+ 0         (+ 0         (+ 0        (+ (cc 8 0) (cc 7 1)                          ))))              (+ (+ 0        (+ 0        (+ (cc 4 0) (cc 3 1)                          )))              (+ (+ 0        1       ) 0        )))             (+ (+ (+ 0        1       ) 0        ) 0        )) 0         ) 0         )
; (+ (+ (+ (+ (+ 0         (+ 0         (+ 0        (+ 0        (+ (cc 7 0) (cc 6 1)             )))))              (+ (+ 0        (+ 0        (+ 0        (+ (cc 3 0) (cc 2 1)             ))))              (+ (+ 0        1       ) 0        )))             (+ (+ (+ 0        1       ) 0        ) 0        )) 0         ) 0         )
; (+ (+ (+ (+ (+ 0         (+ 0         (+ 0        (+ 0        (+ 0        (+ (cc 6 0) (cc 5 1)))))))              (+ (+ 0        (+ 0        (+ 0        (+ 0        (+ (cc 2 0) (cc 1 1))))))              (+ (+ 0        1       ) 0        )))             (+ (+ (+ 0        1       ) 0        ) 0        )) 0         ) 0         )
; …

;;; Exercise 1.15

(define (counter f)
  (define x 0)
  (define (result n)
    (set! x (+ 1 x))
    (pretty-print x)
    (f n))
  (define (reset-count)
    (set! x 0))
  (cons result reset-count))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define counter-reset-pair (counter p))
(define p-counter (car counter-reset-pair))
(define reset-count (cdr counter-reset-pair))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p-counter (sine (/ angle 3.0)))))

; > (reset-count)
; > (sine 12.15)
; 1
; 2
; 3
; 4
; 5
; -0.39980345741334

; The sine function takes on the order of the same amount of space and
; time, since all the calls to p are queued on the stack until the angle
; is reduced to an approximatable value. Because of the repeated division,
; the sine function calls itself O(lg a) times before the angle is
; sufficiently reduced to terminate the recursion.
 
 
;;; Exercise 1.16

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((< n 0) (error "can't handle so much negativity"))
          ((= n 0) a)
          ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))
  (fast-expt-iter b n 1))
  
;;; Exercise 1.17

(define (double n) (+ n n))
(define (halve n)
  (cond
    ((even? n) (/ n 2))
    (else (error "can only halve even numbers"))))
    
(define (mult n m)
  (cond ((or (= n 0) (= m 0)) 0)
        ((= n 1) m)
        ((= m 1) n)
        ((even? n) (mult (halve n) (double m)))
        (else (+ m (mult (- n 1) m)))))
    
;;; Exercise 1.18

(define (fast-mult n m)
  (define (fast-mult-iter n m acc)
    (cond ((= n 0) acc)
          ((even? n) (fast-mult-iter (halve n) (double m) acc))
          (else (fast-mult-iter (- n 1) m (+ m acc)))))
  (fast-mult-iter n m 0))
        
;;; Exercise 1.19

; a_1 ⟵ b_0·q + a_0·q + a_0·p and b_1 ⟵ b_0·p + a_0·q
; a_2 ⟵ b_1·q + a_1·q + a_1·p and b_2 ⟵ b_1·p + a_1·q
; substituting:
; a_2 ⟵ (b_0·p + a_0·q)·q + (b_0·q + a_0·q + a_0·p)·q + (b_0·q + a_0·q + a_0·p)·p and
; b_2 ⟵ (b_0·p + a_0·q)·p + (b_0·q + a_0·q + a_0·p)·q
; multiplying out:
; a_2 ⟵ b_0·p·q + a_0·q^2 + b_0·q^2 + a_0·q^2 + a_0·p·q + b_0·q·p + a_0·q·p + a_0·p^2 and
; b_2 ⟵ b_0·p^2 + a_0·q·p + b_0·q^2 + a_0·q^2 + a_0·p·q
; rearranging terms:
; a_2 ⟵ b_0·p·q + b_0·q^2 + b_0·q·p + a_0·q^2 + a_0·q^2 + a_0·p·q + a_0·q·p + a_0·p^2 and
; b_2 ⟵ b_0·p^2 + b_0·q^2 + a_0·q·p + a_0·q^2 + a_0·p·q
; grouping:
; a_2 ⟵ b_0·(2·p·q + q^2) + a_0·(2·q^2 + 2·p·q + p^2) and [line A]
; b_2 ⟵ b_0·(p^2 + q^2) + a_0·(2·p·q + q^2)
; 
; Pattern matching in the transformation for b_2, let:
; p' = p^2 + q^2 and q' = 2·p·q + q^2
; The form of the transformation of a can be rewritten as:
; a_1 ⟵ b_0·q + a_0·(q + p)
; So we need the coefficient of a_0 in line A to be p' + q'.
; Note that p' + q' = 2·q^2 + 2·p·q + p^2 as required.

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal order evaluation. * indicates remainder operations actually executed.
; (gcd 206 40)
; (if (= 40 0) 206 (gcd 40 (remainder 206 40))) ; we can't expand further because of the if special form
; (if #f 206 (gcd 40 (remainder 206 40)))
; (gcd 40 (remainder 206 40))
; (if (= (remainder* 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= 6 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if #f 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; (if (= (remainder 40 (remainder* 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= (remainder* 40 6) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= 4 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if #f (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if (= (remainder (remainder* 206 40) (remainder 40 (remainder* 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if (= (remainder 6 (remainder* 40 6)) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if (= (remainder* 6 4) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if (= 2 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if #f (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= (remainder (remainder 40 (remainder* 206 40)) (remainder (remainder* 206 40) (remainder 40 (remainder* 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if (= (remainder (remainder* 40 6) (remainder 6 (remainder* 40 6))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if (= (remainder 4 (remainder* 6 4)) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if (= (remainder* 4 2) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if (= 0 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if #t (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (remainder (remainder* 206 40) (remainder 40 (remainder* 206 40)))
; (remainder 6 (remainder* 40 6))
; (remainder* 6 4)
; 2
; ---> With normal order evaluation, remainder is evaluated 18 times.
; 
; (if (= b 0) a (gcd b (remainder a b)))
; 
; Applicative order evaluation.
; (gcd 206 40)
; (if (= 40 0) 206 (gcd 40 (remainder 206 40))) ; we can't expand further because of the if special form
; (if #f 206 (gcd 40 (remainder 206 40)))
; (gcd 40 (remainder* 206 40))
; (gcd 40 6)
; (if (= 6 0) 40 (gcd 6 (remainder 40 6)))
; (if #f 40 (gcd 6 (remainder 40 6)))
; (gcd 6 (remainder* 40 6))
; (gcd 6 4)
; (if (= 4 0) 6 (gcd 4 (remainder 6 4)))
; (if #f 6 (gcd 4 (remainder 6 4)))
; (gcd 4 (remainder* 6 4))
; (gcd 4 2)
; (if (= 2 0) 4 (gcd 2 (remainder 4 2)))
; (if #f 4 (gcd 2 (remainder 4 2)))
; (gcd 2 (remainder* 4 2))
; (gcd 2 0)
; (if (= 0 0) 2 (gcd 0 (remainder 2 0)))
; (if #t 2 (gcd 0 (remainder 2 0)))
; 2
; ---> With applicative order evaluation, remainder is evaluated 4 times.

;;; Exercise 1.21

(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))
; (define (next n) (+ n 1))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

; > (smallest-divisor 199)
; 199
; > (smallest-divisor 1999)
; 1999
; > (smallest-divisor 19999)
; 7

;;; Exercise 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

; This is modified from the version given in the text. It returns a pair
; indicating whether the argument is prime and the elapsed time for the
; test **even if the number is not prime**. It also takes the function used 
; for testing primality to make exercise 1.24 easier.
(define (timed-prime-test tester n)
  (start-prime-test tester n (current-inexact-milliseconds)))
(define (start-prime-test tester n start-time)
  (define (elapsed) (- (current-inexact-milliseconds) start-time))
  (if (tester n)
      (let ((elapsed-time (elapsed)))
        (report-prime n elapsed-time)
        (cons #t elapsed-time))
      (cons #f (elapsed))))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (first-three-primes n)
  (first-three-primes-f prime? n))
(define (first-three-primes-f tester n)
  (define (helper n count total-time)
    (if (= count 3)
        total-time
        (let ((result (timed-prime-test tester n)))
          (helper (+ n 2) 
                  (if (car result)
                      (+ count 1)
                      count)
                  (+ total-time (cdr result))))))
  (helper (if (even? n) (+ n 1) n) 0 0))

; Results using the original definition of smallest-divisor above.
;
; > (first-three-primes 1000)
; 1009 *** 0.003173828125
; 1013 *** 0.001953125
; 1019 *** 0.001953125
; 0.014892578125
; > (first-three-primes 10000)
; 10007 *** 0.008056640625
; 10009 *** 0.009033203125
; 10037 *** 0.0078125
; 0.050048828125
; > (first-three-primes 100000)
; 100003 *** 0.02587890625
; 100019 *** 0.025146484375
; 100043 *** 0.02392578125
; 0.126220703125
; > (first-three-primes 1000000)
; 1000003 *** 0.0771484375
; 1000033 *** 0.075927734375
; 1000037 *** 0.075927734375
; 0.2763671875
; > (first-three-primes 10000000)
; 10000019 *** 0.239990234375
; 10000079 *** 0.239013671875
; 10000103 *** 0.240966796875
; 1.15283203125
; > (first-three-primes 100000000)
; 100000007 *** 1.004150390625
; 100000037 *** 1.5390625
; 100000039 *** 1.910888671875
; 4.93701171875

; n     time (ms)  (sqrt n)
; 10^3  0.015      31.622
; 10^4  0.050      100
; 10^5  0.126      316.227
; 10^6  0.276      1000
; 10^7  1.153      3162.278
; 10^8  4.937      10000
; 
; > (/ 31.622 0.015)
; 2108.133333333333
; > (/ 100 .050)
; 2000.0
; > (/ 316.227 0.126)
; 2509.738095238095
; > (/ 1000 .276)
; 3623.188405797101 <--- surprising
; > (/ 3162.278 1.153)
; 2742.6522116218557
; > (/ 10000 4.937)
; 2025.5215718047396

;;; Exercise 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

; Times are about twice as fast, except for the occasional outlier. I suspect
; garbage collection is messing with the times for large numbers.
; 
; > (first-three-primes 100000000)
; 100000007 *** 0.524169921875
; 100000037 *** 0.491943359375
; 100000039 *** 0.4951171875
; 1.703125
; > (first-three-primes 100000000)
; 100000007 *** 0.56591796875
; 100000037 *** 0.823974609375
; 100000039 *** 0.580810546875
; 2.18701171875
; > (first-three-primes 100000000)
; 100000007 *** 0.489990234375
; 100000037 *** 0.4921875
; 100000039 *** 0.489013671875
; 1.670166015625
; > (first-three-primes 100000000)
; 100000007 *** 0.489990234375
; 100000037 *** 0.597900390625
; 100000039 *** 1.0029296875
; 2.325927734375
; > (first-three-primes 100000000)
; 100000007 *** 0.491943359375
; 100000037 *** 0.494140625
; 100000039 *** 0.489013671875
; 1.6630859375

;;; Exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (fast-prime-generator times)
  (define (prime? n)
    (fast-prime? n times))
  prime?)

; > (first-three-primes-f (fast-prime-generator 6) 1000)
; 1009 *** 0.011962890625
; 1013 *** 0.010986328125
; 1019 *** 0.010986328125
; 0.0546875
; > (first-three-primes-f (fast-prime-generator 6) 10000)
; 10007 *** 0.013916015625
; 10009 *** 0.012939453125
; 10037 *** 0.011962890625
; 0.0810546875
; > (first-three-primes-f (fast-prime-generator 6) 100000)
; 100003 *** 0.01513671875
; 100019 *** 0.014892578125
; 100043 *** 0.014892578125
; 0.1005859375
; > (first-three-primes-f (fast-prime-generator 6) 1000000)
; 1000003 *** 0.01806640625
; 1000033 *** 0.016845703125
; 1000037 *** 0.01806640625
; 0.130126953125
; > (first-three-primes-f (fast-prime-generator 6) 10000000)
; 10000019 *** 0.02392578125
; 10000079 *** 0.02392578125
; 10000103 *** 0.02392578125
; 0.276611328125
; > (first-three-primes-f (fast-prime-generator 6) 100000000)
; 100000007 *** 0.02392578125
; 100000037 *** 0.02783203125
; 100000039 *** 0.030029296875
; 0.189697265625
;
; Apart from some GC hiccups, this seems to scale at O(lg n).


;;; Exercise 1.25

; Alyssa's use of remainder is going to be expensive. The version we're
; actually using take remainder as we go, to keep the results of the
; exponentiation small.

;;; Exercise 1.26

; Louis's expmod recurses twice, once for each argument of the
; multiplication, doubling the amount of work at each stage.

;;; Exercise 1.27

(define (fermat-test-2 n witness)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it witness))

(define (ex-1-27 n)
  (define (helper n challenge)
    (if (= challenge 1)
        #t
        (if (fermat-test-2 n challenge)
            (fermat-test-2 n (- challenge 1))
            #f)))
  (helper n (- n 1)))
  
; “The smallest few [Carmichael numbers] are 561, 1105, 1729, 2465,
; 2821, and 6601”
; > (ex-1-27 10)
; #f
; > (ex-1-27 13)
; #t
; > (ex-1-27 19)
; #t
; > (ex-1-27 561)
; #t
; > (ex-1-27 1105)
; #t
; > (ex-1-27 1729)
; #t
; > (ex-1-27 2465)
; #t
; > (ex-1-27 2821)
; #t
; > (ex-1-27 6601)
; #t

;;; Exercise 1.28
; Not even going to attempt to parse that problem description.
