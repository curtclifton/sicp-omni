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
        
(fast-mult 2 3)



  


