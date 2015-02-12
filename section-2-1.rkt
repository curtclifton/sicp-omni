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

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (let ((mx (average x1 x2))
          (my (average y1 y2)))
      (make-point mx my))))

(let ((segment (make-segment (make-point 2 3) (make-point 1 1))))
  (midpoint-segment segment))
  
;;; Exercise 2.3

; one representation, arbitrary corner points, calculate in accessors
; (define (extract-and-choose chooser selector a b)
;   (chooser (selector a) (selector b)))
; (define (make-rect point-a point-b) 
;   (cons point-a point-b))
; (define (rect-min-x rect)
;   (extract-and-choose min x-point (car rect) (cdr rect)))
; (define (rect-min-y rect)
;   (extract-and-choose min y-point (car rect) (cdr rect)))
; (define (rect-width rect)
;   (- (extract-and-choose max x-point (car rect) (cdr rect))      
;      (rect-min-x rect)))
; (define (rect-height rect) 
;   (- (extract-and-choose max y-point (car rect) (cdr rect))      
;      (rect-min-y rect)))
; (define test-rect (make-rect (make-point 0 0) (make-point 2 4)))

; another representation, corner, width and height
(define (make-rect origin width height) 
  (if (or (< width 0) (< height 0))
      (error "width and height must be non-negative")
      (cons origin (cons width height))))
(define (rect-min-x rect)
  (x-point (car rect)))
(define (rect-min-y rect)
  (y-point (car rect)))
(define (rect-width rect)
  (car (cdr rect)))
(define (rect-height rect) 
  (cdr (cdr rect)))
(define test-rect (make-rect (make-point 0 0) 2 4))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))
(define (rect-perimeter rect)
  (+ (rect-width rect) (rect-width rect) (rect-height rect) (rect-height rect)))

;;; Exercise 2.4

(define (cons-2-4 x y)
  (lambda (chooser-function) (chooser-function x y)))
  
(define (car-2-4 cons-cell)
  (cons-cell (lambda (first second) first)))
  
(define (cdr-2-4 cons-cell)
  (cons-cell (lambda (first second) second)))

(define test-cons-2-4 (cons-2-4 'a 'b))
; > (car-2-4 test-cons-2-4)
; 'a
; > (cdr-2-4 test-cons-2-4)
; 'b

;;; Exercise 2.5

(define (num-pair a b)
  (* (expt 2 a) (expt 3 b)))

(define (log-base-n n x) ; seems like this would be useful
  (/ (log x) (log n)))

(define (divides n m)
  (= 0 (remainder n m)))

(define (prime-something-helper n)
  (lambda (p)
    (define (helper-iter p-remains accum)
      (if (divides p-remains n)
          (helper-iter (/ p-remains n) (+ 1 accum))
          accum))
    (helper-iter p 0)))

(define (num-pair-first p)
  ((prime-something-helper 2) p))

(define (num-pair-second p)
  ((prime-something-helper 3) p))

(define pair-17-19 (num-pair 17 19))

;;; Exercise 2.6

(define zero (lambda (f) identity))
(define (succ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (succ zero)) ; expand on white board
(define two (succ one)) ; expand on white board

(define (unchurch church-num)
  ((church-num (lambda (x) (+ x 1))) 0))

