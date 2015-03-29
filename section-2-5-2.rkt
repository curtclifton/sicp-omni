#lang racket

(require "helpers.rkt")
(require "section-2-put-get.rkt")

;; original versions of attach-tag, type-tag, and contents from text
; (define (attach-tag type-tag contents)
;   (cons type-tag contents))
; (define (type-tag datum) 
;   (if (pair? datum)
;       (car datum)
;       (error "Bad tagged datum: TYPE-TAG" datum))) 
; (define (contents datum)
;   (if (pair? datum) (cdr datum)
;       (error "Bad tagged datum: CONTENTS" datum)))

;; updated versions of attach-tag, type-tag, and contents for exercise 2.78
; this seems fairly dreadful, since the 'scheme-number tag was encapsulated in install-scheme-number-package before, but is now coupled
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) 
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x))) 'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package) 
  ;; internal procedures
  (define (numer x) (car x)) 
  (define (denom x) (cdr x)) 
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))) 
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x)) 
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y)))) 
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y)))) 
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y)))) 
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x)) 
  (put 'real-part '(rectangular) real-part) 
  (put 'imag-part '(rectangular) imag-part) 
  (put 'magnitude '(rectangular) magnitude) 
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) 
    (cons r a))
  (define (real-part z) 
    (* (magnitude z) (cos (angle z)))) 
  (define (imag-part z) 
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x)) 
  (put 'real-part '(polar) real-part) 
  (put 'imag-part '(polar) imag-part) 
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y)))) 
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages 
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)) 
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)) 
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2)))) 
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2)))) 
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  ;; -------------------------------------------------------------
  ;; added in exercise 2.78
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;; -------------------------------------------------------------
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-complex-from-real-imag x y) 
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) 
  ((get 'make-from-mag-ang 'complex) r a))


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;;; Exercise 2.77

(println "exercise 2.77")

(define figure-2-24 (make-complex-from-real-imag 3 4))
(magnitude figure-2-24)

; The first application of magnitude routes through apply-generic and strips the 'complex tag. It then looks up the top-level definition of magnitude (installed by the complex package) and applies it. That hits apply-generic again, strips off the rectangular tag and looks up the version of magnitude defined in the rectangular package. That one does the expected math and returns the answer.

;;; Exercise 2.78

; see implementation above

(println "exercise 2.78")

(make-scheme-number 10)
(make-rational 2 3)

;;; Exercise 2.79

(println "exercise 2.79")

;; This is pretty horrible. We have to have internal knowledge of the encoding of all the number types in order to implement this. We need their tags to install the new procedures. We need the structure of rationals to do the comparison. (For the later, we could install top-level numer and denom procedures, but we'd have to go back and edit the original definitions to do that.)
(define (install-equ)
  ;; internal procedures
  (define (equ-number? n m)
    (if (and (exact? n) (exact? m))
        (= n m)
        (< (abs (- n m)) 0.000001)))
  (define (equ-rational? n m)
    (and (equ? (car n) (car m))
         (equ? (cdr n) (cdr m))))
  (define (equ-complex? a b)
    (and (equ? (real-part a) (real-part b))
         (equ? (imag-part a) (imag-part b))))
  ;; installation
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equ-number? x y)))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rational? x y)))
  (put 'equ? '(complex complex)
       (lambda (x y) (equ-complex? x y)))
  'done)
(install-equ)
(define (equ? x y)
  (apply-generic 'equ? x y))

(equ? 1 1)
(equ? 1 2)
(equ? (make-rational 1 2) (make-rational 2 4))
(equ? (make-rational 1 2) (make-rational 1 3))
(equ? (make-complex-from-real-imag 0 1) (make-complex-from-mag-ang 1 (/ pi 2)))
(equ? (make-complex-from-real-imag 1 0) (make-complex-from-mag-ang 1 0))
(equ? (make-complex-from-real-imag 0 1) (make-complex-from-mag-ang 1 0))

;;; Exercise 2.80

(println "exercise 2.80")

;; Re-tagging the values and dispatching to equ? so we can take advantage of the recursion on types there.
(define (install-=zero)
  ;; installation
  (put '=zero? '(scheme-number)
       (lambda (x) (equ? x 0)))
  (put '=zero? '(rational)
       (lambda (x) (equ? (attach-tag 'rational x) (make-rational 0 1))))
  (put '=zero? '(complex)
       (lambda (x) (equ? (attach-tag 'complex x) (make-complex-from-real-imag 0 0))))
  'done)
(install-=zero)
(define (=zero? x)
  (apply-generic '=zero? x))

(println "should be true:")
(=zero? 0)
(=zero? (make-rational 0 10))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-mag-ang 0 pi))

(println "should be false:")
(=zero? 1)
(=zero? (make-rational 1 10))
(=zero? (make-complex-from-real-imag 0 1))
(=zero? (make-complex-from-real-imag 1 0))
(=zero? (make-complex-from-mag-ang 1 pi))

