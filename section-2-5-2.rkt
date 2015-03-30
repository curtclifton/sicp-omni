#lang racket

(require "helpers.rkt")
(require "section-2-put-get.rkt")

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

;; Text's version, with exercise 2.82c changes:
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags))) 
;       (if proc
;           (apply proc (map contents args))
; ;           (if (= (length args) 2) ; ← text's version
;           (if (and (= (length args) 2) (not (eq? (first type-tags) (second type-tags)))) ; ← ex. 2.82c
;               (let ((type1 (car type-tags))
;                     (type2 (cadr type-tags))
;                     (a1 (car args))
;                     (a2 (cadr args)))
;                 (let ((t1->t2 (get-coercion type1 type2))
;                       (t2->t1 (get-coercion type2 type1)))
;                   (cond (t1->t2
;                          (apply-generic op (t1->t2 a1) a2))
;                         (t2->t1
;                          (apply-generic op a1 (t2->t1 a2)))
;                         (else
;                          (error "No method for these types even with coercion"
;                                 (list op type-tags))))))
;               (error "No method for these types"
;                      (list op type-tags)))))))

(define (install-supertype)
  (put 'supertype 'scheme-number 'rational)
  (put 'supertype 'rational 'complex)
  'done
  )
(install-supertype)
(define (<: type1 type2)
  (define supertype1 (get 'supertype type1))
  (cond ((eq? type1 type2) #t)
        (supertype1 (<: supertype1 type2))
        (else #f)))

(define (raise-to-type arg type)
  (if (eq? (type-tag arg) type)
      arg
      (raise-to-type (raise arg) type)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (arg1 (car args))
                    (arg2 (cadr args)))
                (cond ((eq? type1 type2) (error "no method for pair of " type1))
                      ((<: type1 type2) (apply-generic op (raise-to-type arg1 type2) arg2))
                      ((<: type2 type1) (apply-generic op arg1 (raise-to-type arg2 type1)))
                      (else (error "incomparable types? wat?" type-tags)))
              )
              (error "No method for these types"
                     (list op type-tags)))))))

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
       (lambda (r a) (tag (make-from-mag-ang r a)))) 
  'done)

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

(define-values (put-coercion get-coercion) (make-table))

;;; Exercise 2.82

;; (a) The recursion, it is infinite.
;; (b) apply-generic, as implemented in the text, only tries to coerce arguments of the same type if there is no procedure for a pair of the original types. That's not a problem for the current implementation, though it's about to become one when we expect operations to be inherited.
;; (c) see above

;;; Exercise 2.83

(println "exercise 2.83")

;; To keep this a work system, I'm taking the liberty of omitting a separate 'real' type and just including 'complex' the issues are the same.
(define (install-raise)
  (define (scheme-number->rational n)
    (make-rational n 1))
  (define (rational->complex n)
    (make-complex-from-real-imag (/ (car n) (cdr n)) 0))  
  ;; install raise procedures
  (put 'raise '(scheme-number) scheme-number->rational)
  (put 'raise '(rational) rational->complex)
  'done
  )
(define (raise n)
  (apply-generic 'raise n))
(install-raise)

(raise 1)
(raise (raise 1))
(raise (make-rational 1 2))

;;; Exercise 2.84

(println "exercise 2.84")

;; See definition of <: and updated definition of apply-generic above.
(println "should be true:")
(<: 'scheme-number 'scheme-number)
(<: 'scheme-number 'rational)
(<: 'scheme-number 'complex)
(<: 'rational 'rational)
(<: 'rational 'complex)
(<: 'complex 'complex)
(println "should be false:")
(<: 'rational 'scheme-number)
(<: 'complex 'scheme-number)
(<: 'complex 'rational)

(println "Let's do math!")
(define (test-add n m)
  (println n " + " m " = " (add n m)))
(define test-sn (make-scheme-number 1))
(define test-rat (make-rational 2 3))
(define test-cmplx (make-complex-from-real-imag 4 5))
(test-add test-sn test-sn)
(test-add test-sn test-rat)
(test-add test-sn test-cmplx)
(test-add test-rat test-sn)
(test-add test-rat test-rat)
(test-add test-rat test-cmplx)
(test-add test-cmplx test-sn)
(test-add test-cmplx test-rat)
(test-add test-cmplx test-cmplx)

;;; Exercise 2.85

(println "exercise 2.85")

;; Brought over from exercise 2.79
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

;; This is pretty lame. Since I chose not to implement integer and real, there isn't much interesting to do here. I decided to add some gratuitous rounding so the we only simplify to integers or ratios of integers.
(define (install-project)
  (define (make-exact n)
    (inexact->exact (round n)))
  (define (project-complex n)
    (make-rational (make-exact (real-part n)) 1))
  (define (project-rational n)
    (make-scheme-number (make-exact (/ (car n) (cdr n)))))
  (put 'project '(complex) project-complex)
  (put 'project '(rational) project-rational)
  'done
  )
(install-project)
(define (project n)
  (apply-generic 'project n))

(define (drop n)
  (define original-type (type-tag n))
  (define (drop-further last)
    (let ((projector (get 'project (list (type-tag last)))))
      (if (not projector)
          last
          (let ((next (projector (contents last))))
            (if (equ? (raise-to-type next original-type) n)
                (drop-further next)
                last)))))
  (drop-further n))

;; I'm taking a pass on modifying apply-generic yet again. It's a simple matter of wrapping the whole body of the procedure in an application of drop, since drop is a no-op when the argument isn't projectable.

;;; Exercise 2.86

(println "exercise 2.86")

;; I'm gonna skip this one. It's just some fairly tedious work to define generic versions of all the functions used within install-polar and install-rectangular. We'd also need to change the applications of +, -, *, and / in install-complex to use add, sub, mul, and div instead.
