#lang racket

(define (println x)
  (display x)
  (newline))

;;; Exercise 2.53

(println "exercise 2.53")

(list 'a 'b 'c) ; guess: '(a b c)
(list (list 'george)) ; guess: '((george))
(cdr '((x1 x2) (y1 y2))) ; guess: '((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; guess: '(y1 y2)
(pair? (car '(a short list))) ; guess: #f
(memq 'red '((red shoes) (blue socks))) ; guess: #f, red isn't top-level

(memq 'red '(red shoes blue socks)) ; guess: '(red shoes blue socks)

;;; Exercise 2.54

(println "exercise 2.54")

(define (my-equal? expr1 expr2)
  (define (list-equal? list1 list2)
    (cond ((and (null? list1) (null? list2)) #t)
          ((null? list1) #f)
          ((null? list2) #f)
          ((not (my-equal? (car list1) (car list2))) #f)
          (else (list-equal? (cdr list1) (cdr list2)))))
  (cond ((and (list? expr1) (list? expr2)) (list-equal? expr1 expr2))
        ((and (symbol? expr1) (symbol? expr2)) (eq? expr1 expr2))
        (else #f)))

(my-equal? 'a 'a)
(my-equal? 'a 'b)
(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list) '(this (is a) list))

;;; Exercise 2.55

(println "exercise 2.55")

(car ''abracadabra)

; Expanding the single quotes per footnote 34 (sheesh), this is equivalent to (car (quote (quote abracadabra))), which is equivalent to (car (list 'quote 'abracadabra)).

;;; Section 2.3.2

(println "Section 2.3.2")

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (first-remover op-symbol)
  (define (remove-first terms)
    (let ((remaining-terms (cddr terms)))
      (if (= (length remaining-terms) 1)
          (car remaining-terms)
          (cons op-symbol remaining-terms))))
  remove-first)

(define (commutative-simplifier op op-symbol op-ident op-zero op-zero?)
  (define (simplify terms)
    (let ((non-number-terms 
            (filter (lambda (term) (not (number? term))) terms))
          (number-terms 
            (filter (lambda (term) (number? term)) terms)))
      (let ((folded-constants (apply op number-terms)))
        (cond ((op-zero? folded-constants) op-zero)
              ((= folded-constants op-ident)
               (cond ((= (length non-number-terms) 0) op-ident)
                     ((= (length non-number-terms) 1) (car non-number-terms))
                     (else (cons op-symbol non-number-terms))))
              (else
                (cond ((= (length non-number-terms) 0) folded-constants)
                      (else (cons op-symbol 
                                  (cons folded-constants
                                        non-number-terms)))))))))
  simplify)

(define (make-sum . as)
  ((commutative-simplifier + '+ 0 0 (lambda (x) #f)) as))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) 
  ((first-remover '+) s))

(define (make-product . ms)
  ((commutative-simplifier * '* 1 0 (lambda (x) (= x 0))) ms))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  ((first-remover '*) p))

(define (make-power b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))
(define (power? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((power? exp)
         (make-product (exponent exp)
                       (make-product (make-power (base exp) 
                                                 (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;;; Exercise 2.56

(println "Exercise 2.56")

; See definitions of make-piwer, power?, base, and exponent above, along with the power? case in deriv.

(deriv (make-power 'x 0) 'x)
(deriv (make-power 'x 1) 'x)
(deriv (make-power 'x 2) 'x)
(deriv (make-power 'x 3) 'x)

(define (second-deriv exp var)
  (deriv (deriv exp var) var))

(second-deriv (make-power 'x 0) 'x)
(second-deriv (make-power 'x 1) 'x)
(second-deriv (make-power 'x 2) 'x)
(second-deriv (make-power 'x 3) 'x)

;;; Exercise 2.57

(println "Exercise 2.57")

(println "sums")
(make-sum 0 'x 2 'y)
(make-sum 0 'x 2 3 'y)
(make-sum 0)
(make-sum 1)
(make-sum 'x)
(make-sum 'x 'y)
(make-sum 1 2 3 4)
(make-sum 1 2 3 4 'x)

(println "products")
(make-product 0 'x 2 'y)
(make-product 0 'x 2 3 'y)
(make-product 0)
(make-product 1)
(make-product 'x)
(make-product 'x 'y)
(make-product 1 2 3 4)
(make-product 1 2 3 4 'x)
