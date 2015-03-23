#lang racket

(require "section-2-put-get.rkt")

(define (variable? expr) (symbol? expr))
(define (same-variable? expr1 expr2)
  (equal? expr1 expr2))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0)) 
        (else ((get 'deriv (operator expr))
               (operands expr) var))))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

;;; Exercise 2.73

; (a) The definition of deriv above is using 'deriv as the operation to be looked up in the table and the operator of the expression as the type. The top-level deriv procedure is not installed in the table, but only resorts to the table for differentiating compound expressions. Simple expressions like a number or a variable reference have no operator for conducting the lookup in the table. Note, however, that we could modify the top-level deriv procedure to be a bit more clever about calculating the operator to be used for lookup. For example, we could use 'number as a pseudo-operator for number expressions and 'varref as a pseudo-operator for variable references. Then we could install procedures for differentiating numbers and varrefs in the table as well.

; (b) It would be cleaner to grab the make-sum and make-product code and related helpers from section 2.3, but we're already making assumptions about the representation in the given code for this exercise. I'm going to assume binary operations while I'm at it. When in Rome…

(define (install-symbolic-differentiation)
  ;; local definitions
  (define (deriv-sum operands var)
    (cons '+ (map (lambda (expr) (deriv expr var)) operands)))
  (define (deriv-product operands var)
    (list '+
          (list '* (car operands) (deriv (cadr operands) var))
          (list '* (deriv (car operands) var) (cadr operands))))
  ;; installation
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  )

(install-symbolic-differentiation)

; (c)

(define (install-exponent-rule)
  (define (deriv-exp operands var)
    (define base (car operands))
    (define exponent (cadr operands))
    (list '* exponent
             (list '* (list '^ base (- exponent 1)) (deriv base var))))
  (put 'deriv '^ deriv-exp)
  )

(install-exponent-rule)

; (d) We would just have to reverse the order of our first two arguments to put in parts b and c.


