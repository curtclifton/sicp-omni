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
