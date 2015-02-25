#lang racket

(require "exercise-1-33.rkt")

;;; Exercise 2.17

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))
      
(last-pair (list 23 72 149 34))

;;; Exercise 2.18

(define (reverse lst)
  (define (reverse-helper rest accum)
    (if (null? rest)
        accum
        (reverse-helper (cdr rest) (cons (car rest) accum))))
  (reverse-helper lst '()))

(reverse (list 1 4 9 16 25))

;;; Reverse 2.19

(define no-more? null?)

(define first-denomination car)

(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

;;; Reverse 2.20

(define (same-parity head . lst)
  (define (same-parity a b)
    (eq? (even? a) (even? b)))
  (define (same-parity-helper lst accum)
    (if (null? lst)
        accum
        (if (same-parity head (car lst))
            (same-parity-helper (cdr lst) (cons (car lst) accum))
            (same-parity-helper (cdr lst) accum))))
  (reverse (same-parity-helper lst (list head))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;;; Reverse 2.21

(define (square n) (* n n))

(define (square-list-recur items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-recur (cdr items)))))

(square-list-recur (list 1 2 3 4 5))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4 5))

;;; Exercise 2.22

; With car and cdr, we unpack a list from the beginning. Louis's iterative solution builds the accumulate by cons-ing values onto the beginning of an initially empty accumulator. Thus, the square of the first item becomes the last item of the result; the square of the second item becomes the second to last item of the result; and so on.

; Switching the arguments doesn't work, because it forms nested cons cells, not chained ones.

;;; Exercise 2.23

(define (for-each f lst)
  (map (lambda (n) (f n)) lst)
  #t)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;;; Exercise 2.24

(list 1 (list 2 (list 3 4)))
; ==> '(1 (2 (3 4)))

; See exercise-2-24.graffle for diagrams.

;;; Exercise 2.25

; Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(define list-2-25-a '(1 3 (5 7) 9))
(define list-2-25-b '((7)))
(define list-2-25-c '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list-2-25-a)))))
(car (car list-2-25-b))
(cadr (cadr (cadr (cadr (cadr (cadr list-2-25-c))))))

;;; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; ==> '(1 2 3 4 5 6)
(cons x y) ; ==> '((1 2 3) 4 5 6)
(list x y) ; ==> '((1 2 3) (4 5 6))

;;; Exercise 2.27

(define list-2-27 (list (list 1 2) (list 3 4)))

(define (deep-reverse arg)
  (define (deep-reverse-helper arg) 
    (if (list? arg)
      (deep-reverse-iter arg '())
      arg))
  (define (deep-reverse-iter lst accum)
    (if (null? lst)
        accum
        (deep-reverse-iter 
          (cdr lst)
          (cons (deep-reverse-helper (car lst)) accum))))
  (deep-reverse-helper arg))

(reverse list-2-27)
(deep-reverse list-2-27)

;;; Exercise 2.28

(define list-2-28 (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define (fringe-helper maybe-tree accum)
    (cond ((not (list? maybe-tree))
            (list maybe-tree))
          ((null? maybe-tree)
            accum)
          (else 
            (append (fringe-helper (car maybe-tree) '())
                    (fringe-helper (cdr maybe-tree) accum)))))
  (fringe-helper tree '()))

(fringe list-2-28)

;;; Exercise 2.29

; (define (make-mobile left right)
;   (list left right))
; (define (make-branch len structure)
;   (list len structure))
; (define (mobile? structure) ;; added for part (d):
;   (list? structure))

;        ___left.length____|___right.length_______
;       |                                         |
; left.structure                          right.structure

; (a)

; (define (left-branch mobile)
;   (car mobile))
; (define (right-branch mobile)
;   (car (cdr mobile)))
; (define (branch-length branch)
;   (car branch))
; (define (branch-structure branch)
;   (car (cdr branch)))

; (b)

(define (total-weight structure)
  (if (mobile? structure)
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))
      structure))

; (total-weight mobile-2-29-1)
; (total-weight mobile-2-29-2)

; (c)

(define (balanced? structure)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (mobile? structure)
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (and (balanced? (branch-structure left))
             (balanced? (branch-structure right))
             (eq? (branch-torque left)
                  (branch-torque right))))
      #t))

; (d)

(define (make-mobile left right)
  (cons left right))
(define (make-branch len structure)
  (cons len structure))
(define (mobile? structure)
  (pair? structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

; To make total-weight and balanced? work, I had to add a mobile? predicate and use it in the definitions of total-weight and balanced? I also had to make sure the accessor procedures were defined for the new represenation.

(define mobile-2-29-1
  (make-mobile
    (make-branch 10
                 (make-mobile
                   (make-branch 5 10)
                   (make-branch 2 12)))
    (make-branch 8 8)))

(define mobile-2-29-2
  (make-mobile (make-branch 5 2) (make-branch 10 1)))

;;; Exercise 2.30

(define (square-tree-recur tree)
  (define (square-branches lst accum)
    (if (null? lst)
        accum
        (square-branches (cdr lst) 
                         (cons (square-tree-recur (car lst))
                               accum))))
  (if (list? tree)
      (reverse (square-branches tree '()))
      (square tree)))

(define (square-tree-map tree)
  (if (list? tree)
      (map square-tree-map tree)
      (square tree)))

(square-tree-recur
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;;; Exercise 2.31

(define (tree-map f tree)
  (define (curried-tree-map subtree)
    (tree-map f subtree))
  (if (list? tree)
      (map curried-tree-map tree)
      (f tree)))

(define (square-tree tree) (tree-map square tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;;; Exercise 2.32

(define (subsets s)
  (define (cons-car-f lst)
    (lambda (other) (cons (car lst) other)))
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest (map (cons-car-f s) rest)))))

; In the base case, the set of all subsets of the empty set is the set containing the empty set.
; In the recursive case, rest is the set of all subsets of the set formed by removing the first element from s. The result is all the elements of rest unioned with the set of the sets formed by adding the first element of s to each element of rest.

;;; Exercise 2.33

(define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (map-foldr p sequence)
  (accumulate (lambda (x accum) (cons (p x) accum)) '() sequence))
(define (append-foldr seq1 seq2) 
  (accumulate cons seq2 seq1))
(define (length-foldr sequence)
  (define (counter x accum)
    (+ 1 accum))
  (accumulate counter 0 sequence))
  
;;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;; Exercise 2.35

(define (enumerate-tree tree)
  (cond ((null? tree) '())
         ((not (pair? tree)) (list tree))
         (else (append (enumerate-tree (car tree))
                       (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves '(1))
(count-leaves '(1 2 (3 4)))

;;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;; Exercise 2.37

(define matrix-2-37 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define vector-2-37 '(2 4 6 8))

(define (dot-product vector-a vector-b)
  (accumulate + 0 (map * vector-a vector-b)))
(dot-product vector-2-37 vector-2-37)

(define (matrix-*-vector matrix vector)
  (map (lambda (matrix-row) 
         (dot-product matrix-row vector))
       matrix))
(matrix-*-vector matrix-2-37 vector-2-37)

(define (transpose matrix)
  (accumulate-n cons '() matrix))

(transpose '((1 2) (3 4)))
(transpose matrix-2-37)
  
(define (matrix-*-matrix matrix-a matrix-b)
  (let ((cols (transpose matrix-b)))
    (map (lambda (matrix-a-row) (matrix-*-vector cols matrix-a-row))
         matrix-a)))

(matrix-*-matrix '((1 2)) '((1) (2)))
(matrix-*-matrix '((1) (2)) '((1 2)))
(matrix-*-matrix matrix-2-37 (transpose matrix-2-37))
(matrix-*-matrix (transpose matrix-2-37) matrix-2-37)
(matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((1 4 7) (2 5 8) (3 6 9)))

;;; Exercise 2.38

(define (fold-left-book op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-left-racket op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))


(foldr / 1 '(1 2 3))
(fold-left-book / 1 '(1 2 3))
(fold-left-racket / 1 '(1 2 3))
(foldl / 1 '(1 2 3))

(foldr list '() '(1 2 3))
(fold-left-book list '() '(1 2 3))
(fold-left-racket list '() '(1 2 3))
(foldl list '() '(1 2 3))

(foldr + 0 '(1 2 3))
(fold-left-book + 0 '(1 2 3))
(fold-left-racket + 0 '(1 2 3))
(foldl + 0 '(1 2 3))

; Commutative functions can be folded either direction.

;;; Exercise 2.39

(define (reverse-with-fold-right sequence)
  (accumulate 
    (lambda (element accum) 
      (append accum (list element))) 
    '()
    sequence))
(define (reverse-with-fold-left sequence)
  (fold-left-book 
    (lambda (accum element) (cons element accum)) 
    '()
    sequence))
(define (reverse-with-foldl sequence)
  (foldl 
    (lambda (element accum) (cons element accum)) 
    '()
    sequence))


(reverse-with-fold-right '(1 2 3))
(reverse-with-fold-left '(1 2 3))
(reverse-with-foldl '(1 2 3))

;;; Exercise 2.40

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(flatmap (lambda (x) (list x)) '(10 20 30))
(flatmap (lambda (x) (list x x)) '(10 20 30))
(flatmap (lambda (x) (list x (* 2 x) (* 3 x))) '(10 20 30))

(define (permutations s)
  (if (null? s)                    ; empty set?
      '(())                        ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations '(1))
(permutations '(1 2))
(permutations '(1 2 3))

(display "unique-pairs")
(newline)
(define (unique-pairs n)
  (define (n-through-m n m)
    (range n (+ 1 m)))
  (define one-through-n
    (n-through-m 1 n))
  (if (< n 2)
      (error "argument must be greater than 1: " n)
      (flatmap 
        (lambda (i)
          (map 
            (lambda (j) (list j i))
            (n-through-m 1 (- i 1))))
        one-through-n)))
        
(unique-pairs 3)

(display "prime-sum-pairs")
(newline)
(define (prime-sum-pairs n)
  (filter (lambda (a-pair) (prime? (apply + a-pair)))
          (unique-pairs n)))
(prime-sum-pairs 6)

;;; Exercise 2.41

; TODO: write unique-triples using unique-pairs to get pairs to which to cons a first elemetn

(display "exercise-2-41")
(newline)

(define (unique-triples n)
  (if (< n 3)
      (error "n must be greater than 2: " n)
      (flatmap 
        (lambda (i)
          (map
            (lambda (j-k) (append j-k (list i)))
            (unique-pairs (- i 1))))
        (range 3 (+ n 1)))))

; (unique-triples 10)

(define (exercise-2-41 n s)
  (define (matches triple)
    (= s (apply + triple)))
  (filter matches (unique-triples n)))

(exercise-2-41 10 7)
(exercise-2-41 10 12)
