#lang racket

(define (println x)
  (display x)
  (newline))

(define empty-tree '())
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry)
  (make-tree entry empty-tree empty-tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Exercise 2.63

(println "Exercise 2.63")

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define test-trees
  (list
    (make-leaf 1)
    (make-tree 7
      (make-tree 3
        (make-leaf 1)
        (make-leaf 5))
      (make-tree 9
        empty-tree
        (make-leaf 11)))
    (make-tree 3
      (make-leaf 1)
      (make-tree 7
        (make-leaf 5)
        (make-tree 9
          empty-tree
          (make-leaf 11))))
    (make-tree 5
      (make-tree 3
        (make-leaf 1)
        empty-tree)
      (make-tree 9
        (make-leaf 7)
        (make-leaf 11)))
    ))

(map tree->list-1 test-trees)
(map tree->list-2 test-trees)
(define (all . xs)
  (define (all-list xs)
    (if (null? xs)
        #t
        (and (car xs) (all-list (cdr xs)))))
  (all-list xs))
(apply all (map (lambda (tree) (equal? (tree->list-1 tree)
                                       (tree->list-2 tree)))
                test-trees))

; tree->list-1 produces an in-order traversal in O(n^2), where n is the number of elements in the set. Because append traverses the lists for the left branches at each level, some nodes will be visited more than once. In the worst case, with a tree that has empty right branches at every level (e.g., the mirror of the tree in Figure 2.17), the leaf will be visited n - 1 times, its parent n - 2, and so on up to the root which is visited once.

; tree->list-2 also produces an in-order traversal of the tree, but does it in O(n). cons is the only list construction operation used and it is used once per entry.

;;; Exercise 2.64

(println "Exercise 2.64")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; converting this to let* so we don't need to nest lets to the 5th.
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts)
                                              right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

(list->tree '(1 3 5 7 9 11))

; (a) partial-tree calculates where the middle of its elt list is, then recursively applies partial-tree to the elements before the middle element and the elements after the middle element, constructing a tree from the three parts. By returning the "unused" elements of the processed list as the cdr of result, partial-tree can make produce the elements needed to build the right-subtree. In other words, this structure lets use avoid traversing any elements the input list more than once.
;
;        5
;   1         9
;     3     7   11 

; (b) make-tree is invoked exactly once for each element in the original list, as is cons. Thus, partial-tree is O(n).

;;; Exercise 2.65

(println "Exercise 2.65")

; (define (intersection-merge list1 list2)
;   (cond ((null? list1) '())
;         ((null? list2) '())
;         (else
;           (let ((x1 (car list1))
;                 (x2 (car list2))
;                 (r1 (cdr list1))
;                 (r2 (cdr list2)))
;             (cond ((= x1 x2) (cons x1 (intersection-merge r1 r2)))
;                   ((< x1 x2) (intersection-merge r1 list2))
;                   ((> x1 x2) (intersection-merge list1 r2)))))))

; (define (union-merge list1 list2)
;   (cond ((null? list1) list2)
;         ((null? list2) list1)
;         (else
;           (let ((x1 (car list1))
;                 (x2 (car list2))
;                 (r1 (cdr list1))
;                 (r2 (cdr list2)))
;             (cond ((= x1 x2) (cons x1 (union-merge r1 r2)))
;                   ((< x1 x2) (cons x1 (union-merge r1 list2)))
;                   ((> x1 x2) (cons x2 (union-merge list1 r2))))))))

(define (merger-gen empty-f equal-f smaller-f)
  (define (merger list1 list2)
    (cond ((null? list1) (empty-f list2))
          ((null? list2) (empty-f list1))
          (else
            (let ((x1 (car list1))
                  (x2 (car list2))
                  (r1 (cdr list1))
                  (r2 (cdr list2)))
              (cond ((= x1 x2) (append (equal-f x1) (merger r1 r2)))
                    ((< x1 x2) (append (smaller-f x1) (merger r1 list2)))
                    ((> x1 x2) (append (smaller-f x2) (merger list1 r2))))))))
  merger)
                    

; (define (intersection-set set1 set2)
;   (let ((list1 (tree->list-2 set1))
;         (list2 (tree->list-2 set2)))
;     (list->tree (intersection-merge list1 list2))))

; (define (union-set set1 set2)
;   (let ((list1 (tree->list-2 set1))
;         (list2 (tree->list-2 set2)))
;     (list->tree (union-merge list1 list2))))

(define (set-op-gen merger)
  (define (set-op set1 set2)
    (let ((list1 (tree->list-2 set1))
          (list2 (tree->list-2 set2)))
      (list->tree (merger list1 list2))))
  set-op)

(define intersection-set
  (set-op-gen
    (merger-gen (lambda (xs) '())
                (lambda (x) (list x))
                (lambda (x) '()))))

(define union-set
  (set-op-gen
    (merger-gen (lambda (xs) xs)
                (lambda (x) (list x))
                (lambda (x) (list x)))))

(define test-set1 
  (make-tree 2 
    (make-leaf 1)
    (make-tree 3
      empty-tree
      (make-leaf 4))))
(define test-set2
  (make-tree 4 
    (make-leaf 3)
    (make-tree 5
      empty-tree
      (make-leaf 6))))

(intersection-set test-set1 test-set2)
(union-set test-set1 test-set2)
