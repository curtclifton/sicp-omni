#lang racket

(define (println . xs)
  (define (print items)
    (if (null? items)
        (newline)
        (begin
          (display (car items))
          (print (cdr items)))))
  (print xs))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) (list left
                                          right
                                          (append (symbols left) (symbols right))
                                          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree) (if (leaf? tree)
                          (weight-leaf tree)
                          (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (adjoin-set x set) (cond ((null? set) (list x))
                                 ((< (weight x) (weight (car set))) (cons x set)) (else (cons (car set)
                                                                                              (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs) (if (null? pairs)
                                  '()
                                  (let ((pair (car pairs)))
                                    (adjoin-set (make-leaf (car pair)
                                                           (cadr pair))
                                                ; symbol
                                                ; frequency
                                                (make-leaf-set (cdr pairs))))))


;;;Exercise 2.67

(println "exercise 2.67")

(define sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                     (make-leaf 'B 2)
                                     (make-code-tree
                                      (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define decoded-message (decode sample-message sample-tree))
(println "(decode sample-message sample-tree) ==> " decoded-message)

;;; Exercise 2.68

(define (encodes-symbol? sym tree)
  (memq sym (symbols tree)))
  
(define (encode-symbol sym tree)
  (define (walk-subtree subtree accum)
    (if (leaf? subtree)
        (reverse accum)
        (let ((lb (left-branch subtree))
              (rb (right-branch subtree)))
          (if (encodes-symbol? sym lb)
              (walk-subtree lb (cons 0 accum))
              (walk-subtree rb (cons 1 accum))))))
  (if (encodes-symbol? sym tree)
      (walk-subtree tree '())
      (error "symbol not encoded by tree: " sym)))

(define (encode message tree) (if (null? message)
                                  '()
                                  (append (encode-symbol (car message) tree)
                                          (encode (cdr message) tree))))

(define encoded-message (encode decoded-message sample-tree))
(println "(encode decoded-message sample-tree) ==> " encoded-message)
(println "match? " (equal? sample-message encoded-message))

;;; Exercise 2.69

; (define (generate-huffman-tree pairs) (successive-merge (make-leaf-set pairs)))

;;; Exercise 2.70

; A 2 GET2 SHA3 WAH1 BOOM1 JOB2 NA16 YIP9
; 
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
