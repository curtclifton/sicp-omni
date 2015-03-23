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


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) 
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

(newline)
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

(newline)
(println "exercse 2.68")

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

(newline)
(println "exercse 2.69")

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) (error "empty leaf-set"))
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge
                (adjoin-set (make-code-tree (car leaf-set)
                                            (cadr leaf-set))
                            (cddr leaf-set))))))

(define (generate-huffman-tree pairs) (successive-merge (make-leaf-set pairs)))

(make-leaf-set '((a 1) (b 2) (c 1)))
(successive-merge (make-leaf-set '((a 1) (b 2) (c 1))))

;;; Exercise 2.70

(newline)
(println "exercse 2.70")

(define silhouettes-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(println "silhouettes-tree: " silhouettes-tree)

(println "GET A JOB: " (encode '(GET A JOB) silhouettes-tree))
(println "SHA NA NA NA NA NA NA NA NA: " (encode '(SHA NA NA NA NA NA NA NA NA) silhouettes-tree))
(println "GET A JOB: " (encode '(GET A JOB) silhouettes-tree))
(println "SHA NA NA NA NA NA NA NA NA: " (encode '(SHA NA NA NA NA NA NA NA NA) silhouettes-tree))
(println "WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP: " (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) silhouettes-tree))
(println "SHA BOOM: " (encode '(SHA BOOM) silhouettes-tree))

(define lyrics '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define encoded-lyrics (encode lyrics silhouettes-tree))
(println "full lyrics: " encoded-lyrics)
(println "bit count: " (length encoded-lyrics))
(println "symbol count: " (length lyrics))

; It takes 84 bits to encode the lyrics with our huffman encoding. With this eight symbol alphabet we would need 3 bits per symbol in the lyrics, or 108 bits.


