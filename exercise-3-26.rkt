#lang racket

(require "helpers.rkt")
(require "mcons-all-the-things.rkt")

;;; Exercise 3.26

;; A key-based binary tree implementation sketch 
(define (make-empty-tree)
  (define empty-tree (list '*tree*))
  empty-tree)
(define (make-tree-node item)
  (cons item (cons '() '())))
(define (is-empty-tree? tree)
  (and (pair? tree)
       (eq? (car tree) '*tree*)
       (null? (cdr tree))))
(define (tree-root-node tree)
  (cdr tree))
(define (set-tree-root-node! tree tree-node)
  (set-cdr! tree tree-node))
(define (tree-node-item tree-node)
  (car tree-node))
(define (tree-node-left tree-node)
  (car (cdr tree-node)))
(define (tree-node-right tree-node)
  (cdr (cdr tree-node)))
(define (set-tree-node-item! new-item tree-node)
  (set-car! tree-node new-item))
(define (set-tree-node-left! new-left tree-node)
  (set-car! (cdr tree-node) new-left)
  tree-node)
(define (set-tree-node-right! new-right tree-node)
  (set-cdr! (cdr tree-node) new-right)
  tree-node)
(define (find-tree-item key tree key-fn comparison-fn)
  (define (find-item-helper tree-node)
    (if (null? tree-node)
        false
        (let ((comparison-result 
                (comparison-fn key (key-fn (tree-node-item tree-node)))))
          (cond ((eq? comparison-result '=) (tree-node-item tree-node))
                ((eq? comparison-result '<)
                  (find-item-helper (tree-node-left tree-node)))
                (else (find-item-helper (tree-node-right tree-node)))))))
  (find-item-helper (tree-root-node tree)))
(define (insert-tree-item! item tree key-fn comparison-fn)
  (define key (key-fn item))
  (define (insert-item-helper! tree-node)
    (if (null? tree-node)
        (error "empty tree node, shouldn't happen")
        (let ((comparison-result
                (comparison-fn key (key-fn (tree-node-item tree-node)))))
          (cond ((eq? comparison-result '=) 
                  (set-tree-node-item! item tree-node))
                ((eq? comparison-result '<)
                  (if (null? (tree-node-left tree-node))
                      (set-tree-node-left! (make-tree-node item)
                                           tree-node)
                      (insert-item-helper! (tree-node-left tree-node))))
                (else
                  (if (null? (tree-node-right tree-node))
                      (set-tree-node-right! (make-tree-node item)
                                            tree-node)
                      (insert-item-helper! (tree-node-right tree-node))))))))
  (if (is-empty-tree? tree)
      (set-tree-root-node! tree (make-tree-node item))
      (insert-item-helper! (tree-root-node tree)))
  'ok)

(println "Testing binary tree implementation:")
(define (num-comp x y)
  (cond ((< x y) '<)
        ((= x y) '=)
        ((> x y) '>)
        (else (error "not comparable"))))
(define bt (make-empty-tree))
bt
(insert-tree-item! (cons 3 'three) bt car num-comp)
bt
(insert-tree-item! (cons 1 'one) bt car num-comp)
bt
(insert-tree-item! (cons 5 'five) bt car num-comp)
bt
(insert-tree-item! (cons 2 'two) bt car num-comp)
bt
(for-each
  (lambda (x) (println "find " x ": " (find-tree-item x bt car num-comp)))
  '(1 2 3 4 5 6))

; 
; 
; 
; ;; Updated helpers that use a binary-tree instead of a linked list for subtables.
; (define undefined-value '*undefined*)
; (define (make-table-with-key key)
;   (list key undefined-value))
; (define (table-key table)
;   (car table))
; (define (table-root-value table)
;   (car (cdr table)))
; (define (table-subtable-list table)
;   (cdr (cdr table)))
; (define (set-table-root-value! table new-root-value)
;   (let ((value-cell (cdr table)))
;     (set-car! value-cell new-root-value)))
; (define (add-subtable! table subtable)
;   (let ((value-cell (cdr table)))
;     (set-cdr! value-cell
;               (cons subtable (table-subtable-list table)))
;     'ok))
; (define (table-has-root-value? table)
;   (not (eq? (table-root-value table) undefined-value)))
; 
; (println "Testing table data structure:")
; (define rt (make-table-with-key '*table*))
; (println "raw table:")
; rt
; (println "(table-key rt): " (table-key rt))
; (println "(table-root-value rt): " (table-root-value rt))
; (println "(table-subtable-list rt): " (table-subtable-list rt))
; (define rst (make-table-with-key 0))
; (set-table-root-value! rst 'zero)
; (add-subtable! rt rst)
; (println "added <0> → 'zero")
; rt
; (println "(table-subtable-list rt): " (table-subtable-list rt))
; 
; (define (make-table same-key?)
;   (let ((local-table 
;           (make-table-with-key '*table*)))
;     (define (assoc key subtables)
;       (cond ((null? subtables) false)
;             ((same-key? key (table-key (car subtables))) (car subtables))
;             (else (assoc key (cdr subtables)))))
;     (define (find-record! creating-if-needed rest-of-key-path subtable)
;       (if (null? rest-of-key-path) 
;           subtable
;           (let ((nested-subtable (assoc (car rest-of-key-path)
;                                       (table-subtable-list subtable))))
;             (if nested-subtable
;                 (find-record! creating-if-needed 
;                               (cdr rest-of-key-path)
;                               nested-subtable)
;                 (if creating-if-needed
;                     ; splice in new record/subtable and recurse into it
;                     (let ((new-subtable/record 
;                             (make-table-with-key (car rest-of-key-path))))
;                       (add-subtable! subtable new-subtable/record)
;                       (find-record! creating-if-needed
;                                     (cdr rest-of-key-path)
;                                     new-subtable/record))
;                     ; not found
;                     #f)))))
;     (define (lookup key-1 . other-keys)
;       (define key-path (cons key-1 (list->mlist other-keys)))
;       (let ((subtable (find-record! #f key-path local-table)))
;         (if (and subtable (table-has-root-value? subtable))
;             (table-root-value subtable)
;             false)))
;     (define (insert! value key-1 . other-keys)
;       (define key-path (cons key-1 (list->mlist other-keys)))
;       (define last-key
;         ; We could just take (last key-path), except it's a mlist thanks to our requires, so last would go “boom”.
;         (if (null? other-keys)
;             key-1
;             (last other-keys)))
;       (let ((subtable (find-record! #t key-path local-table)))
;         (set-table-root-value! subtable value)))
;     (define (dispatch m)
;       (cond ((eq? m 'lookup-proc) lookup)
;             ((eq? m 'insert-proc!) insert!)
;             ((eq? m 'find-record!) find-record!) 
;             ((eq? m 'dump-table) local-table) ; debug helper to get at the embedded state
;             (else (error "Unknown operation -- TABLE" m))))
;     dispatch))
; 
; (define t (make-table =))
; (define lt (t 'dump-table))
; (println "empty table: ")
; lt
; ;; NOTE: I changed the API of insert! to take value then 1 or more keys
; ((t 'insert-proc!) 'zero 0)
; (println "inserted <0> → 'zero")
; lt
; ((t 'insert-proc!) 'one-zero 1 0)
; (println "inserted <1 0> → 'one-zero")
; lt
; ((t 'insert-proc!) 'zero-one 0 1)
; (println "inserted <0 1> → 'zero-one")
; lt
; (println "expect to find <0>: " ((t 'lookup-proc) 0))
; (println "don't expect to find <1>: " ((t 'lookup-proc) 1))
; (println "expect to find <1 0>: " ((t 'lookup-proc) 1 0))
; (println "expect to find <0 1>: " ((t 'lookup-proc) 0 1))
; (println "don't expect to find <0 0>: " ((t 'lookup-proc) 0 0))
