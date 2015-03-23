#lang racket

;;; Exercise 2.75

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a))) 
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m) 
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define test-complex (make-from-mag-ang 2.0 (/ pi 3)))

(define (apply-generic op arg) (arg op))

(apply-generic 'real-part test-complex)
(apply-generic 'imag-part test-complex)
(apply-generic 'magnitude test-complex)
(apply-generic 'angle test-complex)

;;; Exercise 2.76

; With explicit dispatch, adding new operations just requires writing those operations. Adding new types requires updating all existing operations.

; With message passing style, adding new types only requires introducing the new types. Adding new operations requires modifying all existing types.

; With data-drected style, both new operations and new types can be added independently, though care must be taken to avoid collisions. Such collisions become increasingly likely has new operations and types are added independently of the original introduction of the types.

