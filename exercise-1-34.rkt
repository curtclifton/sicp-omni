#lang racket

;;; exercise 1.34

(define (f g)
  (g 2))

; (f f) will apply f to 2, which will then attempt to apply 2 to 2,
; resulting in an error
