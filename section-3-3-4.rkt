#lang racket

(require "helpers.rkt")
(require "mcons-all-the-things.rkt")
(require "section-3-3-4-book.rkt")

;;; Exercise 3.28

(println "exercise 3.28")

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

(define (or-gate input-1 input-2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal input-1) (get-signal input-2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input-1 or-action-procedure)
  (add-action! input-2 or-action-procedure)
  'ok)

;; Test an or-gate

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))
(or-gate input-1 input-2 output)
(probe 'output output)
(log-with-time "created wires and or-gate")
(set-signal! input-1 1)
(log-with-time "raised input-1")
(propagate)
(set-signal! input-2 1)
(log-with-time "raised input-2")
(propagate)
(set-signal! input-1 0)
(log-with-time "lowered input-1")
(propagate)
(set-signal! input-2 0)
(log-with-time "lowered input-2")
(propagate)

;;; Interesting circuits

; (define (half-adder a b s c)
;   (let ((d (make-wire)) (e (make-wire)))
;     (or-gate a b d)
;     (and-gate a b c)
;     (inverter c e)
;     (and-gate d e s)
;     'ok))
; 
; (define (full-adder a b c-in sum c-out)
;   (let ((s (make-wire))
;         (c1 (make-wire))
;         (c2 (make-wire)))
;     (half-adder b c-in s c1)
;     (half-adder a s sum c2)
;     (or-gate c1 c2 c-out)
;     'ok))
; 
; ;;; Example use
; 
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define sum (make-wire))
; (define carry (make-wire))
; (probe 'sum sum)
; (probe 'carry carry)
; 
; (half-adder input-1 input-2 sum carry)
; (set-signal! input-1 1)
; (propagate)
; 
; (set-signal! input-2 1)
; (propagate)
