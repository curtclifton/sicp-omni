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

(reset-agenda!)
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

;;; Exercise 3.29

(println "exercise 3.29")

(define (nand-or-gate input-1 input-2 output)
  (define not-input-1 (make-wire))
  (define not-input-2 (make-wire))
  (define not-output (make-wire))
  (inverter input-1 not-input-1)
  (inverter input-2 not-input-2)
  (and-gate not-input-1 not-input-2 not-output)
  (inverter not-output output))

(reset-agenda!)
(define input-3 (make-wire))
(define input-4 (make-wire))
(define output-2 (make-wire))
(nand-or-gate input-3 input-4 output-2)
(propagate)
(probe 'output-2 output-2)
(log-with-time "created wires and nand-or-gate")
(set-signal! input-3 1)
(log-with-time "raised input-3")
(propagate)
(set-signal! input-4 1)
(log-with-time "raised input-4")
(propagate)
(set-signal! input-3 0)
(log-with-time "lowered input-3")
(propagate)
(set-signal! input-4 0)
(log-with-time "lowered input-4")
(propagate)

;; The delay of the nand-gate based or-gate is 7, (+ (* 2 inverter-delay) and-gate-delay). That makes sense, as the pipeline length from input to output includes two inverters and one and-gate.

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
