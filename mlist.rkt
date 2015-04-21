#lang racket

(provide mlist list->mlist mlist->list)

(define (mlist . args)
  (list->mlist args))

(define (list->mlist lst)
  (if (null? lst)
      lst
      (mcons (car lst)
             (list->mlist (cdr lst)))))

(define (mlist->list mlst)
  (if (null? mlst)
      mlst
      (cons (mcar mlst)
            (mlist->list (mcdr mlst)))))
