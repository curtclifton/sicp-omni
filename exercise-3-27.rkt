#lang racket

;;; Exercise 3.27

; I don't much feel like drawing environments, but the basic idea is that memo-fib stores it's argument → value pairs from the bottom up, so that the second call to memo-fib in the body always finds a pre-computed answer. If we just tried to memoize at the top level, then only the final result would be saved. That would be useful if we tried to compute the fib of an even larger argument later, but it wouldn't make the first call any more efficient.
