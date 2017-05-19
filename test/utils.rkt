;; Test utils

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "../src/builtins.rkt")
(require "../src/asm.rkt")

(provide (all-defined-out))
(provide define-fn)

(define (break tag)
  (lambda (labels)
    (instruction 'break
                 (list tag)
                 (lambda (state)
                   (debug tag state)))))

(define (reg-assert state r value)
  (if (equal? (reg state r) value)
      state
      (error (format "Register ~s was not equal to ~s in ~s" r value state))))

(define (test state startup . code)
  (newline)
  (display "Running ")
  (display startup)
  (display ":")
  (newline)
  (newline)
  (run startup state startup code))
