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
  (instruction 'break
               (list tag)
               (lambda (labels)
                 (lambda (state)
                   (debug tag state)))))

(define (reg-assert state r value)
  (if (equal? (reg state r) value)
      state
      (error-fmt "Register ~s was not equal to ~s in ~s" r value state)))

(define (state-assert state expected)
  (if (equal? state expected)
      state
      (error-fmt "State ~s was not equal to ~s" state expected)))

(define (test state startup . code)
  (newline)
  (display "Running ")
  (display startup)
  (display ":")
  (newline)
  (newline)
  (run startup state startup code))

(define (test-op state op . labels)
  (newline)
  (display "Testing ")
  (display (instruction-repr op
                             (cons (cons ':halt :halt)
                                   labels)))
  (display ": ")
  (let ((s (((instruction-asm op) labels) state)))
    (display s)
    (newline)
    s))
