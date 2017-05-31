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

(define (assert-equal? value expected)
  (if (equal? value expected)
      value
      (error-fmt "Value ~s was not equal to ~s" value expected)))

(define (test state startup . code)
  (newline)
  (display "Running ")
  (display startup)
  (display ":")
  (newline)
  (newline)
  (run startup state startup code))

(define (test-op state op . labels)
  (let ((ls (cons (cons ':halt :halt) labels)))
    (newline)
    (display "Testing ")
    (display (instruction-repr op ls))
    (display ": ")
    (let ((s (((instruction-asm op) ls) state)))
      (display s)
      (newline)
      s)))

(define-syntax test-macro
  (syntax-rules ()
    ((test-macro macro expected)
     (begin (newline)
            (display "Testing ")
            (display 'macro)
            (newline)
            (assert-equal? (map (lambda (op)
                                  (instruction-repr op '()))
                                (flatten macro))
                           (map (lambda (op)
                                  (instruction-repr op '()))
                                (flatten expected)))))))

(define-syntax test-error
  (syntax-rules ()
    ((test-error body ...)
     (unless (equal? 'caught-an-error
                     (with-handlers ((identity (lambda (e)
                                                 'caught-an-error)))
                       body ...))
       (error "Test did not catch an error.")))))