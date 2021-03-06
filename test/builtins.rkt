;; Built-in function tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "../src/macros.rkt")
(require "../src/builtins.rkt")
(require "utils.rkt")

(--> (init-state 5)
     (reg-set sp '(-1 . nil))
     (reg-set r1 1)
     (reg-set r2 'nil)
     (test ':fn-cons
           (fn-cons))
     (reg-assert r3 '(1 . nil)))

(define-fn (t-cons)
  (op-set r1 1)
  (op-set r2 'nil)
  (mc-call ':fn-cons r0 r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r0 r1 r3 r2)
  (op-set r3 3)
  (mc-call ':fn-cons r0 r3 r2 r1))

(--> (init-state 15)
     (test ':t-cons
           (t-cons)
           (fn-cons))
     (reg-assert r1 '(3 2 1 . nil)))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(5 4 3 2 1 . nil))
     (test ':fn-free
           (fn-free))
     (reg-assert r1 'nil)
     (reg-assert fr (make-cells 15)))

(define-fn (t-free-cons)
  (op-set r1 1)
  (mc-call ':fn-cons r0 r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (mc-call ':fn-free r0 r3))

(--> (init-state 15)
     (test ':t-free-cons
           (t-free-cons)
           (fn-free)
           (fn-cons))
     (reg-assert r3 'nil)
     (reg-assert fr (make-cells 15)))

(--> (init-state 15)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(3 2 1 . nil))
     (test ':fn-copy
           (fn-copy))
     (reg-assert r1 '(3 2 1 . nil)) ;; 0 cells, supplied externally
     (reg-assert r2 '(3 2 1 . nil)) ;; 3 cells
     (reg-assert fr (make-cells 12)))

(define-fn (t-copy)
  (op-set r1 1)
  (mc-call ':fn-cons r0 r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (break 1)
  (mc-call ':fn-copy r0 r3 r2))

(--> (init-state 15)
     (test ':t-copy
           (t-copy)
           (fn-cons)
           (fn-copy))
     (reg-assert r2 '(2 1 . nil)) ;; 2 cells
     (reg-assert r3 '(2 1 . nil)) ;; 2 cells
     (reg-assert fr (make-cells 11)))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 1)
     (reg-set r2 1)
     (test ':fn-equal?
           (fn-equal?))
     (reg-assert r1 1)
     (reg-assert r2 1)
     (reg-assert r3 'true))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 1)
     (reg-set r2 2)
     (test ':fn-equal?
           (fn-equal?))
     (reg-assert r1 1)
     (reg-assert r2 2)
     (reg-assert r3 'nil))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(2 1 . nil))
     (reg-set r2 '(2 1 . nil))
     (test ':fn-equal?
           (fn-equal?))
     (reg-assert r1 '(2 1 . nil))
     (reg-assert r2 '(2 1 . nil))
     (reg-assert r3 'true))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(1 1 . nil))
     (reg-set r2 '(2 2 . nil))
     (test ':fn-equal?
           (fn-equal?))
     (reg-assert r1 '(1 1 . nil))
     (reg-assert r2 '(2 2 . nil))
     (reg-assert r3 'nil))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(2 1 . nil))
     (reg-set r2 1)
     (test ':fn-equal?
           (fn-equal?))
     (reg-assert r1 '(2 1 . nil))
     (reg-assert r2 1)
     (reg-assert r3 'nil))

(define-fn (t-equal-cons)
  (op-set r1 1)
  (mc-call ':fn-cons r0 r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (mc-call ':fn-equal? r0 r3 r2 r1)
  (op-swap r1 t1)
  (break 1)
  (mc-call ':fn-copy r0 r3 r2)
  (mc-call ':fn-equal? r0 r3 r2 r1)
  (op-swap r1 t2)
  (break 2)
  (op-set r1 3)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (mc-call ':fn-equal? r0 r3 r2 r1)
  (op-swap r1 t3)
  (break 3))

(--> (init-state 20)
     (test ':t-equal-cons
           (t-equal-cons)
           (fn-cons)
           (fn-copy)
           (fn-equal?))
     (reg-assert t1 'nil)
     (reg-assert t2 'true)
     (reg-assert t3 'nil))

(define-fn (t-car-cdr)
  (mc-call ':fn-cdr r0 r1 r2)
  (mc-call ':fn-car r0 r2 r1))

(--> (init-state 10)
     (reg-set r1 '(2 1 . nil))
     (test ':t-car-cdr
           (t-car-cdr)
           (fn-free)
           (fn-car)
           (fn-cdr))
     (reg-assert r1 1)
     (reg-assert fr (make-cells 12)))

(--> (init-state 20)
     (reg-set r1 2)
     (reg-set r2 '(1 2 3 4 5 . nil))
     (test ':fn-nth
           (fn-free)
           (fn-car)
           (fn-cdr)
           (fn-nth))
     (reg-assert r3 3)
     (reg-assert fr (make-cells 25)))

(define-fn (t-nth-cons)
  (op-set r1 5)
  (mc-call ':fn-cons r0 r1 r2 r3)
  (op-set r1 4)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (op-set r1 3)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r0 r1 r3 r3)
  (op-set r1 1)
  (mc-call ':fn-cons r0 r1 r3 r2)
  (op-set r1 2)
  (mc-call ':fn-nth r0 r1 r2 r3))

(--> (init-state 20)
     (test ':t-nth-cons
           (fn-free)
           (fn-car)
           (fn-cdr)
           (fn-cons)
           (fn-nth)
           (t-nth-cons))
     (reg-assert r3 3)
     (reg-assert fr (make-cells 20)))

(define-fn (t-env)
  (mc-call ':fn-make-env r0 r1)
  (op-set r2 23)
  (mc-call ':fn-add-env r0 r1 r2 r1)
  (op-set r2 5)
  (mc-call ':fn-add-env r0 r1 r2 r1)
  (op-set r2 0)
  (break 1)
  (mc-call ':fn-get-env r0 r1 r2 r3)
  (op-swap t0 r3)
  (op-set r2 1)
  (break 2)
  (mc-call ':fn-get-env r0 r1 r2 r3)
  (op-swap t1 r3))

(--> (init-state 25)
     (test ':t-env
           (fn-free)
           (fn-copy)
           (fn-car)
           (fn-cdr)
           (fn-cons)
           (fn-nth)
           (fn-make-env)
           (fn-add-env)
           (fn-get-env)
           (t-env))
     (reg-assert r1 '(5 23 . nil)) ;; 2 cells
     (reg-assert t0 5)
     (reg-assert t1 23)
     (reg-assert fr (make-cells 23)))

(define-fn (t-closure)
  (op-addr r2 ':fn-make-closure)
  (mc-call ':fn-make-closure r0 r1 r2 r3))

(--> (init-state 20)
     (reg-set r1 '(23 5 . nil))
     (test ':t-closure
           (fn-make-closure) ; Should be at offset 7.
           (fn-cons)
           (t-closure))
     (reg-assert r3 '(7 23 5 . nil)))

(define-fn (t-closure-call)
  (mc-call ':fn-make-env r0 r1)
  (op-set r2 23)
  (mc-call ':fn-add-env r0 r1 r2 r1)
  (op-set r2 5)
  (mc-call ':fn-add-env r0 r1 r2 r1)
  (op-addr r2 ':test)
  (mc-call ':fn-make-closure r0 r1 r2 r3)
  (op-swap r0 r3)
  (op-set r1 5)
  (break 1)
  (mc-call-closure r0 r1 r2))

(--> (init-state 25)
     (test ':t-closure-call
           (mc-define ':test ; Should be at offset 7.
                      (mc-spill (list t0 r3)
                                (op-swap t0 r1) ;; Argument in t0.
                                (op-swap r1 r0) ;; Env in r1.
                                (op-set r2 1)
                                (break 2)
                                (mc-call ':fn-get-env r0 r1 r2 r3) ;; Val in r3.
                                (op-swap r0 r1) ;; Env in r0.
                                (break 3)
                                (op-add r2 t0 r3)))
           (fn-free)
           (fn-copy)
           (fn-car)
           (fn-cdr)
           (fn-cons)
           (fn-nth)
           (fn-make-env)
           (fn-add-env)
           (fn-get-env)
           (fn-make-closure)
           (t-closure-call))
     (reg-assert r0 '(7 5 23 . nil)) ; 3 cells.
     (reg-assert r2 28)
     (reg-assert fr (make-cells 22)))
