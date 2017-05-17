;; Basic operations tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "utils.rkt")

(define-fn (t-errors)
  (op-swap-car r1 r1))

(--> (with-handlers ((identity (lambda (e)
                                 (init-state 1))))
       (test (--> (init-state 1)
                  (reg-set c 'true))
             ':t-errors
             (t-errors)))
     (reg-assert c 'init))

(define-fn (t-running)
  (op-assign t1 pc)
  (op-assign t2 pc))

(--> (init-state 5)
     (test ':t-running
           (t-running))
     (reg-assert pc -1)
     (reg-assert t1 7)
     (reg-assert t2 8))

(define-fn (t-basic-ops)
  (op-set r2 'true)
  (op-nil? r1)
  (op-swap c r1)
  (op-eq? r1 r2)
  (op-atom? r2)
  (op-set t3 'hello)
  (op-assign c t3)
  (op-swap-car c fr)
  (op-nil? c)
  (op-swap-cdr t3 fr)
  (op-swap-cdr r2 fr)
  (op-swap-car r1 fr)
  (op-swap r2 fr)
  (op-set r1 'nil)
  (op-set r1 'nil)
  (op-swap-cdr fr r2)
  (op-swap r2 fr)
  (op-swap-car r1 fr)
  (op-set t1 25)
  (op-set t2 3)
  (op-sub t1 t2)
  (op-swap t2 t3)
  (op-set t3 5)
  (op-add t1 t3))

(--> (init-state 4)
     (test ':t-basic-ops
           (t-basic-ops))
     (reg-assert r1 'true)
     (reg-assert r2 'true)
     (reg-assert fr '(nil nil . hello))
     (reg-assert t1 27)
     (reg-assert t2 '(nil . nil))
     (reg-assert t3 5))

(define-fn (t-operations)
  ':label
  (op-jmp ':start)
  ':set-r1-to-hello
  (op-set r1 'hello)
  (op-eq? r1 r2)
  (op-br ':label)
  ':start
  (op-set r2 'hello)
  (op-eq? r1 r2)
  (op-nil? c)
  (op-br ':set-r1-to-hello)
  (op-jmp ':halt)
  (op-set r3 'herp))

(--> (init-state 5)
     (test ':t-operations
           (t-operations))
     (reg-assert r1 'hello)
     (reg-assert r2 'hello)
     (reg-assert r3 'nil))
