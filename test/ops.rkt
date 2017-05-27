;; Basic operations tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "utils.rkt")

;; Basic instructions:

(test-error
 (--> (init-state 1)
      (test-op (op-jmp 23))))

(test-error
 (--> (init-state 1)
      (test-op (op-jmp ':test))))

(--> (init-state 1)
     (test-op (op-jmp ':test) (cons ':test 23))
     (state-assert (--> (init-state 1)
                        (reg-set pc 22))))

(test-error
 (--> (init-state 1)
      (test-op (op-jmp-indirect ':test) (cons ':test 23))))

(test-error
 (--> (init-state 1)
      (reg-set r0 'totally-not-a-number)
      (test-op (op-jmp-indirect r0))))

(--> (init-state 1)
     (reg-set r0 23)
     (test-op (op-jmp-indirect r0))
     (state-assert (--> (init-state 1)
                        (reg-set pc 22)
                        (reg-set r0 23))))

(test-error
 (--> (init-state 1)
      (test-op (op-br r0 23))))

(test-error
 (--> (init-state 1)
      (test-op (op-br r0 ':test))))

(--> (init-state 1)
     (test-op (op-br r0 ':test) (cons ':test 23))
     (state-assert (init-state 1)))

(--> (init-state 1)
     (reg-set r0 'true)
     (test-op (op-br r0 ':test) (cons ':test 23))
     (state-assert (--> (init-state 1)
                        (reg-set pc 22)
                        (reg-set r0 'true))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-nil? r0 r1))))

(--> (init-state 1)
     (test-op (op-nil? r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r1 'true))))

(--> (init-state 1)
     (reg-set r0 'true)
     (reg-set r1 'not-nil)
     (test-op (op-nil? r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'true)
                        (reg-set r1 'nil))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-atom? r0 r1))))

(--> (init-state 1)
     (test-op (op-atom? r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r1 'true))))

(--> (init-state 1)
     (reg-set r0 'not-nil)
     (test-op (op-atom? r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'not-nil)
                        (reg-set r1 'true))))

(--> (init-state 1)
     (reg-set r0 '(not-nil . nil))
     (reg-set r1 'not-nil)
     (test-op (op-atom? r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 '(not-nil . nil))
                        (reg-set r1 'nil))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-eq? r0 r1 r2))))

(--> (init-state 1)
     (reg-set r0 1)
     (reg-set r1 2)
     (reg-set r2 3)
     (test-op (op-eq? r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 1)
                        (reg-set r1 2)
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 '1)
     (reg-set r1 '(not . nil))
     (reg-set r2 'not-nil)
     (test-op (op-eq? r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 '1)
                        (reg-set r1 '(not . nil))
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 '(not . nil))
     (reg-set r1 '(not . nil))
     (reg-set r2 'not-nil)
     (test-op (op-eq? r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 '(not . nil))
                        (reg-set r1 '(not . nil))
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 1)
     (reg-set r1 1)
     (test-op (op-eq? r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 1)
                        (reg-set r1 1)
                        (reg-set r2 'true))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-and r0 r1 r2))))

(--> (init-state 1)
     (reg-set r0 'nil)
     (reg-set r1 'nil)
     (reg-set r2 'not-nil)
     (test-op (op-and r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'nil)
                        (reg-set r1 'nil)
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 'nil)
     (reg-set r1 'true)
     (reg-set r2 'not-nil)
     (test-op (op-and r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'nil)
                        (reg-set r1 'true)
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 'true)
     (reg-set r1 'true)
     (reg-set r2 'not-nil)
     (test-op (op-and r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'true)
                        (reg-set r1 'true)
                        (reg-set r2 'true))))

(--> (init-state 1)
     (reg-set r0 1)
     (reg-set r1 2)
     (reg-set r2 'not-nil)
     (test-op (op-and r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 1)
                        (reg-set r1 2)
                        (reg-set r2 'true))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-or r0 r1 r2))))

(--> (init-state 1)
     (reg-set r0 'nil)
     (reg-set r1 'nil)
     (reg-set r2 'not-nil)
     (test-op (op-or r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'nil)
                        (reg-set r1 'nil)
                        (reg-set r2 'nil))))

(--> (init-state 1)
     (reg-set r0 'nil)
     (reg-set r1 'true)
     (reg-set r2 'not-nil)
     (test-op (op-or r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'nil)
                        (reg-set r1 'true)
                        (reg-set r2 'true))))

(--> (init-state 1)
     (reg-set r0 'true)
     (reg-set r1 'true)
     (reg-set r2 'not-nil)
     (test-op (op-or r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 'true)
                        (reg-set r1 'true)
                        (reg-set r2 'true))))

(--> (init-state 1)
     (reg-set r0 1)
     (reg-set r1 2)
     (reg-set r2 'not-nil)
     (test-op (op-or r2 r1 r0))
     (state-assert (--> (init-state 1)
                        (reg-set r0 1)
                        (reg-set r1 2)
                        (reg-set r2 'true))))

(test-error
 (--> (init-state 1)
      (test-op (op-addr r0 23))))

(test-error
 (--> (init-state 1)
      (test-op (op-addr r0 ':test))))

(test-error
 (--> (init-state 1)
      (reg-set r0 '(not . nil))
      (test-op (op-addr r0 ':test) (cons ':test 23))))

(--> (init-state 1)
     (test-op (op-addr r0 ':test) (cons ':test 23))
     (state-assert (--> (init-state 1)
                        (reg-set r0 23))))

(test-error
 (--> (init-state 1)
      (test-op (op-swap-cdr r1 r1))))

;; More complex stuff:

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
  (op-nil? t0 r1)
  (op-swap t0 r1)
  (op-eq? t0 r1 r2)
  (op-atom? t0 r2)
  (op-set t3 'hello)
  (op-assign t0 t3)
  (op-swap-car t0 fr)
  (op-nil? t0 t1)
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
  (op-sub t1 t1 t2)
  (op-swap t2 t3)
  (op-set t3 5)
  (op-add t1 t1 t3))

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
  (op-eq? t0 r1 r2)
  (op-br t0 ':label)
  ':start
  (op-set r2 'hello)
  (op-eq? t0 r1 r2)
  (op-nil? t0 t0)
  (op-br t0 ':set-r1-to-hello)
  (op-jmp ':halt)
  (op-set r3 'herp))

(--> (init-state 5)
     (test ':t-operations
           (t-operations))
     (reg-assert r1 'hello)
     (reg-assert r2 'hello)
     (reg-assert r3 'nil))
