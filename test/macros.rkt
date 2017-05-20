;; Assembler macros tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "../src/macros.rkt")
(require "utils.rkt")

(define-fn (t-spilling)
  (op-set t1 'hello)
  (op-set t2 'world)
  (mc-spill (list t1)
            (op-set t1 'herp)
            (op-set t2 'derp)))

(--> (init-state 5)
     (test ':t-spilling
           (t-spilling))
     (reg-assert t1 'hello)
     (reg-assert t2 'derp))

(define-fn (t-conditionals)
  (op-set r1 'foo)
  (op-set r2 'bar)
  (op-set r3 'bar)
  (mc-and t0
          (mc-not t0 (op-eq? t0 r1 r2))
          (mc-or t1
                 (op-eq? t1 r1 r3)
                 (op-eq? t2 r2 r3)))
  (op-swap t0 t1))

(--> (init-state 5)
     (test ':t-conditionals
           (t-conditionals))
     (reg-assert t1 'true))

(define-fn (t-basic-if)
  (op-set r1 'true)
  (mc-if (op-nil? t0 r1)
         (op-set t1 'then)
         (op-set t1 'else))
  (mc-if (op-nil? t0 r2)
         (op-set t2 'then)
         (op-set t2 'else))
  (mc-if (list (op-nil? t0 r3)
               (op-swap t0 r1)
               (op-atom? t0 t1)
               (op-and t0 t0 r1))
         (op-set t3 'then)
         (op-set t3 'else)))

(--> (init-state 5)
     (test ':t-basic-if
           (t-basic-if))
     (reg-assert t1 'else)
     (reg-assert t2 'then)
     (reg-assert t3 'then))

(define-fn (t-nested-if)
  (mc-if (op-nil? t0 r1)
         (op-set t1 'is-nil)
         (mc-if (op-atom? t0 r1)
                (op-set t1 'is-atom)
                (op-set t1 'is-list))))

(--> (init-state 5)
     (reg-set r1 '(3 2 1 . nil))
     (test ':t-nested-if
           (t-nested-if))
     (reg-assert t1 'is-list))

(define-fn (t-functions)
  (mc-call ':id r0 r1 r2)
  (op-set r3 23))

(--> (init-state 5)
     (reg-set r1 23)
     (reg-set r2 5)
     (test ':t-functions
           (t-functions)
           (mc-define ':id ; (r0 r1) -> r2
                      (break 1)
                      (op-assign r2 r1)
                      (break 2)))
     (reg-assert r1 23)
     (reg-assert r2 23)
     (reg-assert r3 23))

(define-fn (t-call-reordered)
  (mc-call ':rand r0 r2))

(--> (init-state 5)
     (reg-set r1 23)
     (reg-set r2 13)
     (test ':t-call-reordered
           (t-call-reordered)
           (mc-define ':rand ; r0 -> r1
                      (break 1)
                      (op-set r1 5)
                      (break 2)))
     (reg-assert r1 23)
     (reg-assert r2 5))
