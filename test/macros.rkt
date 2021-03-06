;; Assembler macros tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "../src/macros.rkt")
(require "utils.rkt")

;; Basic stuff

(test-error (returned 23))

(test-error (returned (list 23)))

(assert-equal? (returned (op-set r0 'test))
               r0)

(assert-equal? (returned (list (op-set r0 'test)
                               (op-assign r1 r0)))
               r1)

(assert-equal? (returned (list (op-set r0 'test)
                               (list (list (op-assign r1 r0)))))
               r1)

(test-macro (mc-or r0
                   (op-atom? r1 r2)
                   (op-atom? t1 t2))
            (list (op-atom? r1 r2)
                  (op-atom? t1 t2)
                  (op-or r0 r1 t1)))

(test-macro (mc-and r0
                   (op-atom? r1 r2)
                   (op-atom? t1 t2))
            (list (op-atom? r1 r2)
                  (op-atom? t1 t2)
                  (op-and r0 r1 t1)))

(test-macro (mc-spill '() (op-swap t0 t0))
            (list (op-swap t0 t0)))

(test-macro (mc-spill (list t0) (op-swap r0 r0))
            (list (mc-push sp t0)
                  (op-swap r0 r0)
                  (mc-pop t0 sp)))

(test-macro (mc-spill (list t0 t1) (op-swap r0 r0))
            (list (mc-push sp t0)
                  (mc-push sp t1)
                  (op-swap r0 r0)
                  (mc-pop t1 sp)
                  (mc-pop t0 sp)))

(test-macro (mc-call ':test)
            (list (op-set tpc 4)
                  (op-add tpc tpc pc)
                  (mc-push sp tpc)
                  (op-jmp ':test)))

(test-macro (mc-call ':test r0 r1 r2)
            (list (op-set tpc 4)
                  (op-add tpc tpc pc)
                  (mc-push sp tpc)
                  (op-jmp ':test)))

(test-macro (mc-call t0 r0 r1 r2)
            (list (op-set tpc 4)
                  (op-add tpc tpc pc)
                  (mc-push sp tpc)
                  (op-jmp-indirect t0)))

(test-macro (mc-call ':test r1 r0)
            (mc-spill (list t0 t1)
                      (op-swap r1 t0)
                      (op-swap r0 t1)
                      (op-swap r0 t0)
                      (op-swap r1 t1)
                      (op-set tpc 4)
                      (op-add tpc tpc pc)
                      (mc-push sp tpc)
                      (op-jmp ':test)
                      (op-swap r0 t0)
                      (op-swap r1 t1)
                      (op-swap r1 t0)
                      (op-swap r0 t1)))

;; More complex stuff

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
