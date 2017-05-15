#lang racket
;; Tests

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/ops.rkt")
(require "../src/macros.rkt")
(require "../src/builtins.rkt")
(require "../src/asm.rkt")

;; Testing utils:

(define (break tag)
  (lambda (labels)
    (instruction
     `(break ,tag)
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

;; Actual tests:

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
  (mc-push r2 r1)
  (op-set r1 'nil)
  (mc-pop r1 r2)
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
  (mc-br-if-nil ':set-r1-to-hello)
  (mc-halt)
  (op-set r3 'herp))

(--> (init-state 5)
     (test ':t-operations
           (t-operations))
     (reg-assert r1 'hello)
     (reg-assert r2 'hello)
     (reg-assert r3 'nil))

(define-fn (t-conditionals)
  (op-set r1 'foo)
  (op-set r2 'bar)
  (op-set r3 'bar)
  (mc-and t1
          (mc-not (op-eq? r1 r2))
          (mc-or t2
                 (op-eq? r1 r3)
                 (op-eq? r2 r3)))
  (op-swap c t1))

(--> (init-state 5)
     (test ':t-conditionals
           (t-conditionals))
     (reg-assert t1 'true))

(define-fn (t-basic-if)
  (op-set r1 'true)
  (mc-if (op-nil? r1)
         (op-set t1 'then)
         (op-set t1 'else))
  (mc-if (op-nil? r2)
         (op-set t2 'then)
         (op-set t2 'else))
  (mc-if (list (op-nil? r3)
               (op-swap c r1)
               (op-atom? t1)
               (op-and c r1))
         (op-set t3 'then)
         (op-set t3 'else)))

(--> (init-state 5)
     (test ':t-basic-if
           (t-basic-if))
     (reg-assert t1 'else)
     (reg-assert t2 'then)
     (reg-assert t3 'then))

(define-fn (t-nested-if)
  (mc-if (op-nil? r1)
         (op-set t1 'is-nil)
         (mc-if (op-atom? r1)
                (op-set t1 'is-atom)
                (op-set t1 'is-list))))

(--> (init-state 5)
     (reg-set r1 '(3 2 1 . nil))
     (test ':t-nested-if
           (t-nested-if))
     (reg-assert t1 'is-list))

(define-fn (t-functions)
  (mc-call ':id r1 r2)
  (op-set r3 23))

(--> (init-state 5)
     (reg-set r1 23)
     (reg-set r2 5)
     (test ':t-functions
           (t-functions)
           (mc-define ':id ; r1 -> r2
                      (break 1)
                      (op-assign r2 r1)
                      (break 2)))
     (reg-assert r1 23)
     (reg-assert r2 23)
     (reg-assert r3 23))

(define-fn (t-call-reordered)
  (mc-call ':rand r2))

(--> (init-state 5)
     (reg-set r1 23)
     (reg-set r2 13)
     (test ':t-call-reordered
           (t-call-reordered)
           (mc-define ':rand ; () -> r1
                      (break 1)
                      (op-set r1 5)
                      (break 2)))
     (reg-assert r1 23)
     (reg-assert r2 5))

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
  (mc-call ':fn-cons r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r1 r3 r2)
  (op-set r3 3)
  (mc-call ':fn-cons r3 r2 r1))

(--> (init-state 10)
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
  (mc-call ':fn-cons r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r1 r3 r3)
  (mc-call ':fn-free r3))

(--> (init-state 10)
     (test ':t-free-cons
           (t-free-cons)
           (fn-free)
           (fn-cons))
     (reg-assert r3 'nil)
     (reg-assert fr (make-cells 10)))

(--> (init-state 10)
     (reg-set sp '(-1 . nil))
     (reg-set r1 '(3 2 1 . nil))
     (test ':fn-copy
           (fn-copy))
     (reg-assert r1 '(3 2 1 . nil)) ;; 0 cells, supplied externally
     (reg-assert r2 '(3 2 1 . nil)) ;; 3 cells
     (reg-assert fr (make-cells 7)))

(define-fn (t-copy)
  (op-set r1 1)
  (mc-call ':fn-cons r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r1 r3 r3)
  (break 1)
  (mc-call ':fn-copy r3 r2))

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
  (mc-call ':fn-cons r1 r2 r3)
  (op-set r1 2)
  (mc-call ':fn-cons r1 r3 r3)
  (mc-call ':fn-equal? r3 r2 r1)
  (op-swap r1 t1)
  (break 1)
  (mc-call ':fn-copy r3 r2)
  (mc-call ':fn-equal? r3 r2 r1)
  (op-swap r1 t2)
  (break 2)
  (op-set r1 3)
  (mc-call ':fn-cons r1 r3 r3)
  (mc-call ':fn-equal? r3 r2 r1)
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
  (mc-call ':fn-cdr r1 r2)
  (mc-call ':fn-car r2 r1))

(--> (init-state 10)
     (reg-set r1 '(2 1 . nil))
     (test ':t-car-cdr
           (t-car-cdr)
           (fn-free)
           (fn-car)
           (fn-cdr))
     (reg-assert r1 1)
     (reg-assert fr (make-cells 12)))
