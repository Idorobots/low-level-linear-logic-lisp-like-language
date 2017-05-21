;;; Built-in functions

#lang racket

(provide (all-defined-out))

;; Args meaning:
;; &r1 - borrowed reference in r1
;; r1 - ownership to r1 taken

(require "utils.rkt")
(require "vm.rkt")
(require "ops.rkt")
(require "macros.rkt")

;; Utils

(define-syntax define-fn
  (syntax-rules ()
    ((define-fn (name) body ...)
     (define (name)
         (mc-define (tagged ': (symbol->string 'name))
                    body ...)))))

;; Actual functions (fn args ... result)

(define-fn (fn-free) ; (r0 r1) -> ()
  (mc-spill (list t0) ;; FIXME Don't allocate anything during freeing.
            ;; Check what to do.
            (mc-if (op-atom? t0 r1)
                   (op-set r1 'nil)
                   ;; Compute (cdr r1).
                   (list (mc-pop t0 r1)
                         (mc-call ':fn-free r0 r1) ;; Free (cdr r1).
                         (op-swap t0 r1)
                         (mc-call ':fn-free r0 r1))))) ;; Free (car r1).

(define-fn (fn-copy) ; (r0 &r1) -> r2
  (mc-spill (list t0)
            ;; Check condition.
            (mc-if (op-nil? t0 r2)
                   ;; Check what to do.
                   (mc-if (op-atom? t0 r1)
                          (op-assign r2 r1)
                          (mc-spill (list t1 t2)
                                    ;; Compute the (cdr r1)
                                    (mc-pop t1 r1)
                                    (mc-call ':fn-copy r0 r1 r2) ;; Copy (cdr r1).
                                    (op-swap t1 r1)
                                    (op-swap t2 r2) ;; Result is stored in r2.
                                    (mc-call ':fn-copy r0 r1 r2) ;; Copy (car r1).
                                    (op-swap t1 r1)
                                    (op-swap t2 r2)
                                    ;; Restore the argument.
                                    (mc-push r1 t1)
                                    (mc-push r2 t2)))
                   (op-error 'fn-copy-error))))

(define-fn (fn-equal?) ; (r0 &r1 &r2) -> r3
  (mc-spill (list t0 t1)
            ;; Check the condition.
            (mc-if (mc-and t0
                           (op-atom? t0 r1)
                           (op-atom? t1 r2))
                   ;; Both atoms.
                   (op-eq? r3 r1 r2)
                   (mc-if (mc-and t0
                                  (op-nil? t1 t1) ;; (not t1)
                                  (mc-not t0 (op-atom? t0 r2)))
                          ;; Both non-atoms.
                          (mc-spill (list t2 t3)
                                    ;; Compute (car r1) & (car r2).
                                    (mc-pop t1 r1)
                                    (mc-pop t2 r2)
                                    (mc-call ':fn-equal? r0 r1 r2 r3)
                                    (op-swap t1 r1)
                                    (op-swap t2 r2)
                                    (op-swap t3 r3) ;; Result of (equal? (cdr r1) (cdr r2)) is in t3.
                                    (mc-call ':fn-equal? r0 r1 r2 r3) ;; Result of (equal? (car r1) (car r2)) is in r3.
                                    (op-and t0 t3 r3)
                                    (op-swap t0 r3) ;; Result of (and t3 r3) lands in r3.
                                    ;; Restore arguments.
                                    (op-swap t1 r1)
                                    (op-swap t2 r2)
                                    (mc-push r1 t1)
                                    (mc-push r2 t2))
                          ;; An atom & non-atom.
                          (op-set r3 'nil)))))

(define-fn (fn-cons) ; (r1 r2) -> r3
  (mc-spill (list t0 t1)
            ;; Check proper list condition.
            (mc-if (mc-or t0
                          (mc-not t0 (op-atom? t0 r2))
                          (op-nil? t1 r2))
                   ;; Actually cons the value.
                   (list (op-swap r3 r2)
                         (mc-push r3 r1))
                   ;; Rise error otherwise.
                   (op-error 'fn-cons-error))))

(define-fn (fn-car) ; (r0 r1) -> r2
  (mc-spill (list t0)
            (mc-if (op-atom? t0 r1)
                   (op-error 'fn-car-error)
                   (list (mc-pop r2 r1)
                         (mc-call ':fn-free r0 r1)))))

(define-fn (fn-cdr) ; r1 -> r2
  (mc-spill (list t0)
            (mc-if (op-atom? t0 r1)
                   (op-error 'fn-car-error)
                   (list (op-swap r1 r2)
                         (mc-pop r1 r2)
                         (mc-call ':fn-free r0 r1)))))

(define-fn (fn-nth) ; (r0 r1 r2) -> r3
  (mc-spill (list t0)
            (op-set t0 0)
            (mc-if (op-eq? t0 r1 t0)
                   (list (op-swap r1 r2)
                         (mc-call ':fn-car r0 r1 r3))
                   (list (op-set t0 1)
                         (op-sub t0 r1 t0)
                         (op-swap r1 r2)
                         (mc-call ':fn-cdr r0 r1 r2)
                         (op-swap t0 r1)
                         (mc-call ':fn-nth r0 r1 r2 r3)))))
