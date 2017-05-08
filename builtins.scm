;; Built-in functions

(load "utils.scm")
(load "asm.scm")

;; Utils

(define (fix-state fun) ;; FIXME This shouldn't be here...
  (lambda (state)
    (let ((saved-pc (reg state pc)))
      (--> state
           (reg-set pc 0)
           fun
           (reg-set pc saved-pc)))))

;; Actual functions (fn args ... result)

(define (fn-free)
  (mc-define ':fn-free ; r1 -> ()
             ;; Check condition.
             (mc-when (mc-not (op-nil? r1))
                      ;; Check what to do.
                      (mc-if (op-atom? r1)
                             (op-set r1 'nil)
                             (mc-spill (list t1)
                                       ;; Compute (cdr r1).
                                       (mc-pop t1 r1)
                                       (mc-call ':fn-free r1) ;; Free (cdr r1).
                                       (op-swap t1 r1)
                                       (mc-call ':fn-free r1)))))) ;; Free (car r1).

(define (fn-copy)
  (mc-define ':fn-copy ; r1 -> r2
             ;; Check condition.
             (mc-if (op-nil? r2)
                    ;; Check what to do.
                    (mc-if (op-atom? r1)
                           (op-assign r2 r1)
                           (mc-spill (list t1 t2)
                                     ;; Compute the (cdr r1)
                                     (mc-pop t1 r1)
                                     (mc-call ':fn-copy r1 r2) ;; Copy (cdr r1).
                                     (op-swap t1 r1)
                                     (op-swap t2 r2) ;; Result is stored in r2.
                                     (mc-call ':fn-copy r1 r2) ;; Copy (car r1).
                                     (op-swap t1 r1)
                                     (op-swap t2 r2)
                                     ;; Restore the argument.
                                     (mc-push r1 t1)
                                     (mc-push r2 t2)))
                    (op-set c 'fn-copy-error))))

(define (fn-equal? r1 r2 r3)
  (lambda (labels)
    (fix-state
     (lambda (state)
       (run 'fn-equal? state
            (mc-spill (list t1)
                      ;; Check the condition.
                      (mc-if (mc-and t1
                                     (op-atom? r1)
                                     (op-atom? r2))
                             ;; Both atoms.
                             (list
                              (op-eq? r1 r2)
                              (op-swap c r3))
                             (mc-if (mc-and t1
                                            (op-not t1)
                                            (mc-not (op-atom? r2)))
                                    ;; Both non-atoms.
                                    (mc-spill (list t2 t3)
                                              ;; Compute (car r1) & (car r2).
                                              (mc-pop t1 r1)
                                              (mc-pop t2 r2)
                                              (fn-equal? r1 r2 r3)
                                              (op-swap t1 r1)
                                              (op-swap t2 r2)
                                              (op-swap t3 r3) ;; Result of (equal? (cdr r1) (cdr r2)) is in t3.
                                              (fn-equal? r1 r2 r3) ;; Result of (equal? (car r1) (car r2)) is in r3.
                                              (op-and t3 r3)
                                              (op-swap c r3) ;; Result of (and t3 r3) lands in r3.
                                              ;; Restore arguments.
                                              (op-swap t1 r1)
                                              (op-swap t2 r2)
                                              (mc-push r1 t1)
                                              (mc-push r2 t2))
                                    ;; An atom & non-atom.
                                    (op-set r3 'nil)))))))))

(define (fn-cons)
  (mc-define ':fn-cons ; (r1 r2) -> r3
             (mc-spill (list t1)
                       ;; Check proper list condition.
                       (mc-if (mc-or t1
                                     (mc-not (op-atom? r2))
                                     (op-nil? r2))
                              ;; Actually cons the value.
                              (list
                               (op-swap r3 r2)
                               (mc-push r3 r1))
                              ;; Rise error otherwise.
                              (op-set c 'fn-cons-error)))))
