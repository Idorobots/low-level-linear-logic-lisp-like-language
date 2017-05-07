;; Tests

(load "utils.scm")
(load "asm.scm")
(load "builtins.scm")

(-->
 (run 'if (init-state 1)
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
 (reg-assert t1 'else)
 (reg-assert t2 'then)
 (reg-assert t3 'then))

(-->
 (run 'if (--> (init-state 1)
               (reg-set r1 '(3 2 1 . nil)))
      (mc-if (op-nil? r1)
             (op-set t1 'is-nil)
             (mc-if (op-atom? r1)
                    (op-set t1 'is-atom)
                    (op-set t1 'is-list))))
 (reg-assert t1 'is-list))

(-->
 (run 'control (init-state 1)
      ':label
      (op-jmp ':start)
      ':set-r1-to-hello
      (op-set r1 'hello)
      (op-eq? r1 r2)
      (op-jmp-if-not-nil ':label)
      ':start
      (op-set r2 'hello)
      (op-eq? r1 r2)
      (op-jmp-if-nil ':set-r1-to-hello)
      (op-halt)
      (op-set r3 'herp))
 (reg-assert r1 'hello)
 (reg-assert r2 'hello)
 (reg-assert r3 'nil))

(-->
 (run 'ops (init-state 4)
     (op-set r2 'true)
     (op-nil? r1)
     (op-swap c r1)
     (op-eq? r1 r2)
     (op-atom? r2)
     (op-set sp 'hello)
     (op-assign c sp)
     (op-swap-car c fr)
     (op-nil? c)
     (op-swap-cdr sp fr)
     (mc-push r2 r1)
     (op-set r1 'nil)
     (mc-pop r1 r2))
 (reg-assert r1 'true)
 (reg-assert r2 'true)
 (reg-assert fr '(nil . hello)))

(-->
 (run 'cons (init-state 5)
      (op-set r1 1)
      (fn-cons r1 r2 r3)
      (op-set r1 2)
      (fn-cons r1 r3 r3)
      (op-set r1 3)
      (fn-cons r1 r3 r3))
 (reg-assert r3 '(3 2 1 . nil)))

(-->
 (run 'free (init-state 5)
      (op-set r1 1)
      (fn-cons r1 r2 r3)
      (op-set r1 2)
      (fn-cons r1 r3 r3)
      (fn-free r3))
 (reg-assert r3 'nil)
 (reg-assert fr (make-cells 5)))

(-->
 (run 'free (--> (init-state 5)
                 (reg-set r1 '(5 4 3 2 1 . nil)))
      (fn-free r1))
 (reg-assert r1 'nil)
 (reg-assert fr (make-cells 10)))

(-->
 (run 'copy (init-state 7)
      (op-set r1 1)
      (fn-cons r1 r2 r3)
      (op-set r1 2)
      (fn-cons r1 r3 r3)
      (fn-copy r3 r2))
 (reg-assert r2 '(2 1 . nil)) ;; 2 cells
 (reg-assert r3 '(2 1 . nil)) ;; 2 cells
 (reg-assert fr (make-cells 3)))

(-->
 (run 'equal (init-state 10)
      (op-set r1 1)
      (fn-cons r1 r2 r3)
      (op-set r1 2)
      (fn-cons r1 r3 r3)
      (fn-equal? r3 r2 r1)
      (op-swap r1 t1)
      (break 1)
      (fn-copy r3 r2)
      (fn-equal? r3 r2 r1)
      (op-swap r1 t2)
      (break 2)
      (op-set r1 3)
      (fn-cons r1 r3 r3)
      (fn-equal? r3 r2 r1)
      (op-swap r1 t3)
      (break 3))
 (reg-assert t1 'nil)
 (reg-assert t2 'true)
 (reg-assert t3 'nil))

(-->
 (run 'equal (init-state 10)
      (op-set r1 1)
      (op-set r2 2)
      (fn-equal? r1 r2 r3))
 (reg-assert r3 'nil))

(-->
 (run 'equal (init-state 10)
      (op-set r1 1)
      (op-set r2 1)
      (fn-equal? r1 r2 r3))
 (reg-assert r3 'true))

(-->
 (run 'equal (--> (init-state 10)
                  (reg-set r1 '(2 1 . nil))
                  (reg-set r2 1))
      (fn-equal? r1 r2 r3))
 (reg-assert r3 'nil))

(-->
 (run 'equal (--> (init-state 10)
                  (reg-set r1 '(2 1 . nil))
                  (reg-set r2 '(2 1 . nil)))
      (fn-equal? r1 r2 r3))
 (reg-assert r3 'true))

(-->
 (run 'equal (--> (init-state 10)
                  (reg-set r1 '(1 1 . nil))
                  (reg-set r2 '(2 2 . nil)))
      (fn-equal? r1 r2 r3))
 (reg-assert r3 'nil))
