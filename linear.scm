;;; Linear-typed Lisp

;; Utils:

(define-syntax -->
  (syntax-rules ()
    ((--> expr) expr)
    ((--> expr (fn args ...) clauses ...) (--> (fn expr args ...) clauses ...))
    ((--> expr clause clauses ...) (--> (clause expr) clauses ...))))

(define (debug tag value)
  (display tag)
  (display ": ")
  (display value)
  (newline))

(define (trace tag state)
  (debug tag state)
  state)

(define (break tag)
  (lambda (labels)
    (lambda (state)
      (trace tag state))))

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (nil? a)
  (eq? a 'nil))

(define (label-offset labels label)
  (let ((off (assoc label labels)))
    (if (null? off)
        :halt
        (cdr off))))

(define (tagged tag suffix)
  (string->symbol (string-append (symbol->string tag) suffix)))

;; State: (C R1 R2 SP FR)

(define :halt -1)

(define pc 0)
(define c 1)
(define r1 2)
(define r2 3)
(define r3 4)
(define t1 5)
(define t2 6)
(define t3 7)
(define sp 8)
(define fr 9)

(define (state pc c r1 r2 r3 t1 t2 t3 sp fr)
  (list pc c r1 r2 r3 t1 t2 t3 sp fr))

(define (init-state cells)
  (define (make-cells n)
    (if (equal? n 1)
        'nil
        (cons 'nil (make-cells (- n 1)))))
  (state 0
         'init                ;; Initial state.
         'nil 'nil 'nil       ;; Main registers.
         'nil 'nil 'nil       ;; Temp registers.
         'nil                 ;; Stack pointer.
         (make-cells cells))) ;; Free list.

(define (reg state r)
  (list-ref state r))

(define (reg-set state r value)
  (if (equal? r 0)
      (cons value (cdr state))
      (cons (car state)
            (reg-set (cdr state) (- r 1) value))))

;; Assembly & running:

(define (assemble tag code)
  (define (label-index ops label)
    (let loop ((i 0)
               (remaining ops))
      (cond ((null? remaining) :halt)
            ((equal? label (car remaining)) i)
            ((symbol? (car remaining)) (loop i (cdr remaining)))
            (':else (loop (+ i 1) (cdr remaining))))))
  (let* ((flat (flatten code))
         (labels (map (lambda (label)
                        (cons label (label-index flat label)))
                      (filter symbol? flat)))
         (assembly (map (lambda (op)
                          (op labels))
                        (filter (lambda (x) (not (symbol? x))) flat))))
    (debug (tagged tag "-labels") labels)
    (debug (tagged tag "-n-ops") (length assembly))
    assembly))

(define (run tag state . code)
  (define (do-run tag state ops)
    (define (set-pc state value)
      (reg-set state pc value))
    (define (inc-pc state)
      (set-pc state (+ 1 (reg state pc))))
    (let* ((curr-pc (reg state pc)))
      (if (equal? :halt curr-pc)
          (trace (tagged tag "-result") state)
          (begin (trace tag state)
                 (let ((result ((list-ref ops curr-pc)
                                (set-pc state 0)))) ;; FIXME Needed in order to support functions.
                   (do-run tag
                           (if (number? result)
                               (set-pc state result)
                               (set-pc result (+ 1 curr-pc)))
                           ops))))))
  (do-run tag state
          (assemble (tagged tag "-assembly")
                    (append code
                            (list (op-halt)
                                  ':halt)))))

;; Opcodes (op dest src ...):

;; pc := (address-of :halt)
(define (op-halt)
  (lambda (labels)
    (lambda (state)
      :halt)))

;; pc := (address-of label)
(define (op-jmp label)
  (lambda (labels)
    (lambda (state)
      (label-offset labels label))))

;; pc := (address-of label) if c is nil.
(define (op-jmp-if-nil label)
  (lambda (labels)
    (lambda (state)
      (if (nil? (reg state c))
          (label-offset labels label)
          state))))

;; pc := (address-of label) if r is (not nil)
(define (op-jmp-if-not-nil label)
  (lambda (labels)
    (lambda (state)
      (if (not (nil? (reg state c)))
          (label-offset labels label)
          state))))

;; c := (nil? r)
(define (op-nil? r)
  (lambda (labels)
    (lambda (state)
      (if (nil? (reg state r))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; c := (atom? r)
(define (op-atom? r)
  (lambda (labels)
    (lambda (state)
      (if (atom? (reg state r))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; c := (eq? r1 r2)
(define (op-eq? r1 r2)
  (lambda (labels)
    (lambda (state)
      (if (eq? (reg state r1) (reg state r2))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; c := (not r)
(define (op-not r)
  (lambda (labels)
    (lambda (state)
      (if (nil? (reg state r))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; c := (and r1 r2)
(define (op-and r1 r2)
  (lambda (labels)
    (lambda (state)
      (if (and (not (nil? (reg state r1)))
               (not (nil? (reg state r2))))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; c := (or r1 r2)
(define (op-or r1 r2)
  (lambda (labels)
    (lambda (state)
      (if (or (not (nil? (reg state r1)))
              (not (nil? (reg state r2))))
          (reg-set state c 'true)
          (reg-set state c 'nil)))))

;; r := atom
(define (op-set r atom)
  (lambda (labels)
    (lambda (state)
      (if (and (atom? (reg state r))
               (atom? atom))
          (reg-set state r atom)
          (reg-set state c 'op-set-error)))))

;; r1 := r2
(define (op-assign r1 r2)
  (lambda (labels)
    (lambda (state)
      (let ((a (reg state r2)))
        (if (and (atom? (reg state r1))
                 (atom? a))
            (reg-set state r1 a)
            (reg-set state c 'op-assign-error))))))

;; tmp := r1, r1 := r2, r2 := tmp
(define (op-swap r1 r2)
  (lambda (labels)
    (lambda (state)
      (let ((a (reg state r1))
            (b (reg state r2)))
        (--> state
             (reg-set r1 b)
             (reg-set r2 a))))))

;; tmp := r1, r1 := (car r2), (car r2) := tmp
(define (op-swap-car r1 r2)
  (lambda (labels)
    (lambda (state)
      (let ((a (reg state r1))
            (b (reg state r2)))
        (if (and (not (equal? r1 r2))
                 (not (atom? b)))
            (--> state
                 (reg-set r1 (car b))
                 (reg-set r2 (cons a (cdr b))))
            (reg-set state c 'op-swap-car-error))))))

;; tmp := r1, r1 := (cdr r2), (cdr r2) := tmp
(define (op-swap-cdr r1 r2)
  (lambda (labels)
    (lambda (state)
      (let ((a (reg state r1))
            (b (reg state r2)))
        (if (and (not (equal? r1 r2))
                 (not (atom? b)))
            (--> state
                 (reg-set r1 (cdr b))
                 (reg-set r2 (cons (car b) a)))
            (reg-set state c 'op-swap-cdr-error))))))

;; Macros:

;; r1 := (cons r2 r1), r2 := nil
(define (mc-push r1 r2)
  (list (op-swap-cdr r1 fr)
        (op-swap-car r2 fr)
        (op-swap r1 fr)))

;; r1 := (car r2), r2 := (cdr r2)
(define (mc-pop r1 r2)
  (list (op-set r1 'nil)
        (op-swap-cdr fr r2)
        (op-swap r2 fr)
        (op-swap-car r1 fr)))

;; Functions (fn args ... result):

(define (fn-free r1)
  (lambda (labels)
    (lambda (state)
      (run 'fn-free state
           ;; Check condition.
           (op-nil? r1)
           (op-jmp-if-not-nil ':end)
           ;; Check what to do.
           (op-atom? r1)
           (op-jmp-if-nil ':not-atom)
           (op-set r1 'nil)
           (op-jmp ':end)
           ':not-atom
           ;; Save state.
           (mc-push sp t1)
           ;; Compute (cdr r1).
           (mc-pop t1 r1)
           (fn-free r1) ;; Free (cdr r1).
           (op-swap t1 r1)
           (fn-free r1) ;; Free (car r1).
           ;; Restore state.
           (mc-pop t1 sp)
           ':end))))

(define (fn-copy r1 r2)
  (lambda (labels)
    (lambda (state)
      (run 'fn-copy state
           ;; Check condition.
           (op-nil? r2)
           (op-jmp-if-nil ':raise-error)
           ;; Check what to do.
           (op-atom? r1)
           (op-jmp-if-nil ':not-atom)
           (op-assign r2 r1)
           (op-jmp ':end)
           ':raise-error
           (op-set c 'fn-copy-error)
           (op-jmp ':end)
           ':not-atom
           ;; Save state.
           (mc-push sp t1)
           (mc-push sp t2)
           ;; Compute the (cdr r1)
           (mc-pop t1 r1)
           (fn-copy r1 r2) ;; Copy (cdr r1).
           (op-swap t1 r1)
           (op-swap t2 r2) ;; Result is stored in r2.
           (fn-copy r1 r2) ;; Copy (car r1).
           (op-swap t1 r1)
           (op-swap t2 r2)
           ;; Restore the argument.
           (mc-push r1 t1)
           (mc-push r2 t2)
           ;; Restore state.
           (mc-pop t2 sp)
           (mc-pop t1 sp)
           ':end))))

(define (fn-equal? r1 r2 r3)
  (lambda (labels)
    (lambda (state)
      (run 'fn-equal? state
           ;; Save state.
           (mc-push sp t1)
           (mc-push sp t2)
           ;; Check the condition.
           (op-atom? r1)
           (op-swap c t1)
           (op-atom? r2)
           (op-swap c t2)
           (op-and t1 t2)
           (op-jmp-if-not-nil ':both-atoms)
           (op-not t1)
           (op-swap c t1)
           (op-not t2)
           (op-swap c t2)
           (op-and t1 t2)
           (op-jmp-if-not-nil ':both-non-atoms)
           ;; An atom & non-atom.
           (op-set r3 'nil)
           (op-jmp ':end)
           ':both-non-atoms
           ;; Save some more state.
           (mc-push sp t3)
           ;; Compute (car r1) & (car r2).
           (mc-pop t1 r1)
           (mc-pop t2 r2)
           (fn-equal? r1 r2 r3)
           (op-swap t1 r1)
           (op-swap t2 r2)
           (op-swap t3 r3) ;; Result of (equal? (cdr r1) (cdr r2)) is in t3.
           (fn-equal? r1 r2 r3) ;; Result of (equal? (car r1) (car r2)) is in r3.
           (op-swap t1 r1)
           (op-swap t2 r2)
           (op-and t3 r3)
           (op-swap c r3) ;; Result of (and t3 r3) lands in r3.
           ;; Restore arguments.
           (mc-push r1 t1)
           (mc-push r2 t2)
           ;; Restore some more state.
           (mc-pop t3 sp)
           (op-jmp ':end)
           ':both-atoms
           (op-eq? r1 r2)
           (op-swap c r3)
           ':end
           ;; Restore state.
           (mc-pop t2 sp)
           (mc-pop t1 sp)))))

(define (fn-cons r1 r2 r3)
  (lambda (labels)
    (lambda (state)
      (run 'fn-cons state
           ;; Save state.
           (mc-push sp t1)
           ;; Check proper list condition.
           (op-atom? r2)
           (op-not c)
           (op-swap c t1)
           (op-nil? r2)
           (op-or t1 c)
           (op-jmp-if-nil ':raise-error)
           ;; Actually cons the value.
           (op-swap r3 r2)
           (mc-push r3 r1)
           (op-jmp ':end)
           ':raise-error
           (op-set c 'fn-cons-error)
           ':end
           ;; Restore state.
           (mc-pop t1 sp)))))

;; Examples:

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

(run 'cons (init-state 5)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (op-set r1 3)
     (fn-cons r1 r3 r3))

(run 'free (init-state 5)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (fn-free r3))

(run 'free (--> (init-state 10)
                (reg-set r1 '(5 4 3 2 1 . nil)))
     (fn-free r1))

(run 'copy (init-state 7)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (fn-copy r3 r2))

(run 'equal (init-state 10)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (fn-equal? r3 r2 r1)
     (break 1)
     (fn-copy r3 r2)
     (fn-equal? r3 r2 r1)
     (break 2)
     (op-set r1 3)
     (fn-cons r1 r3 r3)
     (fn-equal? r3 r2 r1)
     (break 3))

(run 'equal (init-state 10)
     (op-set r1 1)
     (op-set r2 2)
     (fn-equal? r1 r2 r3))

(run 'equal (init-state 10)
     (op-set r1 1)
     (op-set r2 1)
     (fn-equal? r1 r2 r3))

(run 'equal (--> (init-state 10)
                 (reg-set r1 '(2 1 . nil))
                 (reg-set r2 1))
     (fn-equal? r1 r2 r3))

(run 'equal (--> (init-state 10)
                 (reg-set r1 '(2 1 . nil))
                 (reg-set r2 '(2 1 . nil)))
     (fn-equal? r1 r2 r3))

(run 'equal (--> (init-state 10)
                 (reg-set r1 '(1 1 . nil))
                 (reg-set r2 '(2 2 . nil)))
     (fn-equal? r1 r2 r3))
