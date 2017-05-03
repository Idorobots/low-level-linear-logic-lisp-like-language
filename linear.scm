;;; Linear-typed Lisp

;; Utils:

(define-syntax -->
  (syntax-rules ()
    ((--> expr) expr)
    ((--> expr (fn args ...) clauses ...) (--> (fn expr args ...) clauses ...))
    ((--> expr clause clauses ...) (--> (clause expr) clauses ...))))

(define (trace tag state)
  (display tag)
  (display ": ")
  (display state)
  (newline)
  state)

(define (break tag)
  (lambda (state)
    (trace tag state)))

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (nil? a)
  (eq? a 'nil))

;; State: (C R1 R2 SP FR)

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

;; Run:

(define (run tag state . ops)
  (define (set-pc state value)
    (reg-set state pc value))
  (define (inc-pc state)
    (set-pc state (+ 1 (reg state pc))))
  (define (label-index ops label)
    (let loop ((i 0)
               (remaining ops))
      (if (or (null? remaining)
              (equal? label (car remaining)))
          i
          (loop (+ i 1) (cdr remaining)))))
  (let* ((curr-pc (reg state pc))
         (curr-op (if (>= curr-pc (length ops))
                      ':halt
                      (list-ref ops curr-pc))))
    (cond ((equal? ':halt curr-op)
           (trace (string-append (symbol->string tag) "-result") state))
          ((symbol? curr-op)
           (apply run tag (inc-pc state) ops))
          ((procedure? curr-op)
           (trace tag state)
           (let ((result (curr-op (set-pc state 0)))) ;; FIXME Needed in order to support functions.
             (apply run
                    tag
                    (if (symbol? result)
                        (set-pc state (label-index ops result))
                        (set-pc result (+ 1 curr-pc)))
                    ops))))))

;; Opcodes (op dest src ...):

;; pc := (address-of :halt)
(define (op-halt)
  (op-jmp ':halt))

;; pc := (address-of label)
(define (op-jmp label)
  (lambda (state)
    label))

;; pc := (address-of label) if c is nil.
(define (op-jmp-if-nil label)
  (lambda (state)
    (if (nil? (reg state c))
        label
        state)))

;; pc := (address-of label) if r is (not nil)
(define (op-jmp-if-not-nil label)
  (lambda (state)
    (if (not (nil? (reg state c)))
        label
        state)))

;; c := (nil? r)
(define (op-nil? r)
  (lambda (state)
    (if (nil? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; c := (atom? r)
(define (op-atom? r)
  (lambda (state)
    (if (atom? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; c := (eq? r1 r2)
(define (op-eq? r1 r2)
  (lambda (state)
    (if (eq? (reg state r1) (reg state r2))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; c := (not r)
(define (op-not r)
  (lambda (state)
    (if (nil? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; c := (and r1 r2)
(define (op-and r1 r2)
  (lambda (state)
    (if (and (not (nil? (reg state r1)))
             (not (nil? (reg state r2))))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; c := (or r1 r2)
(define (op-or r1 r2)
  (lambda (state)
    (if (or (not (nil? (reg state r1)))
            (not (nil? (reg state r2))))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

;; r := atom
(define (op-set r atom)
  (lambda (state)
    (if (and (atom? (reg state r))
             (atom? atom))
        (reg-set state r atom)
        (reg-set state c 'op-set-error))))

;; r1 := r2
(define (op-assign r1 r2)
  (lambda (state)
    (let ((a (reg state r2)))
      (if (and (atom? (reg state r1))
               (atom? a))
          (reg-set state r1 a)
          (reg-set state c 'op-assign-error)))))

;; tmp := r1, r1 := r2, r2 := tmp
(define (op-swap r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (--> state
           (reg-set r1 b)
           (reg-set r2 a)))))

;; tmp := r1, r1 := (car r2), (car r2) := tmp
(define (op-swap-car r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (and (not (equal? r1 r2))
               (not (atom? b)))
          (--> state
               (reg-set r1 (car b))
               (reg-set r2 (cons a (cdr b))))
          (reg-set state c 'op-swap-car-error)))))

;; tmp := r1, r1 := (cdr r2), (cdr r2) := tmp
(define (op-swap-cdr r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (and (not (equal? r1 r2))
               (not (atom? b)))
          (--> state
               (reg-set r1 (cdr b))
               (reg-set r2 (cons (car b) a)))
          (reg-set state c 'op-swap-cdr-error)))))

;; Somewhat functions:

;; r1 := (cons r2 r1), r2 := nil
(define (op-push r1 r2)
  (lambda (state)
    (if (not (equal? r1 r2))
        (run 'op-push state
             (op-swap-cdr r1 fr)
             (op-swap-car r2 fr)
             (op-swap r1 fr))
        (reg-set state c 'op-cons-error))))

;; r1 := (car r2), r2 := (cdr r2)
(define (op-pop r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (and (nil? a)
               (not (atom? b)))
          (run 'op-pop state
               (op-swap-cdr fr r2)
               (op-swap r2 fr)
               (op-swap-car r1 fr))
          (reg-set state c 'op-pop-error)))))

;; Functions (fn args ... result):

(define (fn-free r1)
  (lambda (state)
    (trace 'fn-free state)
    (let ((a (reg state r1)))
      (if (not (nil? a))
          (if (atom? a)
              (run 'fn-free-atom state
                   (op-set r1 'nil))
              (run 'fn-free-non-atom state
                   ;; Save state.
                   (op-push sp t1)
                   ;; Compute (cdr r1).
                   (op-pop t1 r1)
                   (fn-free r1) ;; Free (cdr r1).
                   (op-swap t1 r1)
                   (fn-free r1) ;; Free (car r1).
                   ;; Restore state.
                   (op-pop t1 sp)))
          (trace 'fn-free-result-nil state)))))

(define (fn-copy r1 r2)
  (lambda (state)
    (trace 'fn-copy state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (nil? b)
          (if (not (nil? a))
              (if (atom? a)
                  (run 'fn-copy-atom state
                       (op-assign r2 r1))
                  (run 'fn-copy-non-atom state
                       ;; Save state.
                       (op-push sp t1)
                       (op-push sp t2)
                       ;; Compute the (cdr r1)
                       (op-pop t1 r1)
                       (fn-copy r1 r2) ;; Copy (cdr r1).
                       (op-swap t1 r1)
                       (op-swap t2 r2) ;; Result is stored in r2.
                       (fn-copy r1 r2) ;; Copy (car r1).
                       (op-swap t1 r1)
                       (op-swap t2 r2)
                       ;; Restore the argument.
                       (op-push r1 t1)
                       (op-push r2 t2)
                       ;; Restore state.
                       (op-pop t2 sp)
                       (op-pop t1 sp)))
              (trace 'fn-copy-result-nil state))
          (reg-set state c 'fn-copy-error)))))

(define (fn-equal? r1 r2 r3)
  (lambda (state)
    (trace 'fn-equal? state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (or (and (atom? a)
                   (not (atom? b)))
              (and (atom? b)
                   (not (atom? a))))
          (run 'fn-equal?-mismatched state
               (op-set r3 'nil))
          (if (and (atom? a)
                   (atom? b))
              (run 'fn-equal?-eq state
                   (op-eq? r1 r2)
                   (op-swap c r3))
              (run 'fn-equal?-non-eq state
                   ;; Save state.
                   (op-push sp t1)
                   (op-push sp t2)
                   (op-push sp t3)
                   ;; Compute (car r1) & (car r2).
                   (op-pop t1 r1)
                   (op-pop t2 r2)
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
                   (op-push r1 t1)
                   (op-push r2 t2)
                   (op-set t3 'nil)
                   ;; Restore state.
                   (op-pop t3 sp)
                   (op-pop t2 sp)
                   (op-pop t1 sp)))))))

(define (fn-cons r1 r2 r3)
  (lambda (state)
    (run 'fn-cons state
         ;; Save state.
         (op-push sp t1)
         ;; Check proper list condition.
         (op-atom? r2)
         (op-not c)
         (op-swap c t1)
         (op-nil? r2)
         (op-or t1 c)
         (op-jmp-if-nil ':raise-error)
         ;; Actually cons the value.
         (op-swap r3 r2)
         (op-push r3 r1)
         (op-jmp ':end)
         ':raise-error
         (op-set c 'fn-cons-error)
         ':end
         ;; Restore state.
         (op-set t1 'nil)
         (op-pop t1 sp))))

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
     (op-set r3 'herp)
     ':halt)

(run 'ops (init-state 4)
     (op-set r2 'true)
     (op-null? r1)
     (op-swap c r1)
     (op-eq? r1 r2)
     (op-atom? r2)
     (op-set sp 'hello)
     (op-assign c sp)
     (op-swap-car c fr)
     (op-null? c)
     (op-swap-cdr sp fr)
     (op-push r2 r1)
     (op-set r1 'nil)
     (op-pop r1 r2))

(run 'cons (init-state 5)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (op-set r1 3)
     (fn-cons r1 r3 r3))

(run 'free (init-state 4)
     (op-set r1 1)
     (fn-cons r1 r2 r3)
     (op-set r1 2)
     (fn-cons r1 r3 r3)
     (fn-free r3))

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
     (fn-copy r3 r2)
     (fn-equal? r3 r2 r1)
     (op-set r1 3)
     (fn-cons r1 r3 r3)
     (fn-equal? r3 r2 r1))
