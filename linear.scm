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

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (nil? a)
  (eq? a 'nil))

;; State: (C R1 R2 SP FR)

(define c 0)
(define r1 1)
(define r2 2)
(define t1 3)
(define t2 4)
(define sp 5)
(define fr 6)

(define (state c r1 r2 t1 t2 sp fr)
  (list c r1 r2 t1 t2 sp fr))

(define (init-state cells)
  (define (make-cells n)
    (if (equal? n 1)
        'nil
        (cons 'nil (make-cells (- n 1)))))
  (state 'init                ;; Initial state.
         'nil 'nil            ;; Main registers.
         'nil 'nil            ;; Temp registers.
         'nil                 ;; Stack pointer.
         (make-cells cells))) ;; Free list.

(define (reg state r)
  (list-ref state r))

(define (reg-set state r value)
  (if (equal? r 0)
      (cons value (cdr state))
      (cons (car state)
            (reg-set (cdr state) (- r 1) value))))

;; Opcodes:

(define (op-null? r)
  (lambda (state)
    (if (nil? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-atom? r)
  (lambda (state)
    (if (atom? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-eq? r1 r2)
  (lambda (state)
    (if (eq? (reg state r1) (reg state r2))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-set r1 atom)
  (lambda (state)
    (if (and (atom? (reg state r1))
             (atom? atom))
        (reg-set state r1 atom)
        (reg-set state c 'op-set-error))))

(define (op-assign r1 r2)
  (lambda (state)
    (let ((a (reg state r2)))
      (if (and (atom? (reg state r1))
               (atom? a))
          (reg-set state r1 a)
          (reg-set state c 'op-assign-error)))))

(define (op-swap r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (--> state
           (reg-set r1 b)
           (reg-set r2 a)))))

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

(define (op-cons r1 r2)
  (lambda (state)
    (if (not (equal? r1 r2))
        (--> state
             ((op-swap-car r1 fr))
             ((op-swap r2 fr))
             ((op-swap-cdr fr r2)))
        (reg-set state c 'op-cons-error))))

(define (op-push r1 r2)
  (op-cons r1 r2))

(define (op-pop r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (and (nil? a)
               (not (atom? b)))
          (--> state
               ((op-swap-cdr fr r2))
               ((op-swap r2 fr))
               ((op-swap-car r1 fr)))
          (reg-set state c 'op-pop-error)))))

;; Functions:

(define (fn-free r1) ;; NOTE Argument has to be passed as the r1.
  (lambda (state)
    (trace 'fn-free state)
    (let ((a (reg state r1)))
      (if (not (nil? a))
          (if (atom? a)
              (trace 'fn-free-result-atom
                     ((op-set r1 'nil) state))
              (trace 'fn-free-result-non-atom
                     (--> state
                          ((op-push r2 sp))
                          ((op-pop r2 r1))
                          ((fn-free r1))
                          ((op-swap r2 r1))
                          ((fn-free r1))
                          ((op-pop r2 sp)))))
          (trace 'fn-free-result-nil state)))))

(define (fn-copy r1 r2)
  (lambda (state)
    (trace 'fn-copy state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (if (nil? b)
          (if (not (nil? a))
              (if (atom? a)
                  (trace 'fn-copy-result-atom
                         ((op-assign r2 r1) state))
                  (trace 'fn-copy-result-non-atom
                         (--> state
                              ((op-push t1 sp))
                              ((op-push t2 sp))
                              ((op-pop t1 r1))
                              ((fn-copy r1 r2))
                              ((op-swap t1 r1))
                              ((op-swap t2 r2))
                              ((fn-copy r1 r2))
                              ((op-swap t1 r1))
                              ((op-swap t2 r2))
                              ((op-push t1 r1))
                              ((op-push t2 r2))
                              ((op-pop t2 sp))
                              ((op-pop t1 sp))
                              )))
              (trace 'fn-copy-result-nil state))
          (reg-set state c 'fn-copy-error)))))

(define (fn-equal? r1 r2)
  ???)

;; Run:

(define (run state opcodes)
  (foldl (lambda (o s)
           (trace 'run s)
           (o s))
         state
         opcodes))

;; Examples:

(trace 'result
 (run
  (init-state 4)
  (list (op-set r2 'true)
        (op-null? r1)
        (op-swap c r1)
        (op-eq? r1 r2)
        (op-atom? r2)
        (op-set sp 'hello)
        (op-assign c sp)
        (op-swap-car c fr)
        (op-null? c)
        (op-swap-cdr sp fr)
        (op-cons r1 r2)
        (op-set r1 'nil)
        (op-pop r1 r2))))

(trace 'result
 (run
  (init-state 4)
  (list (op-set r1 1)
        (op-cons r1 r2)
        (op-set r1 2)
        (op-cons r1 r2)
        (op-swap r1 r2)
        (fn-free r1))))

(trace 'result
 (run
  (init-state 7)
  (list (op-set r1 1)
        (op-cons r1 r2)
        (op-set r1 2)
        (op-cons r1 r2)
        (op-swap r1 r2)
        (fn-copy r1 r2))))
