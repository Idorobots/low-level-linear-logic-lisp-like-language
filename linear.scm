;;; Linear-typed Lisp

;; Utils:

(define-syntax -->
  (syntax-rules ()
    ((--> expr) expr)
    ((--> expr (fn args ...) clauses ...) (--> (fn expr args ...) clauses ...))
    ((--> expr clause clauses ...) (--> (clause expr) clauses ...))))

(define (trace state)
  (display state)
  (newline))

;; State: (C R1 R2 SP FR)

(define c 0)
(define r1 1)
(define r2 2)
(define sp 3)
(define fr 4)

(define (state c r1 r2 sp fr)
  (list c r1 r2 sp fr))

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
    (if (equal? (reg state r) 'nil)
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-atom? r)
  (lambda (state)
    (if (symbol? (reg state r))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-eq? r1 r2)
  (lambda (state)
    (if (eq? (reg state r1) (reg state r2))
        (reg-set state c 'true)
        (reg-set state c 'nil))))

(define (op-swap r1 r2)
  (lambda (state)
    (let ((a (reg state r1))
          (b (reg state r2)))
      (--> state
          (reg-set r1 b)
          (reg-set r2 a)))))

(define (op-swap-car r1 r2)
  ???)

(define (op-swap-cdr r1 r2)
  ???)

(define (op-set r1 atom)
  ???)

(define (op-assign r1 r2)
  ???)

(define (op-cons r1 r2)
  ???)

(define (op-push r1 r2)
  (op-cons r1 r2))

(define (op-pop r1 r2)
  ???)

;; Run:

(define (run state opcodes)
  (foldl (lambda (o s)
           (trace s)
           (o s))
         state
         opcodes))

;; Examples:

(trace
 (run
  (state 'nil 'nil 'nil 'nil 'nil)
  (list (op-null? c)
        (op-swap c r1)
        (op-atom? r1)
        (op-null? r1)
        (op-eq? c r1))))
