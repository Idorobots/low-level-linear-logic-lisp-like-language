;;; Linear-typed Lisp

;; Utils:

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
  ???)

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
  (foldl (lambda (s o)
           (trace s)
           (o s))
         state
         opcodes))
