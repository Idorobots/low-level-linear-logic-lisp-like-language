;; VM instructions

#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "vm.rkt")

;; Instructions

(define (instruction repr exec)
  (list repr exec))

(define instruction-repr car)
(define instruction-exec cadr)

(define-syntax define-op
  (syntax-rules (->)
    ((define-op (name args ...) -> rest ...)
     (define-op (name args ...) 'ok -> rest ...))
    ((define-op (name args ...) -> (asm-args ...) -> rest ...)
     (define-op (name args ...) 'ok -> (asm-args ...) 'ok -> rest ...))
    ((define-op (name args ...) -> (asm-args ...) asm-setup -> rest ...)
     (define-op (name args ...) 'ok -> (asm-args ...) asm-setup -> rest ...))
    ((define-op (name args ...) validation -> (asm-args ...) -> rest ...)
     (define-op (name args ...) validation -> (asm-args ...) 'ok -> rest ...))
    ((define-op (name args ...) validation
       -> (asm-args ...) asm-setup
       -> (exec-args ...) body ...)
     (define (name args ...)
       validation
       (lambda (asm-args ...)
         asm-setup
         (instruction `(name ,(arg-repr args asm-args ...) ...)
                      (lambda (exec-args ...)
                        body ...)))))))

;; Utils

(define (arg-repr a labels)
  (let* ((regs `((,pc . "pc")
                 (,c . "c")
                 (,r1 . "r1")
                 (,r2 . "r2")
                 (,r3 . "r3")
                 (,t1 . "t1")
                 (,t2 . "t2")
                 (,t3 . "t3")
                 (,sp . "sp")
                 (,fr . "fr")))
         (reg (assoc a regs))
         (addr (assoc a labels)))
    (cond (reg (cdr reg))
          (addr (format "(or '~s ~s)" a (cdr addr)))
          (':else a))))

(define-syntax define-math-op
  (syntax-rules ()
    ((define-math-op (name r1 r2) op)
     (define-op (name r1 r2) -> (labels) -> (state)
       (let ((a (reg state r1))
             (b (reg state r2)))
         (if (and (number? a)
                  (number? b))
             (reg-set state r1 (op a b))
             (error 'op-math-error)))))))

;; Opcodes (op dest src ...):

;; pc := (address-of label)
(define-op (op-jmp label)
  (unless (symbol? label)
    (error-fmt "Invalid op-jmp label ~s" label))

  -> (labels)
  (define off (label-offset labels label))

  -> (state)
  (set-pc-jmp state off))

;; pc := (address-of label) if (not (nil? c))
(define-op (op-br label)
  (unless (symbol? label)
    (error-fmt "Invalid op-br label ~s" label))

  -> (labels)
  (define off (label-offset labels label))

  -> (state)
  (if (nil? (reg state c))
      state
      (set-pc-jmp state off)))

;; c := (nil? r)
(define-op (op-nil? r) -> (labels) -> (state)
  (reg-set state c (if (nil? (reg state r))
                       'true
                       'nil)))

;; c := (atom? r)
(define-op (op-atom? r) -> (labels) -> (state)
  (reg-set state c (if (atom? (reg state r))
                       'true
                       'nil)))

;; c := (eq? r1 r2)
(define-op (op-eq? r1 r2) -> (labels) -> (state)
  (reg-set state c (if (eq? (reg state r1) (reg state r2))
                       'true
                       'nil)))

;; c := (and r1 r2)
(define-op (op-and r1 r2) -> (labels) -> (state)
  (reg-set state c (if (and (not (nil? (reg state r1)))
                            (not (nil? (reg state r2))))
                       'true
                       'nil)))

;; c := (or r1 r2)
(define-op (op-or r1 r2) -> (labels) -> (state)
  (reg-set state c (if (or (not (nil? (reg state r1)))
                           (not (nil? (reg state r2))))
                       'true
                       'nil)))

;; r := atom
(define-op (op-set r atom)
  (unless (atom? atom)
    (error-fmt "Invalid op-set argument ~s" atom))

  -> (labels)
  -> (state)
  (if (and (atom? (reg state r))
           (atom? atom))
      (reg-set state r atom)
      (error 'op-set-error)))

;; r1 := r2
(define-op (op-assign r1 r2) -> (labels) -> (state)
  (let ((a (reg state r2)))
    (if (and (atom? (reg state r1))
             (atom? a))
        (reg-set state r1 a)
        (error 'op-assign-error))))

;; tmp := r1, r1 := r2, r2 := tmp
(define-op (op-swap r1 r2) -> (labels) -> (state)
  (let ((a (reg state r1))
        (b (reg state r2)))
    (--> state
         (reg-set r1 b)
         (reg-set r2 a))))

;; tmp := r1, r1 := (car r2), (car r2) := tmp
(define-op (op-swap-car r1 r2)
  (when (equal? r1 r2)
    (error-fmt "op-swap-car arguments must be different, given ~s" r1))

  -> (labels)
  -> (state)
  (let ((b (reg state r2)))
    (if (not (atom? b))
        (--> state
             (reg-set r1 (car b))
             (reg-set r2 (cons (reg state r1) (cdr b))))
        (error 'op-swap-car-error))))

;; tmp := r1, r1 := (cdr r2), (cdr r2) := tmp
(define-op (op-swap-cdr r1 r2)
  (when (equal? r1 r2)
    (error-fmt "op-swap-cdr arguments must be different, given ~s" r1))

-> (labels)
-> (state)
  (let ((b (reg state r2)))
    (if (not (atom? b))
        (--> state
             (reg-set r1 (cdr b))
             (reg-set r2 (cons (car b) (reg state r1))))
        (error 'op-swap-cdr-error))))

;; r1 := r1 op r2
(define-math-op (op-add r1 r2) +)
(define-math-op (op-sub r1 r2) -)
(define-math-op (op-mul r1 r2) *)
(define-math-op (op-div r1 r2) /)