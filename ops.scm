;; VM instructions

(load "utils.scm")
(load "vm.scm")

;; Instructions

(define (instruction repr exec)
  (list repr exec))

(define instruction-repr car)
(define instruction-exec cadr)

(define-syntax define-op
  (syntax-rules (->)
    ((define-op (name args ...) -> (asm-args ...) -> (exec-args ...)
       body ...)
     (define (name args ...)
       (lambda (asm-args ...)
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
         (if (and (number? r1)
                  (number? r2))
             (reg-set state r1 (op a b))
             (error 'op-math-error)))))))

;; Opcodes (op dest src ...):

;; pc := (address-of label)
(define-op (op-jmp label) -> (labels) -> (state)
  (set-pc-jmp state (label-offset labels label)))

;; pc := (address-of label) if (not (nil? c))
(define-op (op-br label) -> (labels) -> (state)
  (if (nil? (reg state c))
      state
      (set-pc-jmp state (label-offset labels label))))

;; c := (nil? r)
(define-op (op-nil? r) -> (labels) -> (state)
  (if (nil? (reg state r))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; c := (atom? r)
(define-op (op-atom? r) -> (labels) -> (state)
  (if (atom? (reg state r))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; c := (eq? r1 r2)
(define-op (op-eq? r1 r2) -> (labels) -> (state)
  (if (eq? (reg state r1) (reg state r2))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; c := (not r)
(define-op (op-not r) -> (labels) -> (state)
  (if (nil? (reg state r))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; c := (and r1 r2)
(define-op (op-and r1 r2) -> (labels) -> (state)
  (if (and (not (nil? (reg state r1)))
           (not (nil? (reg state r2))))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; c := (or r1 r2)
(define-op (op-or r1 r2) -> (labels) -> (state)
  (if (or (not (nil? (reg state r1)))
          (not (nil? (reg state r2))))
      (reg-set state c 'true)
      (reg-set state c 'nil)))

;; r := atom
(define-op (op-set r atom) -> (labels) -> (state)
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
(define-op (op-swap-car r1 r2) -> (labels) -> (state)
  (let ((a (reg state r1))
        (b (reg state r2)))
    (if (and (not (equal? r1 r2))
             (not (atom? b)))
        (--> state
             (reg-set r1 (car b))
             (reg-set r2 (cons a (cdr b))))
        (error 'op-swap-car-error))))

;; tmp := r1, r1 := (cdr r2), (cdr r2) := tmp
(define-op (op-swap-cdr r1 r2) -> (labels) -> (state)
  (let ((a (reg state r1))
        (b (reg state r2)))
    (if (and (not (equal? r1 r2))
             (not (atom? b)))
        (--> state
             (reg-set r1 (cdr b))
             (reg-set r2 (cons (car b) a)))
        (error 'op-swap-cdr-error))))

;; r1 := r1 op r2
(define-math-op (op-add r1 r2) +)
(define-math-op (op-sub r1 r2) -)
(define-math-op (op-mul r1 r2) *)
(define-math-op (op-div r1 r2) /)
