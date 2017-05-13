;; VM instructions

(load "utils.scm")
(load "vm.scm")

;; Instructions

(define (instruction repr exec)
  (list repr exec))

(define instruction-repr car)
(define instruction-exec cadr)

;; Utils

(define (make-op-math name op r1 r2)
  (lambda (labels)
    (instruction
     `(,name ,r1 ,r2)
     (lambda (state)
       (let ((a (reg state r1))
             (b (reg state r2)))
         (if (and (number? r1)
                  (number? r2))
             (reg-set state r1 (op a b))
             (error 'op-math-error)))))))

;; Opcodes (op dest src ...):

;; pc := (address-of :halt)
(define (op-halt)
  (lambda (labels)
    (instruction
     '(halt)
     (lambda (state)
       (set-pc-jmp state :halt)))))

;; pc := (address-of label)
(define (op-jmp label)
  (lambda (labels)
    (let ((off (label-offset labels label)))
      (instruction
       `(jmp ,label)
       (lambda (state)
         (set-pc-jmp state off))))))

;; pc := (address-of label) if c is nil.
(define (op-jmp-if-nil label)
  (lambda (labels)
    (let ((off (label-offset labels label)))
      (instruction
       `(jmp-if-nil ,label)
       (lambda (state)
         (if (nil? (reg state c))
             (set-pc-jmp state off)
             state))))))

;; pc := (address-of label) if r is (not nil)
(define (op-jmp-if-not-nil label)
  (lambda (labels)
    (let ((off (label-offset labels label)))
      (instruction
       `(jmp-if-not-nil ,label)
       (lambda (state)
         (if (not (nil? (reg state c)))
             (set-pc-jmp state off)
             state))))))

;; c := (nil? r)
(define (op-nil? r)
  (lambda (labels)
    (instruction
     `(nil? ,r)
     (lambda (state)
       (if (nil? (reg state r))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; c := (atom? r)
(define (op-atom? r)
  (lambda (labels)
    (instruction
     `(atom? ,r)
     (lambda (state)
       (if (atom? (reg state r))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; c := (eq? r1 r2)
(define (op-eq? r1 r2)
  (lambda (labels)
    (instruction
     `(eq? ,r1 ,r2)
     (lambda (state)
       (if (eq? (reg state r1) (reg state r2))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; c := (not r)
(define (op-not r)
  (lambda (labels)
    (instruction
     `(not ,r)
     (lambda (state)
       (if (nil? (reg state r))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; c := (and r1 r2)
(define (op-and r1 r2)
  (lambda (labels)
    (instruction
     `(and ,r1 ,r2)
     (lambda (state)
       (if (and (not (nil? (reg state r1)))
                (not (nil? (reg state r2))))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; c := (or r1 r2)
(define (op-or r1 r2)
  (lambda (labels)
    (instruction
     `(or ,r1 ,r2)
     (lambda (state)
       (if (or (not (nil? (reg state r1)))
               (not (nil? (reg state r2))))
           (reg-set state c 'true)
           (reg-set state c 'nil))))))

;; r := atom
(define (op-set r atom)
  (lambda (labels)
    (instruction
     `(set ,r ,atom)
     (lambda (state)
       (if (and (atom? (reg state r))
                (atom? atom))
           (reg-set state r atom)
           (error 'op-set-error))))))

;; r1 := r2
(define (op-assign r1 r2)
  (lambda (labels)
    (instruction
     `(assign ,r1 ,r2)
     (lambda (state)
       (let ((a (reg state r2)))
         (if (and (atom? (reg state r1))
                  (atom? a))
             (reg-set state r1 a)
             (error 'op-assign-error)))))))

;; tmp := r1, r1 := r2, r2 := tmp
(define (op-swap r1 r2)
  (lambda (labels)
    (instruction
     `(swap ,r1 ,r2)
     (lambda (state)
       (let ((a (reg state r1))
             (b (reg state r2)))
         (--> state
              (reg-set r1 b)
              (reg-set r2 a)))))))

;; tmp := r1, r1 := (car r2), (car r2) := tmp
(define (op-swap-car r1 r2)
  (lambda (labels)
    (instruction
     `(swap-car ,r1 ,r2)
     (lambda (state)
       (let ((a (reg state r1))
             (b (reg state r2)))
         (if (and (not (equal? r1 r2))
                  (not (atom? b)))
             (--> state
                  (reg-set r1 (car b))
                  (reg-set r2 (cons a (cdr b))))
             (error 'op-swap-car-error)))))))

;; tmp := r1, r1 := (cdr r2), (cdr r2) := tmp
(define (op-swap-cdr r1 r2)
  (lambda (labels)
    (instruction
     `(swap-cdr ,r1 ,r2)
     (lambda (state)
       (let ((a (reg state r1))
             (b (reg state r2)))
         (if (and (not (equal? r1 r2))
                  (not (atom? b)))
             (--> state
                  (reg-set r1 (cdr b))
                  (reg-set r2 (cons (car b) a)))
             (error 'op-swap-cdr-error)))))))

;; r1 := r1 op r2
(define (op-add r1 r2) (make-op-math 'add + r1 r2))
(define (op-sub r1 r2) (make-op-math 'sub - r1 r2))
(define (op-mul r1 r2) (make-op-math 'mul * r1 r2))
(define (op-div r1 r2) (make-op-math 'div / r1 r2))
