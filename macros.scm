;; ASM macros

(load "utils.scm")
(load "ops.scm")


;; Halts the VM.
(define (mc-halt)
  (op-jmp ':halt))

;; Does nothing.
(define (mc-noop)
  (op-swap c c))

;; Jumps
(define (mc-br-if-nil label)
  (list (op-nil? c)
        (op-br label)))

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

;; Conditionals
(define (mc-if cond then else)
  (let ((:then-label (gen-label ':then))
        (:end-label (gen-label ':end)))
    (list cond
          (op-br :then-label)
          else
          (op-jmp :end-label)
          :then-label
          then
          :end-label)))

(define (mc-when cond . body)
  (mc-if cond
         body
         (mc-noop)))

(define (mc-not expr)
  (list expr
        (op-nil? c)))

(define (mc-and tmp expr-a expr-b)
  (list expr-a
        (op-swap c tmp)
        expr-b
        (op-and c tmp)))

(define (mc-or tmp expr-a expr-b)
  (list expr-a
        (op-swap c tmp)
        expr-b
        (op-or c tmp)))

;; Register spilling
(define (mc-spill regs . body)
  (list (map (lambda (r)
               (mc-push sp r))
             regs)
        body
        (map (lambda (r)
               (mc-pop r sp))
             (reverse regs))))

;; Function calling
;; FIXME Loose these in favour of CPS.
(define (mc-ret)
  (list (mc-pop c sp)
        (op-swap c pc)))

(define (mc-call label . args)
  (let* ((tmps (take (list t1 t2 t3) (length args)))
         (reordered (take (list r1 r2 r3) (length args)))
         (prep (flatten (list (mc-push sp c)
                              (op-jmp label))))
         (call (list (op-set c (length prep))
                     (op-add c pc)
                     prep)))
    (if (equal? args reordered)
        call
        ;; Needs args reordering first...
        (mc-spill tmps
                  ;; Reorder the args to support proper calling convention.
                  (map op-swap args tmps)
                  (map op-swap reordered tmps)
                  call
                  ;; Restore the arguments & return value ordering.
                  ;; NOTE Needs to restore all registers to support functions not taking ownership.
                  (map op-swap reordered tmps)
                  (map op-swap args tmps)))))

(define (mc-define name . body)
  (list name
        body
        (mc-ret)))
