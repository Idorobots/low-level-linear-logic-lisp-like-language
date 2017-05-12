;;; Linear-typed Lisp assembler

(load "utils.scm")

;; Instructions

(define (instruction repr exec)
  (list repr exec))

(define instruction-repr car)
(define instruction-exec cadr)

(define (break tag)
  (lambda (labels)
    (instruction
     `(break ,tag)
     (lambda (state)
       (debug tag state)))))

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

(define (set-pc-jmp state value)
  ;; Accomodates the pc increment when running.
  (reg-set state pc (- value 1)))

;; Assembly & running:

(define (assemble startup code)
  (define (label-index ops label)
    (let loop ((i 0)
               (remaining ops))
      (cond ((null? remaining) (error (format "Nonexistant label ~s" label)))
            ((equal? label (car remaining)) i)
            ((symbol? (car remaining)) (loop i (cdr remaining)))
            (':else (loop (+ i 1) (cdr remaining))))))
  (let* ((flat (flatten (list ':init
                              (mc-call startup)
                              (op-halt)
                              code)))
         (labels (cons (cons ':halt :halt)
                       (map (lambda (label)
                              (cons label (label-index flat label)))
                            (filter symbol? flat))))
         (assembly (map (lambda (op)
                          (op labels))
                        (filter (lambda (x)
                                  (not (symbol? x)))
                                flat))))
    (list labels
          (map instruction-repr assembly)
          (map instruction-exec assembly))))

(define (run tag state startup code)
  (define (make-tag base offset)
    (if (<= offset 0)
        base
        (tagged base (format "+~s" offset))))
  (define (compute-labels labels ops)
    (let loop ((ls (reverse labels))
               (offset (- (length ops) 1))
               (acc '()))
      (cond ((< offset 0) acc)
            ((< offset (cdar ls)) (loop (cdr ls) offset acc))
            (':else (loop ls
                          (- offset 1)
                          (cons (make-tag (caar ls) (- offset (cdar ls)))
                                acc))))))
  (let* ((assembled (assemble startup code))
         (labels (car assembled))
         (disasm (cadr assembled))
         (ops (caddr assembled))
         (op-labels (compute-labels labels ops)))
    (debug (tagged tag "-labels") labels)
    (debug (tagged tag "-n-ops") (length ops))
    (let loop ((state state))
      (define (inc-pc state)
        (reg-set state pc (+ 1 (reg state pc))))
      (define (trace state pc)
        (display (format "~s\t~s\t~s~n"
                         (list-ref op-labels pc)
                         (list-ref disasm pc)
                         state))
        state)
      (let ((curr-pc (reg state pc)))
        (if (equal? :halt curr-pc)
            (debug (tagged tag "-result") state)
            (--> state
                 (trace curr-pc)
                 ((list-ref ops curr-pc))
                 (inc-pc)
                 (loop)))))))

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
(define (op-add r1 r2) (make-op-math 'op-add + r1 r2))
(define (op-sub r1 r2) (make-op-math 'op-sub - r1 r2))
(define (op-mul r1 r2) (make-op-math 'op-mul * r1 r2))
(define (op-div r1 r2) (make-op-math 'op-div / r1 r2))

;; Macros:

;; Does nothing.
(define (mc-noop)
  (op-swap c c))

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
  (let ((:else-label (gen-label ':else))
        (:end-label (gen-label ':end)))
    (list cond
          (op-jmp-if-nil :else-label)
          then
          (op-jmp :end-label)
          :else-label
          else
          :end-label)))

(define (mc-when cond . body)
  (mc-if cond
         body
         (mc-noop)))

(define (mc-not expr)
  (list expr
        (op-not c)))

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
