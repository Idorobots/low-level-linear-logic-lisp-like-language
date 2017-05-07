;;; Linear-typed Lisp assembler

(load "utils.scm")

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
          state
          (begin (trace tag state)
                 (let ((result ((list-ref ops curr-pc)
                                (set-pc state 0)))) ;; FIXME Needed in order to support functions.
                   (do-run tag
                           (if (number? result)
                               (set-pc state result)
                               (set-pc result (+ 1 curr-pc)))
                           ops))))))
  (trace (tagged tag "-result")
         (do-run tag state
                 (assemble (tagged tag "-assembly")
                           (append code
                                   (list (op-halt)
                                         ':halt))))))

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
