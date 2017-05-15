;; VM & runtime state

#lang racket

(provide (all-defined-out))

(require "utils.rkt")

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

(define (state-format state)
  (define (n-cells cells)
    (if (pair? cells)
        (+ 1 (n-cells (cdr cells)))
        0))
  (apply format
         "pc=~s, c=~s, r1=~s, r2=~s, r3=~s, t1=~s, t2=~s, t3=~s, sp=~s, fr: ~s cells"
         (append (take state 9)
                 (--> state last n-cells list))))

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
