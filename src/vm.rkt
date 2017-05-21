;; VM & runtime state

#lang racket

(provide (all-defined-out))

(require "utils.rkt")

(define :halt -1)

(define pc 0)
(define tpc 1) ;; FIXME Get rid of this.
(define r0 2)
(define r1 3)
(define r2 4)
(define r3 5)
(define t0 6)
(define t1 7)
(define t2 8)
(define t3 9)
(define sp 10)
(define fr 11)

(define (state pc tpc r0 r1 r2 r3 t0 t1 t2 t3 sp fr)
  (list pc tpc r0 r1 r2 r3 t0 t1 t2 t3 sp fr))

(define (init-state cells)
  (state 0
         0
         'nil 'nil 'nil 'nil  ;; Main registers.
         'nil 'nil 'nil 'nil  ;; Temp registers.
         'nil                 ;; Stack pointer.
         (make-cells cells))) ;; Free list.

(define (state-format state)
  (define (n-cells cells)
    (if (pair? cells)
        (+ 1 (n-cells (cdr cells)))
        0))
  (string-append
   (apply format
          "pc=~s, tpc=~s, r0=~s, r1=~s, r2=~s, r3=~s, t0=~s, t1=~s, t2=~s, t3=~s, sp=~s, "
          (take state (- (length state) 1)))
   (let ((v (last state)))
     (if (atom? v)
         (format "fr=~s" v)
         (format "fr: ~s cells" (n-cells v))))))

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
