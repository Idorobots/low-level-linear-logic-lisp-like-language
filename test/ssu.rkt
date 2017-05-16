;; SSU Tests

#lang racket

(provide (all-defined-out))

(require "../src/ssu.rkt")

(define simple-ops
  '((op-set $a 1)
    (op-set $b 2)
    (op-add $c $a $b)
    (op-set $d 3)
    (op-mul $e $c $d)
    (op-eq? $f $e 9)))

(define free-ops
  '((op-nil? $is-nil $arg)
    (op-nil? $not-is-nil $is-nil)
    (op-br _ $not-is-nil :not-is-nil :is-nil)
    (op-jmp _ :end)                               ;; :is-nil
    (op-atom? $is-atom $arg)                    ;; :not-is-nil
    (op-br _ $is-atom :is-atom :not-is-atom)
    (op-car $car $arg)                          ;; :not-is-atom
    (op-call $car-free :fn-free $car)
    (op-cdr $cdr $arg)
    (op-call $cdr-free :fn-free $cdr)
    (op-jmp _ :end)
    (op-ret _)))                                ;; :end, :is-nil

(define cons-ops
  '((op-atom? $is-atom $arg2)
    (op-nil? $not-is-atom $is-atom)
    (op-nil? $is-nil $arg2)
    (op-or $not-is-atom-or-nil $not-is-atom $is-nil)
    (op-br _ $not-is-atom-or-nil :is-atom-or-nil :not-is-atom-or-nil)
    (op-set $error fn-cons-error)
    (op-ret _ $error)
    (op-cons $cons $arg1 $arg2)                 ;; :not-is-atom-or-nil
    (op-ret _ $cons)))
