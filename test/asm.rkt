;; Assembly utils tests

#lang racket

(require "../src/utils.rkt")
(require "../src/vm.rkt")
(require "../src/asm.rkt")
(require "utils.rkt")

(assert-equal? (--> (init-state 1)
                    (set-pc-jmp 23)
                    (inc-pc))
               (--> (init-state 1)
                    (reg-set pc 23)))

(test-error (label-index '() ':test-1))

(test-error (label-index '(:test-1) ':test-2))

(assert-equal? (label-index '(:test-1 1 2 3 :test-2) ':test-1)
               0)

(assert-equal? (label-index '(:test-1 1 2 3 :test-2) ':test-2)
               3)

(test-error (label-offset '() ':test-1))

(test-error (label-offset '((:test-1 . 23)) ':test-2))

(assert-equal? (label-offset '((:test-1 . 23)
                               (:test-2 . 5))
                             ':test-1)
               23)

(assert-equal? (compute-labels '((:test-1 . 0)
                                 (:test-2 . 3))
                               '(1 2 3))
               '(:test-1
                 :test-1+1
                 :test-1+2))

(assert-equal? (compute-labels '((:test-1 . 0)
                                 (:test-2 . 3))
                               '(1 2 3 4 5))
               '(:test-1
                 :test-1+1
                 :test-1+2
                 :test-2
                 :test-2+1))
