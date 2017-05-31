;;; Linear-typed Lisp assembler

#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "vm.rkt")
(require "ops.rkt")
(require "macros.rkt")

;; Utils

(define (inc-pc state)
  (reg-set state pc (+ 1 (reg state pc))))

(define (make-tag base offset)
  (if (<= offset 0)
      base
      (tagged base (format "+~s" offset))))

(define (label-index ops label)
  (let loop ((i 0)
             (remaining ops))
    (cond ((null? remaining) (error (format "Nonexistant label ~s" label)))
          ((equal? label (car remaining)) i)
          ((symbol? (car remaining)) (loop i (cdr remaining)))
          (':else (loop (+ i 1) (cdr remaining))))))

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

;; Assembly

(define (assemble startup code)
  (let* ((flat (flatten (list ':init
                              (mc-call startup r0)
                              (mc-halt)
                              code)))
         (labels (cons (cons ':halt :halt)
                       (map (lambda (label)
                              (cons label (label-index flat label)))
                            (filter symbol? flat))))
         (stripped (filter (lambda (x)
                             (not (symbol? x)))
                           flat))
         (asm (map (lambda (op)
                     ((instruction-asm op) labels))
                   stripped))
         (disasm (map (lambda (op)
                        (instruction-repr op labels))
                      stripped)))
    (list labels asm disasm)))

;; Execution

(define (run tag state startup code)
  (let* ((assembled (assemble startup code))
         (labels (car assembled))
         (ops (cadr assembled))
         (disasm (caddr assembled))
         (op-labels (compute-labels labels ops)))
    (debug (tagged tag "-labels") labels)
    (debug (tagged tag "-n-ops") (length ops))
    (let loop ((state state))
      (define (trace state pc)
        (define (print padding i)
          (--> (format "~a " i)
               (right-pad padding " ")
               (display)))
        (print 20 (list-ref op-labels pc))
        (print 30 (list-ref disasm pc))
        (print 10 (state-format state))
        (newline)
        state)
      (let ((curr-pc (reg state pc)))
        (if (equal? :halt curr-pc)
            (debug (tagged tag "-result") state)
            (--> state
                 (trace curr-pc)
                 ((list-ref ops curr-pc))
                 (inc-pc)
                 (loop)))))))
