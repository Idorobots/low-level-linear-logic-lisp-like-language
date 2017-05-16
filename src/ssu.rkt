;; Static Single Use form

#lang racket

(provide (all-defined-out))

(require "utils.rkt")

;; Utils

(define (variable? v)
  (and (symbol? v)
       (equal? (string-ref (symbol->string v) 0)
               #\$)))

;; (op ret . args)

(define (ret op)
  (if (variable? (cadr op))
      (list (cadr op))
      '()))

(define (args op)
  (filter variable? (cddr op)))

(define (contains? as a)
  (cond ((null? as) #f)
        ((equal? a (car as)) #t)
        (else (contains? (cdr as) a))))

(define (diff as bs)
  (filter (lambda (a)
            (contains? bs a))
          as))

;; SSU computation

(define (liveliness ops existing returned)
  (let ((introduced (map flatten (scan existing (map ret ops))))
        (required (reverse (map flatten (scan returned (map args (reverse ops)))))))
    (zip-with list
              ops
              (zip-with diff
                        introduced
                        required))))
