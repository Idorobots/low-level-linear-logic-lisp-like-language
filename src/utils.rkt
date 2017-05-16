;; Utils

#lang racket

(provide (all-defined-out))

(define-syntax -->
  (syntax-rules ()
    ((--> expr) expr)
    ((--> expr (fn args ...) clauses ...) (--> (fn expr args ...) clauses ...))
    ((--> expr clause clauses ...) (--> (clause expr) clauses ...))))

(define (debug tag value)
  (display (format "~s: ~s~n" tag value))
  value)

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (nil? a)
  (eq? a 'nil))

(define (error-fmt fmt . args)
  (error (apply format fmt args)))

(define (label-offset labels label)
  (let ((off (assoc label labels)))
    (if (false? off)
        (error-fmt "Nonexistent label ~s" label)
        (cdr off))))

(define (tagged tag suffix)
  (string->symbol (string-append (symbol->string tag) suffix)))

(define (gen-label label)
  (string->symbol (string-append (symbol->string label)
                                 "-"
                                 (symbol->string (gensym)))))

(define (make-cells n)
  (if (equal? n 1)
      'nil
      (cons 'nil (make-cells (- n 1)))))

(define (repeat thing n)
  (if (<= n 0)
      '()
      (cons thing (repeat thing (- n 1)))))

(define (right-pad str n ch)
  (if (<= n (string-length str))
      str
      (string-append (right-pad str (- n (string-length ch)) ch) ch)))

(define zip-with map)

(define (zip as bs)
  (zip-with cons as bs))

(define (scan-with f init as)
  (cdr (reverse (foldl (lambda (a s)
                         (cons (f a (car s))
                               s))
                       (list init)
                       as))))

(define (scan init as)
  (scan-with cons init as))
