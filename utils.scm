;; Utils

(define-syntax -->
  (syntax-rules ()
    ((--> expr) expr)
    ((--> expr (fn args ...) clauses ...) (--> (fn expr args ...) clauses ...))
    ((--> expr clause clauses ...) (--> (clause expr) clauses ...))))

(define (debug tag value)
  (display tag)
  (display ": ")
  (display value)
  (newline))

(define (trace tag state)
  (debug tag state)
  state)

(define (break tag)
  (lambda (labels)
    (lambda (state)
      (trace tag state))))

(define (atom? a)
  (or (symbol? a) (number? a)))

(define (nil? a)
  (eq? a 'nil))

(define (label-offset labels label)
  (let ((off (assoc label labels)))
    (if (null? off)
        :halt
        (cdr off))))

(define (tagged tag suffix)
  (string->symbol (string-append (symbol->string tag) suffix)))

(define (gen-label label)
  (string->symbol (string-append (symbol->string label)
                                 "-"
                                 (symbol->string (gensym)))))

(define (reg-assert state r value)
  (if (equal? (reg state r) value)
      state
      (error (format "Register ~s was not equal to ~s in ~s" r value state))))

(define (make-cells n)
  (if (equal? n 1)
      'nil
      (cons 'nil (make-cells (- n 1)))))
