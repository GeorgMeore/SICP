(define (expand-question-marks exp)
  (define (expand-question-mark symbol)
    (let ((chars (symbol->string symbol)))
      (if (string=? (substring chars 0 1) "?")
          (list '? (string->symbol (substring chars 1 (string-length chars))))
          symbol)))
  (define (map-over-symbols proc exp)
    (cond ((pair? exp)
            (cons (map-over-symbols proc (car exp))
                  (map-over-symbols proc (cdr exp))))
          ((symbol? exp) (proc exp))
          (else exp)))
  (map-over-symbols expand-question-mark exp))

(define (contract-question-mark var)
  (string->symbol
    (string-append "?"
      (if (number? (cadr var))
          (string-append (symbol->string (caddr var))
                         "-"
                         (number->string (cadr var)))
          (symbol->string (cadr var))))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define rule-counter 0)

(define (new-rule-application-id!)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (reset-rule-counter!)
  (set! rule-counter 0))

(define (make-variable var id)
  (cons '? (cons id (cdr var))))

; (assert! <body>)
(define (assertion? exp)
  (tagged-list? exp 'assert!))

(define (assertion-body exp)
  (cadr exp))

; (and <conj1> ... <conjn>)
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp)
  (cdr exp))

; (or <disj1> ... <disjn>)
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses exp)
  (cdr exp))

; (not <query>)
(define (not? exp)
  (tagged-list? exp 'not))

(define (not-query exp)
  (cadr exp))

; (lisp-value . <call>)
(define (lisp-value? exp)
  (tagged-list? exp 'lisp-value))

(define (lisp-value-call exp)
  (cdr exp))

; (? <name>)
(define (var? exp)
  (tagged-list? exp '?))

; (always-true)
(define (always-true? exp)
  (tagged-list? exp 'always-true))

; (rule <conclusion> <body>)
; (rule <conclusion>)
(define (rule? stmt)
  (tagged-list? stmt 'rule))

(define (rule-conclusion rule)
  (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))
