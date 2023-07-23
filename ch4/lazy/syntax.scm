(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

; <number>
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))


; <symbol>
(define (variable? exp)
  (symbol? exp))


; (quote <text>)
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))


; (lambda (<params>...) <body>)
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


; (def <var> <value>)
; (def (<var> <params>...) <body>)
(define (definition? exp)
  (tagged-list? exp 'def))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))


; (if <pred> <cons>)
; (if <pred> <cons> <alt>)
(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))


; (begin <actions>...)
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))


; (<operator> <operands>...)
(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))


; (let ((<var1> <exp1>) ... (<varn> <expn>)) <body>)
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-expression exp)
  (cons (make-lambda (map car (cadr exp)) (cddr exp))
        (map cadr (cadr exp))))
