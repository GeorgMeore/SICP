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


; (let ((<name1> <exp1>)...(<namen> <expn>)) <body>)
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-names exp)
  (map car (cadr exp)))
(define (let-exps exp)
  (map cadr (cadr exp)))
(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-names exp) (let-body exp))
        (let-exps exp)))


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
(define (make-if cond cons alt)
  (list 'if cond cons alt))


; (begin <actions>...)
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (make-begin seq)
  (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


; (cond (<cond1> <seq1>)...(<condn>|else <seqn>))
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (clause-cond clause)
  (car clause))
(define (clause-exp clause)
  (sequence->exp (cdr clause)))

(define (expand-clauses clauses)
  (cond ((null? clauses)
          'false)
        ((eq? (clause-cond (car clauses)) 'else)
          (clause-exp (car clauses)))
        (else
          (make-if (clause-cond (car clauses))
                   (clause-exp (car clauses))
                   (expand-clauses (cdr clauses))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


; (<operator> <operands>...)
(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))
(define (last-operand? ops)
  (null? (cdr ops)))
