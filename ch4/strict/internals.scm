; predicates
(define (false? x)
  (eq? x 'false))

(define (true? x)
  (not (false? x)))


; procedures
(define (wrap-boolean proc)
  (define (wrapped . args)
    (if (apply proc args) 'true 'false))
  wrapped)

(define primitive-procedures
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= (wrap-boolean =))
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? (wrap-boolean null?))
        (cons 'eq? (wrap-boolean eq?))
        (cons 'list list)
  ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cdr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (make-procedure params executor env)
  (list 'procedure params executor env))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-executor proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))


; frames
(define (make-frame vars vals)
  (cond ((and (null? vars) (null? vals))
          '((* *))) ; empty frame
        ((null? vars)
          (error "Too many arguments supplied" vars vals))
        ((null? vals)
          (error "Too few arguments supplied" vars vals))
        (else
          (cons (cons (car vars) (car vals))
                (make-frame (cdr vars) (cdr vals))))))

(define (add-binding! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (get-binding var frame)
  (cond ((null? frame)
          '())
        ((eq? (caar frame) var)
          (car frame))
        (else
          (get-binding var (cdr frame)))))


; environments
(define the-empty-environment
  '())

(define (first-frame env)
  (car env))

(define (enclosing-environment env)
  (cdr env))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals)
        base-env))

(define (scan-environment var env found not-found)
  (define (loop env)
    (if (eq? env the-empty-environment)
        (not-found)
        (let ((binding (get-binding var (first-frame env))))
          (if (null? binding)
              (loop (enclosing-environment env))
              (found binding)))))
  (loop env))

(define (lookup-variable-value var env)
  (scan-environment var env
    (lambda (binding) (cdr binding))
    (lambda () (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (scan-environment var env
    (lambda (binding) (set-cdr! binding val))
    (lambda () (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((binding (get-binding var (first-frame env))))
    (if (null? binding)
        (add-binding! var val (first-frame env))
        (set-cdr! binding val))))

(define (setup-environment)
  (let ((initial-environment
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true 'true initial-environment)
    (define-variable! 'false 'false initial-environment)
    (define-variable! 'nil '() initial-environment)
    initial-environment))
