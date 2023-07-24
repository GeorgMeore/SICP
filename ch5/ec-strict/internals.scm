(define (true? x)
  (not (eq? x 'false)))


(define (make-error message . objects)
  (cons 'error (cons message objects)))
(define (error-message err)
  (cadr err))
(define (error-objects err)
  (cddr err))

(define (make-value result)
  (cons 'value result))
(define (value-object succ)
  (cdr succ))

(define (error? result)
  (tagged-list? result 'error))


(define (op+ . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply + args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (op- . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply - args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (op* . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply * args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (op/ . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply / args)))
          ((number? (car nums))
            (if (= (car nums) 0)
                (make-error "Zero division error" (car nums))
                (check (cdr nums))))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (op= . args)
  (define (check nums)
    (cond ((null? nums)
            (make-value (if (apply = args) 'true 'false)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (op-cons . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (make-value (cons (car args) (cadr args)))
        (if (> argc 2)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (op-car . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? arg)
            (make-value (car (car args)))
            (make-error "Not a pair" (car args)))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (op-cdr . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? arg)
            (make-value (cdr (car args)))
            (make-error "Not a pair" (car args)))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (op-null? . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (make-value (if (null? (car args)) 'true 'false))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (op-eq? . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (make-value (if (eq? (car args) (cadr args)) 'true 'false))
        (if (> argc 2)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define primitive-procedures
  (list
    (cons '+ op+)
    (cons '- op-)
    (cons '* op*)
    (cons '/ op/)
    (cons '= op=)
    (cons 'cons op-cons)
    (cons 'car op-car)
    (cons 'cdr op-cdr)
    (cons 'null? op-null?)
    (cons 'eq? op-eq?)
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

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (procedure-parameters proc)
  (cadr proc))
(define (procedure-body proc)
  (caddr proc))
(define (procedure-environment proc)
  (cadddr proc))


(define (make-frame vars vals)
  (cond ((and (null? vars) (null? vals))
          '((* *))) ; empty frame
        ((null? vars) "Too many arguments supplied")
        ((null? vals) "Too few arguments supplied")
        (else
          (let ((result (make-frame (cdr vars) (cdr vals))))
            (if (string? result)
                result
                (cons (cons (car vars) (car vals)) result))))))

(define (add-binding! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (get-binding var frame)
  (cond ((null? frame) '())
        ((eq? (caar frame) var)
          (car frame))
        (else
          (get-binding var (cdr frame)))))

(define the-empty-environment
  '())

(define (first-frame env)
  (car env))
(define (enclosing-environment env)
  (cdr env))

(define (extend-environment vars vals base-env)
  (let ((result (make-frame vars vals)))
    (if (string? result)
        (make-error result)
        (make-value (cons result base-env)))))

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
    (lambda (binding)
      (make-value (cdr binding)))
    (lambda ()
      (make-error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (scan-environment var env
    (lambda (binding)
      (set-cdr! binding val)
      (make-value 'ok))
    (lambda ()
      (make-error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((binding (get-binding var (first-frame env))))
    (if (null? binding)
        (add-binding! var val (first-frame env))
        (set-cdr! binding val))))

(define (setup-environment)
  (let ((initial-environment
         (value-object
           (extend-environment (primitive-procedure-names)
                               (primitive-procedure-objects)
                               the-empty-environment))))
    (define-variable! 'true 'true initial-environment)
    (define-variable! 'false 'false initial-environment)
    (define-variable! 'nil '() initial-environment)
    initial-environment))


(define (display-object obj)
  (if (compound-procedure? obj)
    (display (list 'procedure (procedure-parameters obj)))
    (display obj)))

(define (print-object obj)
  (display-object obj)
  (newline))

(define (print-error err)
  (display "error: ")
  (cond ((string? err)
          (display err))
        (else
          (display (error-message err))
          (for-each
            (lambda (obj)
              (display " ")
              (display-object obj))
            (error-objects err))))
  (newline))
