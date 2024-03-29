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
(define (unwrap succ)
  (cdr succ))

(define (error? result)
  (tagged-list? result 'error))


(define (proc+ . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply + args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc- . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply - args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc* . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply * args)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc/ . args)
  (define (check nums)
    (cond ((null? nums) (make-value (apply / args)))
          ((number? (car nums))
            (if (= (car nums) 0)
                (make-error "Zero division error")
                (check (cdr nums))))
          (else (make-error "Not a number" (car nums)))))
  (if (null? args)
      (make-error "Too few arguments supplied")
      (check args)))

(define (proc-mod . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (let ((first (car args)) (second (cadr args)))
          (cond ((not (integer? first))
                  (make-error "Not an integer" first))
                ((not (integer? second))
                  (make-error "Not an integer" second))
                ((= second 0)
                  (make-error "Zero division error"))
                (else
                  (make-value (remainder first second)))))
        (if (> argc 2)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc= . args)
  (define (check nums)
    (cond ((null? nums)
            (make-value (if (apply = args) 'true 'false)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc> . args)
  (define (check nums)
    (cond ((null? nums)
            (make-value (if (apply > args) 'true 'false)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc< . args)
  (define (check nums)
    (cond ((null? nums)
            (make-value (if (apply < args) 'true 'false)))
          ((number? (car nums))
            (check (cdr nums)))
          (else (make-error "Not a number" (car nums)))))
  (check args))

(define (proc-cons . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (make-value (cons (car args) (cadr args)))
        (if (> argc 2)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-car . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? (car args))
            (make-value (car (car args)))
            (make-error "Not a pair" (car args)))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-cdr . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? (car args))
            (make-value (cdr (car args)))
            (make-error "Not a pair" (car args)))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-null? . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (make-value (if (null? (car args)) 'true 'false))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-eq? . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (make-value (if (eq? (car args) (cadr args)) 'true 'false))
        (if (> argc 2)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-not . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (make-value (if (eq? (car args) 'false) 'true 'false))
        (if (> argc 1)
            (make-error "Too many arguments supplied")
            (make-error "Too few arguments supplied")))))

(define (proc-print . args)
  (for-each
    (lambda (arg)
      (display-object arg))
    args)
  (make-value 'ok))

(define (proc-println . args)
  (for-each
    (lambda (arg)
      (display-object arg))
    args)
  (newline)
  (make-value 'ok))

(define primitive-procedures
  (list
    (cons '+ proc+)
    (cons '- proc-)
    (cons '* proc*)
    (cons '/ proc/)
    (cons 'mod proc-mod)
    (cons '= proc=)
    (cons '> proc>)
    (cons '< proc<)
    (cons 'not proc-not)
    (cons 'cons proc-cons)
    (cons 'car proc-car)
    (cons 'cdr proc-cdr)
    (cons 'null? proc-null?)
    (cons 'eq? proc-eq?)
    (cons 'print proc-print)
    (cons 'println proc-println)
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
        ((null? vars)
          (make-error "Too many arguments supplied"))
        ((null? vals)
          (make-error "Too few arguments supplied"))
        (else
          (let ((result (make-frame (cdr vars) (cdr vals))))
            (if (error? result)
                result
                (cons (cons (car vars) (car vals)) result))))))

(define (add-binding! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (get-binding var frame)
  (cond ((null? (cdr frame)) '())
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
    (if (error? result)
        result
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
         (unwrap
           (extend-environment (primitive-procedure-names)
                               (primitive-procedure-objects)
                               the-empty-environment))))
    (define-variable! 'true 'true initial-environment)
    (define-variable! 'false 'false initial-environment)
    (define-variable! 'nil '() initial-environment)
    initial-environment))


(define (display-object obj)
  (define (scan obj)
    (cond ((compound-procedure? obj)
            '<procedure>)
          ((primitive-procedure? obj)
            '<primitive>)
          ((pair? obj)
            (cons (scan (car obj)) (scan (cdr obj))))
          (else obj)))
  (display (scan obj)))

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


(define (delay-it body env)
  (list 'thunk body env))

(define (thunk? object)
  (tagged-list? object 'thunk))

(define (thunk-body thunk)
  (cadr thunk))
(define (thunk-environment thunk)
  (caddr thunk))

(define (set-thunk-value! thunk value)
  (set-cdr! thunk value)
  (set-car! thunk 'evaluated-thunk))

(define (evaluated-thunk? object)
  (tagged-list? object 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cdr evaluated-thunk))
