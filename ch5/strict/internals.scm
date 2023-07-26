(define (true? x)
  (not (eq? x 'false)))


(define the-error '())

(define (op-failed?)
  (not (null? the-error)))

(define (set-error! message . objects)
  (set! the-error (cons message objects)))
(define (clear-error!)
  (set! the-error '()))

(define (error-message)
  (car the-error))
(define (error-objects)
  (cdr the-error))


(define (op+ . args)
  (define (check nums)
    (cond ((null? nums) (apply + args))
          ((number? (car nums))
            (check (cdr nums)))
          (else (set-error! "Not a number" (car nums)))))
  (check args))

(define (op- . args)
  (define (check nums)
    (cond ((null? nums) (apply - args))
          ((number? (car nums))
            (check (cdr nums)))
          (else (set-error! "Not a number" (car nums)))))
  (check args))

(define (op* . args)
  (define (check nums)
    (cond ((null? nums) (apply * args))
          ((number? (car nums))
            (check (cdr nums)))
          (else (set-error! "Not a number" (car nums)))))
  (check args))

(define (op/ . args)
  (define (check nums)
    (cond ((null? nums) (apply / args))
          ((number? (car nums))
            (if (= (car nums) 0)
                (set-error! "Zero division error")
                (check (cdr nums))))
          (else (set-error! "Not a number" (car nums)))))
  (if (null? args)
      (set-error! "Too few arguments supplied")
      (check args)))

(define (op= . args)
  (define (check nums)
    (cond ((null? nums)
            (if (apply = args) 'true 'false))
          ((number? (car nums))
            (check (cdr nums)))
          (else (set-error! "Not a number" (car nums)))))
  (check args))

(define (op-cons . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (cons (car args) (cadr args))
        (if (> argc 2)
            (set-error! "Too many arguments supplied")
            (set-error! "Too few arguments supplied")))))

(define (op-car . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? (car args))
            (car (car args))
            (set-error! "Not a pair" (car args)))
        (if (> argc 1)
            (set-error! "Too many arguments supplied")
            (set-error! "Too few arguments supplied")))))

(define (op-cdr . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (pair? (car args))
            (cdr (car args))
            (set-error! "Not a pair" (car args)))
        (if (> argc 1)
            (set-error! "Too many arguments supplied")
            (set-error! "Too few arguments supplied")))))

(define (op-null? . args)
  (let ((argc (length args)))
    (if (= argc 1)
        (if (null? (car args)) 'true 'false)
        (if (> argc 1)
            (set-error! "Too many arguments supplied")
            (set-error! "Too few arguments supplied")))))

(define (op-eq? . args)
  (let ((argc (length args)))
    (if (= argc 2)
        (if (eq? (car args) (cadr args)) 'true 'false)
        (if (> argc 2)
            (set-error! "Too many arguments supplied")
            (set-error! "Too few arguments supplied")))))

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
        ((null? vars)
          (set-error! "Too many arguments supplied")
          'failed)
        ((null? vals)
          (set-error! "Too few arguments supplied")
          'failed)
        (else
          (let ((rest (make-frame (cdr vars) (cdr vals))))
            (if (eq? rest 'failed)
                rest
                (cons (cons (car vars) (car vals)) rest))))))

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
  (let ((frame (make-frame vars vals)))
    (if (eq? frame 'failed)
        'failed
        (cons frame base-env))))

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
      (cdr binding))
    (lambda ()
      (set-error! "Unbound variable" var))))

(define (set-variable-value! var val env)
  (scan-environment var env
    (lambda (binding)
      (set-cdr! binding val))
    (lambda ()
      (set-error! "Unbound variable" var))))

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


(define (display-object obj)
  (if (compound-procedure? obj)
    (display (list 'procedure (procedure-parameters obj)))
    (display obj)))

(define (print-object obj)
  (display-object obj)
  (newline))

(define (print-error)
  (display "error: ")
  (display (error-message))
  (for-each
    (lambda (obj)
      (display " ")
      (display-object obj))
    (error-objects))
  (newline))
