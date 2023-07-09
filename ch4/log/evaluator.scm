(define (qeval query frame-stream)
  (cond ((and? query)
          (conjoin (and-clauses query) frame-stream))
        ((or? query)
          (disjoin (or-clauses query) frame-stream))
        ((not? query)
          (negate (not-query query) frame-stream))
        ((lisp-value? query)
          (lisp-value (lisp-value-call query) frame-stream))
        ((always-true? query)
          frame-stream)
        (else
          (simple-query query frame-stream))))

(define (conjoin conjuncts frame-stream)
  (if (null? conjuncts)
      frame-stream
      (conjoin (cdr conjuncts)
               (qeval (car conjuncts) frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (null? disjuncts)
      the-empty-stream
      (stream-interleave-delayed
        (qeval (car disjuncts) frame-stream)
        (delay (disjoin (cdr disjuncts) frame-stream)))))

(define (negate not-query frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null?
            (qeval not-query
                   (singleton-stream frame)))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream))

(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum)
      (check-an-assertion datum pattern frame))
    the-assertions))

(define (check-an-assertion datum pattern frame)
  (let ((match-result (pattern-match pattern datum frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pattern datum frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pattern datum) frame)
        ((var? pattern) (extend-if-consistent pattern datum frame))
        ((and (pair? pattern) (pair? datum))
          (pattern-match (cdr pattern)
                         (cdr datum)
                         (pattern-match (car pattern)
                                        (car datum)
                                        frame)))
        (else 'failed)))

(define (extend-if-consistent var datum frame)
  (let ((binding (frame-lookup var frame)))
    (if binding
        (pattern-match (binding-value binding) datum frame)
        (frame-extend var datum frame))))

(define (apply-rules pattern frame)
  (stream-flatmap
    (lambda (rule)
      (apply-a-rule rule pattern frame))
    the-rules))

(define (apply-a-rule rule pattern frame)
  (let ((clean-rule (rename-rule-variables rule)))
    (let ((unify-result
           (unify-match pattern
                        (rule-conclusion clean-rule)
                        frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-rule-variables rule)
  (let ((id (new-rule-application-id!)))
    (define (traverse exp)
      (cond ((var? exp) (make-variable exp id))
            ((pair? exp)
              (cons (traverse (car exp))
                    (traverse (cdr exp))))
            (else exp)))
    (traverse rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
          (unify-match (cdr p1)
                       (cdr p2)
                       (unify-match (car p1)
                                    (car p2)
                                    frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((var-binding (frame-lookup var frame)))
    (cond (var-binding
            (unify-match (binding-value var-binding) val frame))
          ((var? val)
            (let ((val-binding (frame-lookup val frame)))
              (if val-binding
                  (unify-match var (binding-value val-binding) frame)
                  (frame-extend var val frame))))
          ((depends-on? val var frame) 'failed)
          (else (frame-extend var val frame)))))

(define (depends-on? exp var frame)
  (define (traverse exp)
    (cond ((var? exp)
            (or (equal? var exp)
                (let ((binding (frame-lookup exp frame)))
                  (and binding (traverse (binding-value binding))))))
          ((pair? exp)
            (or (traverse (car exp))
                (traverse (cdr exp))))
          (else #f)))
  (traverse exp))

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v) (error "Unbound variable" v))))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

(define (execute exp)
  (apply (eval (car exp)) (cdr exp)))

(define (instantiate exp frame ubound-var-handler)
  (define (traverse exp)
    (cond ((var? exp)
            (let ((binding (frame-lookup exp frame)))
              (if binding
                  (traverse (binding-value binding))
                  (ubound-var-handler exp))))
          ((pair? exp)
            (cons (traverse (car exp)) (traverse (cdr exp))))
          (else exp)))
  (traverse exp))
