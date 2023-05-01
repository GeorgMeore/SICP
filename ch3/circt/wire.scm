(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (call-each procedures)
      (cond ((pair? procedures)
              ((car procedures))
              (call-each (cdr procedures)))))
    (define (set-my-signal! new-value)
      (cond ((not (or (= new-value 0) (= new-value 1)))
              (error "invalid signal"))
            ((not (= signal-value new-value))
              (set! signal-value new-value)
              (call-each action-procedures))))
    (define (accept-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)) ; startup
    (define (self m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-procedure!)
            (else (error "bad action"))))
    self))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire proc)
  ((wire 'add-action!) proc))
