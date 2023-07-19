(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
              (lambda (value) (set! contents value)))
            (else
              (error "Invalid register operation" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))


(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Pop from empty stack")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (reset) (set! s '()))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'reset) (reset))
            (else
              (error "Invalid stack operation" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (reset stack)
  (stack 'reset))


(define (create-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (text '()))
    (let ((ops-table (list (cons 'initizlize-stack
                                 (lambda () (reset stack)))))
          (reg-table (list (cons 'pc pc)
                           (cons 'flag flag))))
      (define (allocate-register name)
        (if (assoc name reg-table)
            (error "Register reallocation" name)
            (set! reg-table
                  (cons (cons name (make-register name))
                        reg-table))))
      (define (get-register name)
        (let ((val (assoc name reg-table)))
          (if val
              (cdr val)
              (error "Unknown register" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts)
                  'done)
                (else
                  ((instruction-executor (car insts)))
                  (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
                (set-contents! pc text)
                (execute))
              ((eq? message 'install-instructions)
                (lambda (insts) (set! text insts)))
              ((eq? message 'allocate-register)
                allocate-register)
              ((eq? message 'get-register)
                get-register)
              ((eq? message 'install-operations)
                (lambda (ops) (set! ops-table (append ops-table ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) ops-table)
              (else
                (error "Invalid machine operation" message))))
      dispatch)))

(define (make-machine regs ops text)
  (let ((machine (create-machine)))
    (for-each
      (lambda (name)
        ((machine 'allocate-register) name))
      regs)
    ((machine 'install-operations) ops)
    ((machine 'install-instructions)
      (assemble text machine))
    machine))

(define (start machine)
  (machine 'start))

(define (get-register machine name)
  ((machine 'get-register) name))

(define (get-register-contents machine name)
  (get-contents (get-register machine name)))

(define (set-register-contents! machine name value)
  (set-contents! (get-register machine name) value))
