(define (println . values)
  (for-each display values)
  (newline))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set-trace)
              (lambda (value) (set! trace value)))
            ((eq? message 'set)
              (lambda (value)
                (when trace
                  (println "register: " name " " contents " " value))
                (set! contents value)))
            (else
              (error "Invalid register operation" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (set-trace! register value)
  ((register 'set-trace) value))


(define (make-stack)
  (let ((stack '())
        (pushes 0)
        (depth 0)
        (max-depth 0))
    (define (push x)
      (set! depth (+ depth 1))
      (set! pushes (+ pushes 1))
      (set! max-depth (max depth max-depth))
      (set! stack (cons x stack)))
    (define (pop)
      (if (null? stack)
          (error "Pop from empty stack")
          (let ((top (car stack)))
            (set! stack (cdr stack))
            (set! depth (- depth 1))
            top)))
    (define (reset) (set! stack '()))
    (define (stat)
      (println "stack: " pushes " " depth " " max-depth))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'reset) (reset))
            ((eq? message 'stat) (stat))
            (else
              (error "Invalid stack operation" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


(define (make-machine machine-operations program-text)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (text '*notext*)
        (labels '*nolabels*)
        (trace #f))
    (let ((ops-table (list (cons 'stack-reset (lambda () (stack 'reset)))
                           (cons 'stack-stat (lambda () (stack 'stat)))))
          (reg-table (list (cons 'pc pc)
                           (cons 'flag flag))))
      (define (install-operations ops)
        (set! ops-table (append ops-table ops)))
      (define (install-instructions ists labs)
        (set! text ists)
        (set! labels labs))
      (define (get-register name)
        (let ((val (assoc name reg-table)))
          (cond (val (cdr val))
                ((eq? text '*notext*) ; assembling is in process
                  (let ((reg (make-register name)))
                    (set! reg-table (cons (cons name reg) reg-table))
                    reg))
                (else (error "Unknown register" name)))))
      (define (execute-one inst)
        (when trace
          (println (instruction-text inst)))
        ((instruction-executor inst)))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((instruction-break (car insts)) 'break)
                (else
                  (execute-one (car insts))
                  (execute)))))
      (define (continue)
        (execute-one (car (get-contents pc)))
                (execute))
      (define (get-instruction label-name n)
        (let ((label (assoc label-name labels)))
          (if label
              (or (assoc (+ (label-number label) n) text)
                  (error "Invalid instruction location"))
              (error "Undefined label" label-name))))
      (define (dispatch message)
        (cond ((eq? message 'start)
                (set-contents! pc text)
                (execute))
              ((eq? message 'get-register)
                get-register)
              ((eq? message 'stack) stack)
              ((eq? message 'operations) ops-table)
              ((eq? message 'set-trace)
                (lambda (value) (set! trace value)))
              ((eq? message 'add-break)
                (lambda (label-name n)
                  (set-instruction-break! (get-instruction label-name n) #t)))
              ((eq? message 'remove-break)
                (lambda (label-name n)
                  (set-instruction-break! (get-instruction label-name n) #f)))
              ((eq? message 'remove-all-breaks)
                (for-each
                  (lambda (inst) (set-instruction-break! inst #f))
                  text))
              ((eq? message 'proceed)
                (execute-one (car (get-contents pc)))
                (execute))
              (else
                (error "Invalid machine operation" message))))
      (install-operations machine-operations)
      (assemble program-text dispatch install-instructions)
      dispatch)))

(define (start machine)
  (machine 'start))

(define (proceed machine)
  (machine 'proceed))

(define (get-register machine name)
  ((machine 'get-register) name))

(define (get-register-contents machine name)
  (get-contents (get-register machine name)))

(define (set-register-contents! machine name value)
  (set-contents! (get-register machine name) value))

(define (enable-instruction-tracing machine)
  ((machine 'set-trace) #t))

(define (disable-instruction-tracing machine)
  ((machine 'set-trace) #f))

(define (enable-register-tracing machine name)
  (set-trace! (get-register machine name) #t))

(define (disable-register-tracing machine name)
  (set-trace! (get-register machine name) #f))

(define (set-breakpoint machine label n)
  ((machine 'add-break) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'remove-break) label n))

(define (cancel-all-breakpoints machine)
  (machine 'remove-all-breaks))
