(define (assemble text machine receive)
  (extract-labels
    text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      (receive insts labels))))

(define (extract-labels text receive)
  (define (go-over-text text inst-number receive)
    (if (null? text)
        (receive '() '())
        (if (symbol? (car text))
            (go-over-text (cdr text) inst-number
              (lambda (insts labels)
                (receive
                  insts
                  (cons (make-label (car text) insts inst-number) labels))))
            (go-over-text (cdr text) (+ inst-number 1)
              (lambda (insts labels)
                (receive
                  (cons (make-instruction (car text) inst-number) insts)
                  labels))))))
  (go-over-text text 0 receive))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-executor!
          inst
          (make-executor
            (instruction-text inst)
            labels machine pc flag stack ops)))
      insts)))


(define (make-instruction text number)
  (list number text '*noexec* #f))

(define (instruction-number inst)
  (car inst))

(define (instruction-text inst)
  (cadr inst))

(define (instruction-executor inst)
  (caddr inst))

(define (instruction-break inst)
  (cadddr inst))

(define (set-instruction-executor! inst proc)
  (set-car! (cddr inst) proc))

(define (set-instruction-break! inst val)
  (set-car! (cdddr inst) val))


(define (make-label name insts number)
  (list name number insts))

(define (label-number label)
  (cadr label))

(define (label-insts label)
  (caddr label))


(define (lookup-label labels name)
  (let ((val (assoc name labels)))
    (if val
        (label-insts val)
        (error "Undefined label" name))))

(define (lookup-op ops name)
  (let ((val (assoc name ops)))
    (if val
        (cdr val)
        (error "Undefined operation" name))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-executor inst labels machine pc flag stack ops)
  (cond ((assign? inst)
          (make-assign inst machine labels ops pc))
        ((test? inst)
          (make-test inst machine labels ops flag pc))
        ((branch? inst)
          (make-branch inst machine labels flag pc))
        ((goto? inst)
          (make-goto inst machine labels pc))
        ((save? inst)
          (make-save inst machine stack pc))
        ((restore? inst)
          (make-restore inst machine stack pc))
        ((perform? inst)
          (make-perform inst machine labels ops pc))
        (else
          (error "Invalid instruction" inst))))

(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-name inst)))
        (value (assign-value inst)))
    (let ((value-proc
           (if (operation? value)
               (make-operation value machine labels ops)
               (make-primitive (car value) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-test inst machine labels ops flag pc)
  (let ((condition (test-condition inst)))
    (let ((condition-proc
           (if (operation? condition)
               (make-operation condition machine labels ops)
               (error "Operation expected" inst))))
      (lambda ()
        (set-contents! flag (condition-proc))
        (advance-pc pc)))))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (let ((insts
           (if (label? dest)
               (lookup-label labels (label-name dest))
               (error "Label expected" inst))))
      (lambda ()
        (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label? dest)
            (let ((insts (lookup-label labels (label-name dest))))
              (lambda () (set-contents! pc insts))))
          ((register? dest)
            (let ((reg (get-register machine (register-name dest))))
              (lambda () (set-contents! pc (get-contents reg)))))
          (else
            (error "Label or register expected" inst)))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (save-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((dst (get-register machine (restore-name inst))))
    (lambda ()
      (set-contents! dst (pop stack))
      (advance-pc pc))))

(define (make-perform inst machine labels ops pc)
  (let ((action (perform-action inst)))
    (let ((action-proc
           (if (operation? action)
               (make-operation action machine labels ops)
               (error "Operation expected" inst))))
      (lambda ()
        (action-proc)
        (advance-pc pc)))))

(define (make-primitive exp machine labels)
  (cond ((constant? exp)
          (let ((c (constant-value exp)))
            (lambda () c)))
        ((label? exp)
          (let ((insts (lookup-label labels (label-name exp))))
            (lambda () insts)))
        ((register? exp)
          (let ((reg (get-register machine (register-name exp))))
            (lambda () (get-contents reg))))
        (else
          (error "Invalid primitive expression" exp))))

(define (make-operation exp machine labels ops)
  (let ((op (lookup-op ops (operation-name exp)))
        (aprocs
          (map (lambda (op)
                 (make-primitive op machine labels))
               (operation-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
