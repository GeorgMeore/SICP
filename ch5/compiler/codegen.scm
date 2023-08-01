(define (list-find elem lst)
  (define (iter lst index)
    (cond ((null? lst) #f)
          ((eq? (car lst) elem) index)
          (else (iter (cdr lst) (+ 1 index)))))
  (iter lst 0))

(define (find-variable var cenv)
  (define (iter cenv env-index)
    (if (null? cenv)
        'global
        (let ((frame-index (list-find var (car cenv))))
          (if frame-index
              (list env-index frame-index)
              (iter (cdr cenv) (+ 1 env-index))))))
  (iter cenv 0))


(define (compile-toplevel seq)
  (append-instruction-sequences
    (make-instruction-sequence '() '(env)
      `((assign env (op get-global-environment))))
    (compile-toplevel-sequence seq)))

(define (compile-toplevel-sequence seq)
  (let ((first (first-exp seq)))
    (let ((first-exp-code
           (if (definition? first)
               (compile-definition first 'val 'next)
               (compile first 'val 'next '()))))
      (if (last-exp? seq)
          first-exp-code
          (preserving '(env)
            first-exp-code
            (compile-toplevel-sequence (rest-exps seq)))))))

(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
          (compile-self-evaluating exp target linkage))
        ((quoted? exp)
          (compile-quoted exp target linkage))
        ((variable? exp)
          (compile-variable exp target linkage cenv))
        ((assignment? exp)
          (compile-assignment exp target linkage cenv))
        ((definition? exp)
          (error "Non-toplevel definition" exp))
        ((if? exp)
          (compile-if exp target linkage cenv))
        ((lambda? exp)
          (compile-lambda exp target linkage cenv))
        ((begin? exp)
          (compile-sequence (begin-actions exp) target linkage cenv))
        ((application? exp)
          (compile-application exp target linkage cenv))
        (else
          (error "Unknown expression type" exp))))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage cenv)
  (let ((addr (find-variable exp cenv)))
    (end-with-linkage linkage
      (if (eq? addr 'global)
          (make-instruction-sequence '() (list target 'env)
            `((assign env (op get-global-environment))
              (assign ,target
                      (op lookup-variable-value)
                      (const ,exp)
                      (reg env))))
          (make-instruction-sequence '(env) (list target)
            `((assign ,target
                      (op lexical-address-lookup)
                      (const ,addr)
                      (reg env))))))))

(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (value-code (compile (assignment-value exp) 'val 'next cenv)))
    (end-with-linkage linkage
      (preserving '(env)
        value-code
        (let ((addr (find-variable var cenv)))
          (if (eq? addr 'global)
              (make-instruction-sequence '(val) (list target 'env)
                `((assign env (op get-global-environment))
                  (perform (op set-variable-value!)
                           (const ,var)
                           (reg val)
                           (reg env))
                  (assign ,target (const ok))))
              (make-instruction-sequence '(env val) (list target)
                `((perform (op lexical-address-set!)
                           (const ,addr)
                           (reg val)
                           (reg env))
                  (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (value-code (compile (definition-value exp) 'val 'next '())))
    (end-with-linkage linkage
      (preserving '(env)
        value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (compile-if exp target linkage cenv)
  (let ((true-branch (make-label 'true-branch))
        (false-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((cons-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((pred-code (compile (if-predicate exp) 'val 'next cenv))
            (cons-code
              (compile (if-consequent exp) target cons-linkage cenv))
            (alt-code
              (compile (if-alternative exp) target linkage cenv)))
        (preserving '(env continue)
          pred-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '()
              `((test (op false?) (reg val))
                (branch (label ,false-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences true-branch cons-code)
              (append-instruction-sequences false-branch alt-code))
            after-if))))))

(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage cenv)
      (preserving '(env continue)
        (compile (first-exp seq) target 'next cenv)
        (compile-sequence (rest-exps seq) target linkage cenv))))

(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry cenv))
        after-lambda))))

(define (compile-lambda-body exp proc-entry cenv)
  (let ((params (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
          (assign env
                  (op compiled-procedure-environment)
                  (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,params)
                  (reg argl)
                  (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return (cons params cenv)))))

(define (compile-application exp target linkage cenv)
  (let ((proc-code (compile (operator exp) 'proc 'next cenv))
        (operand-codes
          (map (lambda (op)
                 (compile op 'val 'next cenv))
               (operands exp))))
    (preserving '(env continue)
      proc-code
      (preserving '(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
          '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                 (car operand-codes)
                 (make-instruction-sequence '(val) '(argl)
                   '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                code-to-get-last-arg
                (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
           (car operand-codes)
           (make-instruction-sequence '(val argl) '(argl)
             '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-procedure-application target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
              (make-instruction-sequence '(proc argl) (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg argl)))))))
        after-call))))

(define (compile-procedure-application target linkage)
  (let ((all-regs '(env proc val argl continue)))
    (cond ((and (eq? target 'val)
                (not (eq? linkage 'return)))
            (make-instruction-sequence '(proc) all-regs
              `((assign continue (label ,linkage))
                (assign val (op compiled-procedure-entry) (reg proc))
                (goto (reg val)))))
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
            (let ((proc-return (make-label 'proc-return)))
              (make-instruction-sequence '(proc) all-regs
                `((assign continue (label ,proc-return))
                  (assign val (op compiled-procedure-entry) (reg proc))
                  (goto (reg val))
                  ,proc-return
                  (assign ,target (reg val))
                  (goto (label ,linkage))))))
          ((and (eq? target 'val)
                (eq? linkage 'return))
            (make-instruction-sequence '(proc continue) all-regs
              '((assign val (op compiled-procedure-entry) (reg proc))
                (goto (reg val)))))
          ((and (not (eq? target 'val))
                (eq? linkage 'return))
            (error "Cannot use return with a target that is not 'val'")))))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
          (make-instruction-sequence '(continue) '()
            '((goto (reg continue)))))
        ((eq? linkage 'next)
          (empty-instruction-sequence))
        (else
          (make-instruction-sequence '() '()
            `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
    instruction-sequence
    (compile-linkage linkage)))
