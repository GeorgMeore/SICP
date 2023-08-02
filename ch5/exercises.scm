; ex 1
; Data paths:
;    cond(=) <--------- reg(n)----.         .-- reg(p) <-- btn(p<-1) -- const(1)
;      ^               ^     |     \       /     ^
;      |               |     |      \     /      |
;      |         btn(n<-d)   |      |     |     btn(p<-m)
;      |               |     v      v     v     /
;    const(0)          `-op(dec)    op(mul) ---'
;
; Controller:
;          start
;            |
;            v
;         do(p<-1)
;            |
;            v    (no)
;    .---> if(=) -----> do(p<-m)
;    |       |(yes)        |
;    |       v             v
;    |      done        do(n<-d)
;    `--------------------'

; ex 2
(controller
   (assign p (const 1))
 test-n
   (test (op =) (reg n) (const 0))
   (branch (label done))
   (assign p (op *) (reg n) (reg p))
   (assign n (op -) (reg n) (const 1))
   (goto (label test-n))
 done)

; ex 3
(controller
 loop
   (test (op good-enough?) (reg g) (reg x))
   (branch (label done))
   (assign g (op improve) (reg g) (reg x))
   (goto (label loop))
 done)

(controller
   (assign g 1.0)
 loop
   (assign t (op *) (reg x) (reg x))
   (assign t (op -) (reg t) (reg x))
   (test (op >) (reg t) (const 0))
   (branch (label check))
   (assign t (op -) (const 0) (reg t))
 check
   (test (op <) (reg t) (const 0.001))
   (branch (label done))
   (assign t (op /) (reg x) (reg g))
   (assign t (op +) (reg t) (reg g))
   (assign g (op /) (reg t) (const 2))
   (goto (label loop))
 done)

; ex 4
(controller
   (assign continue (label exit))
 expt
   (test (op =) (reg n) (const 0))
   (branch (label expt-base-case))
   (save continue)
   (assign continue (label expt-after-rec-call))
   (assign n (op -) (reg n) (const 1))
   (goto (label expt))
 expt-after-rec-call
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
 expt-base-case
   (assign val (const 1))
   (goto (reg continue))
 exit)

(controller
   (assign continue (label exit))
 expt-iter
   (assign val (const 1))
 expt-iter-loop
   (test (op =) (reg n) (const 0))
   (branch (label expt-iter-done))
   (assign val (op *) (reg val) (reg b))
   (assign n (op -) (reg n) (const 1))
   (goto (label expt-iter-loop))
 expt-iter-done
   (goto (reg continue))
 exit)

; ex 6
; There is a redundant restore/save on the `continue` register
;   ...
;   (restore continue)
;   ;; set up to compute Fib(n − 2)
;   (assign n (op -) (reg n) (const 2))
;   (save continue)
;   ...

; ex 20
; (define x (cons 1 2))
; (define y (list x x))
;
;   (3)[.|.] -> (2)[.|/]
;       |.----------'
;       V
;   (1)[.|.] -> [2]
;       |
;       V
;      [1]
;
;         0    1    2    3
; cars | .. | n1 | p1 | p1
;--------------------------
; cdrs | .. | n2 | e0 | p2
;
; free = 4
; x = 1
; y = 3

; ex 21
(controller-a
   (assign continue (label exit))
 check-tree
   (test (op null?) (reg tree))
   (branch (label is-null))
   (test (op pair?) (reg tree))
   (branch (label is-cons))
   (goto (label is-atom))
 is-cons
   (save continue)
   (assign continue (label left-done))
   (save tree)
   (assign tree (op car) (reg tree))
   (goto (label check-tree))
 left-done
   (restore tree)
   (assign tree (op cdr) (reg tree))
   (save val)
   (assign continue (label right-done))
   (goto (label check-tree))
 right-done
   (restore temp)
   (assign val (op +) (reg val) (reg temp))
   (restore continue)
   (goto (reg continue))
 is-atom
   (assign val (const 1))
   (goto (reg continue))
 is-null
   (assign val (const 0))
   (goto (reg continue))
 exit)

(controller-b
   (assign val (const 0))
   (assign continue (label exit))
 check-tree
   (test (op null?) (reg tree))
   (branch (label is-null-or-atom))
   (test (op pair?) (reg tree))
   (branch (label is-cons))
   (assign val (op +) (reg val) (const 1))
 is-null-or-atom
   (goto (reg continue))
 is-cons
   (save continue)
   (assign continue (label right-done))
   (save tree)
   (assign tree (op car) (reg tree))
   (goto (label check-tree))
 right-done
   (restore tree)
   (assign tree (op cdr) (reg tree))
   (restore continue)
   (goto (label check-tree))
 exit)

; ex 31
; Left-to-right argument evaluation is assumed here
; (f 'x 'y)
;   In this case we can skip all of the saves.
; ((f) 'x 'y)
;   In this case we can skip all of the saves.
; (f (g 'x) y)
;   We need to save `proc`, `env` and `argl` before evaluating (g 'x).
; (f (g 'x) 'y)
;   We need to save `proc` and `argl` before evaluating (g 'x).

; ex 33
;   The first program first evaluates `n` and then evaluates `(factorial (- n 1))`.
; It needs to save `argl` before evaluating `(factorial (- n 1))`.
; But on the other hand it can skip saving `env` before evaluating `n`.
;   The second program first evaluates `(factorial-alt (- n 1))` and only then evaluates `n`.
; It needs to save `env` before evaluating `(factorial-alt (- n 1))`, but it need not save `argl`.
;   Thus the difference is that the first variant does a save/restore on `env` and
; the second does on `argl`. The total amount of operations is the same.

; ex 34
  ;; construct the `factorial` procedure and skip over the body code
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
entry1  ;; entry point for `factorial`
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; construct the internal `iter` procedure and skip over the body code
  (assign val (op make-compiled-procedure) (label entry3) (reg env))
  (goto (label after-lambda4))
entry3  ;; entry point for `iter`
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  ;; computing `(> n counter)` could affect `continue` and `env`
  (save continue)
  (save env)
  ;; compute `(> n counter)`
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
compiled-branch9
  (assign continue (label after-call10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call10  ;; `val` now contains the result of `(> n counter)`
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch6))
true-branch5
  ;; return `product`
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch6
  ;; compute and return `(iter (* counter product) (+ counter 1))`
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  ;; computing the argument list changes the values of `continue` and `proc`
  (save continue)
  (save proc) ;; save `iter`
  (save env)
  ;; compute `(+ counter 1)`
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
compiled-branch15
  (assign continue (label after-call16))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call16  ;; `val` now contains the result of `(+ counter 1)`
  (assign argl (op list) (reg val))
  (restore env)
  (save argl) ;; save partial argument list for `iter`
  ;; compute `(* counter product)`
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
compiled-branch12
  (assign continue (label after-call13))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call13  ;; `val` now contains the result of `(* counter product)`
  (restore argl) ;; restore partial argument list for `iter`
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ;; restore `iter`
  ;; do the recursive `iter` call
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
compiled-branch18
  ;; since this is the last expression to be evaluated
  ;; we don't need to set up any after-call actions (and save any registers)
  ;; and can just jump straight to the procedure entry.
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call19
after-if7
after-lambda4
  ;; assign the procedure to the variable `iter`
  (perform (op define-value!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  ;; compute `(iter 1 1)`
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch20))
compiled-branch21
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch20
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call22
after-lambda2
  ;; assign the procedure to the variable `factorial`
  (perform (op define-value!) (const factorial) (reg val) (reg env))
  (assign val (const ok))


; ex 35
(define (f x)
  (+ x (g (+ x 2))))
