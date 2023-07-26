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
;   We need to `env` before evaluating (f).
; (f (g 'x) y)
;   We need to save `proc`, `env` and `argl` before evaluating (g 'x).
; (f (g 'x) 'y)
;   We need to save `proc` and `argl` before evaluating (g 'x).
