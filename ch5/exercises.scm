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
