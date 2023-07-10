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
