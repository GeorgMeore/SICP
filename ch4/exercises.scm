; ex 27
;(define count 0)
;(define (id x) (set! count (+ count 1)) x)
;(define w (id (id 10)))
;count ; prints 1
;w     ; prints 10
;count ; prints 2
; When evaluating this definition, only the outer application is evaluated,
; leading to the increment of the count.
; The inner application `(id 10)` is stored as a thunk.
; When we evaluate `w`, we force the thunk and `count` is once more incremented.

; ex28
((lambda (f) (f 5)) (lambda (x) (+ x 1)))
