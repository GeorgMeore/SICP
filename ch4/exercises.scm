; ex 27
; > (define count 0)
; > (define (id x) (set! count (+ count 1)) x)
; > (define w (id (id 10)))
; > count ; prints 1
; > w     ; prints 10
; > count ; prints 2
; When evaluating this definition, only the outer application is evaluated,
; leading to the increment of the count.
; The inner application `(id 10)` is stored as a thunk.
; When we evaluate `w`, we force the thunk and `count` is once more incremented.

; ex28
((lambda (f) (f 5)) (lambda (x) (+ x 1)))

; ex 35
(define (an-integer-between low high)
  (if (= low high)
      low
      (amb low (an-integer-between (+ low 1) high))))

; ex 36
; We cannot simply replace an-integer-between by an-integer-starting-from
; because the evaluator first tries all of the alternatives at the most recent
; choise point before backing up to the previous choise point.
; That means that the evaluator will only try the triplets of the form
; (low low low), (low low (+ low 1)), (low low (+ low 2)) ...
(define (pythagorean-triplets)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
