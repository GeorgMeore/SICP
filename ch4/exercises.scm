; ex 27
; > (define count 0)
; > (define (id x) (set! count (+ count 1)) x)
; > (define w (id (id 10)))
; > count ; prints 1
; > w     ; prints 10
; > count ; prints 2
; When evaluating this definition, only the outer application is evaluated,
; leading to the increment of `count`.
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

; ex 55
(supervisor ?x (Ben Bitdiddle))
(job ?x (accounting . ?y))
(address ?x (Slumerville . ?y))

; ex 56
(and (supervisor ?x (Ben Bitdiddle))
     (address ?x ?a))
(and (salary (Ben Bitdiddle) ?x)
     (salary ?p ?y)
     (lisp-value > ?x ?y))
(and (supervisor ?x ?y)
     (not (job ?y (computer . ?j)))
     (job ?y ?j))

; ex 57
(rule (can-replace ?p1 ?p2)
  (and (not (same ?p1 ?p2))
       (job ?p1 ?j1)
       (job ?p2 ?j2)
       (or (same ?j1 ?j2)
           (can-do-job ?j1 ?j2))))

(can-replace ?x (Cy D. Fect))
(and (salary ?p1 ?x)
     (salary ?p2 ?y)
     (lisp-value > ?y ?x)
     (can-replace ?p1 ?p2))

; ex 58
(rule (bigshot ?p ?d)
  (and (job ?p (?d . ?x))
       (or (not (supervisor ?p ?s))
           (and (supervisor ?p ?s)
                (not (job ?s (?d . ?y)))
                (not (bigshot ?s ?d))))))

; ex 59
(meeting ?division (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
  (or (meeting whole-company ?day-and-time)
      (and (job ?person (?division . ?title))
           (meeting ?division ?day-and-time))))

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

; ex 60
; `lives-near` is commutative: if `(lives-near x y)` is true then
; `(lives-near y x)` must also be true.
; That's why when both arguments are unbound we will get both solutions.

; ex 61
; (?x next-to ?y in (1 (2 3) 4))
; -> ((2 3) next-to 4 in (1 (2 3) 4))
; -> (1 next-to (2 3) in (1 (2 3) 4))
; (?x next-to 1 in (2 1 3 1))
; -> (3 next to 1 in (2 1 3 1))
; -> (2 next to 1 in (2 1 3 1))
