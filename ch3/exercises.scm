; ex 1
(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ acc x))
    acc))

; ex 2
(define (make-monitored f)
  (define counter 0)
  (define (monitored arg)
    (cond ((eq? arg 'how-many-calls?) counter)
          ((eq? arg 'reset-counter) (set! counter 0))
          (else (set! counter (+ counter 1))
                (f arg))))
  monitored)

; ex 3, 4, 7
(define (make-account balance passwd)
  (define invalid-attempts 0)
  (define (call-the-cops)
    (display "Contacting the authorities\n"))
  (define (withdraw amount)
    (cond ((>= balance amount)
            (set! balance (- balance amount))
            balance)
          (else "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-joint new-passwd)
    (secure-dispatcher new-passwd))
  (define (dispatch action)
    (cond ((eq? action 'withdraw) withdraw)
          ((eq? action 'deposit) deposit)
          ((eq? action 'make-joint) make-joint)
          (else (error "Unknown action" action))))
  (define (secure-dispatcher valid-password)
    (lambda (passwd action)
      (cond ((not (eq? passwd valid-password))
              (set! invalid-attempts (+ invalid-attempts 1))
              (if (>= invalid-attempts 7) (call-the-cops))
              (lambda (_) "Invalid password"))
            (else
              (set! invalid-attempts 0)
              (dispatch action)))))
  (secure-dispatcher passwd))

(define (make-joint account passwd new-passwd)
  ((account passwd 'make-joint) new-passwd))

; ex 5
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (define (monte-carlo)
    (define (iter remaining passed)
      (cond ((= remaining 0) (/ passed trials))
            ((experiment) (iter (- remaining 1) (+ passed 1)))
            (else (iter (- remaining 1) passed))))
    (iter trials 0))
  (* (monte-carlo) (* (- x2 x1) (- y2 y1))))

; ex 17
(define (count-pairs x)
  (define seen '())
  (define (traverse x)
    (cond ((or (memq x seen) (not (pair? x)))
            0)
          (else
            (set! seen (cons x seen))
            (+ 1 (traverse (car x)) (traverse (cdr x))))))
  (traverse x))

; ex 19 (Floyd's algorithm)
; Let x_1, x_2, ... be a possibly infinite sequence, where x_k+1 = f(x_k).
; We need to check if it containts a cycle (i.e. there exist i /= j: x_i = x_j).
; Suppose we have a loop:
;   Let l be a length of the loop (i.e. smallet number such that x_i = x_i+l).
;   Let m be a start of the loop (i.e. smallest number such that x_m = x_m+l).
;   It's obvious that for any i > m, k > 0: x_i = x_i+kl (rotational symmetry of the loop).
;   Let j be smallest multiple of l bigger than m (i.e. min {j > m: j = k'l}).
;   x_j = x_j+k'l and therefore x_j = x_2j.
;   That means that if a sequence contains a loop there must exist such i that x_i = x_2i.
;   On the other hand if for a given sequence there exists i: x_i = x_2i
;     sequence must contain a loop (by definition).
(define (has-cycle? x)
  (define (floyd-check i-th 2i-th)
    (cond ((eq? i-th 2i-th)
            #t) ; found x_i == x_2i
          ((or (atom? 2i-th) (atom? (cdr 2i-th)))
            #f) ; found the end of the sequence
          (else
            (floyd-check (cdr i-th) (cddr 2i-th)))))
  (and (pair? x) (floyd-check x (cdr x))))
