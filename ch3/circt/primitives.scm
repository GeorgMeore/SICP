(load "wire")

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output)
  (define (logical-not s)
    (if (= s 0) 1 0))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay
        inverter-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (and-gate a b output)
  (define (logical-and sa sb)
    (if (and (= sa 1) (= sb 1)) 1 0))
  (define (and-inputs)
    (let ((new-value (logical-and (get-signal a) (get-signal b))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a and-inputs)
  (add-action! b and-inputs))

(define (or-gate a b output)
  (define (logical-or sa sb)
    (if (or (= sa 1) (= sb 1)) 1 0))
  (define (or-inputs)
    (let ((new-value (logical-or (get-signal a) (get-signal b))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a or-inputs)
  (add-action! b or-inputs))

(define (probe name wire)
  (define (print-wire-signal)
    (print (current-time the-agenda) " " name " " (get-signal wire)))
  (add-action! wire (lambda () (print-wire-signal))))
