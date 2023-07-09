(define the-assertions the-empty-stream)

(define the-rules the-empty-stream)

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (let ((old-assertions the-assertions))
    (set! the-assertions (stream-cons assertion old-assertions)))
  'ok)

(define (add-rule! rule)
  (let ((old-rules the-rules))
    (set! the-rules (stream-cons rule old-rules)))
  'ok)
