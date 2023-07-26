(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))

(define (registers-needed seq)
  (if (symbol? seq) '() (car seq)))
(define (registers-modified seq)
  (if (symbol? seq) '() (cadr seq)))
(define (statements seq)
  (if (symbol? seq) (list seq) (caddr seq)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union
        (registers-needed seq1)
        (list-difference (registers-needed seq2)
                         (registers-modified seq1)))
      (list-union
        (registers-modified seq1)
        (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
          (car seqs)
          (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((reg (car regs)))
        (if (and (needs-register? seq2 reg)
                 (modifies-register? seq1 reg))
            (preserving (cdr regs)
              (make-instruction-sequence
                (list-union (list reg) (registers-needed seq1))
                (list-difference (registers-modified seq1) (list reg))
                (append `((save ,reg))
                        (statements seq1)
                        `((restore ,reg))))
              seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1) (registers-needed seq2))
    (list-union (registers-modified seq1) (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))


(define label-counter 0)

(define (make-label name)
  (set! label-counter (+ 1 label-counter))
  (string->symbol
    (string-append (symbol->string name)
                   (number->string label-counter))))
