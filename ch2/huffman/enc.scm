#!/usr/bin/csi -s

(include "encoder.scm")


(define (counters words)
  (define (inc word-counters word)
    (cond ((null? word-counters)
            (list (list word 1)))
          ((eq? (caar word-counters) word)
            (cons (list word (+ (cadar word-counters) 1))
                  (cdr word-counters)))
          (else
            (cons (car word-counters)
                  (inc (cdr word-counters) word)))
    )
  )
  (define (count words word-counters)
    (if (null? words)
        word-counters
        (count (cdr words)
               (inc word-counters (car words)))
    )
  )
  (count words '())
)

(let ((input (read)))
  (let ((tree (generate-huffman-tree (counters input))))
    (display (encode input tree))
    (newline)
  )
)
