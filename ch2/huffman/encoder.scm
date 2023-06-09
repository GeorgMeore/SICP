(include "tree.scm")


(define (generate-huffman-tree pairs)
  (define (insort x l)
    (cond ((null? l)
            (list x))
          ((< (weight x) (weight (car l)))
            (cons x l))
          (else
            (cons (car l) (insort x (cdr l))))
    )
  )
  (define (sorted-leafs pairs)
    (if (null? pairs)
        '()
        (insort (make-leaf (caar pairs) (cadar pairs))
                (sorted-leafs (cdr pairs)))
    )
  )
  (define (successive-merge sorted-nodes)
    (cond ((null? sorted-nodes)
            (error "empty node list"))
          ((null? (cdr sorted-nodes))
            (car sorted-nodes))
          (else
            (let (
              (first (car sorted-nodes))
              (second (cadr sorted-nodes))
              (rest (cddr sorted-nodes))
            )
              (successive-merge (insort (make-code-tree first second) rest))
            )
          )
    )
  )
  (successive-merge (sorted-leafs pairs))
)

(define (encode message tree)
  (define (encode-symbol symbol tree)
      (if (leaf? tree)
        (if (eq? symbol (leaf-symbol tree))
            '()
            (error "symbol is not in the tree" symbol)
        )
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (encode-symbol symbol (left-branch tree)))
            (cons 1 (encode-symbol symbol (right-branch tree)))
        )
      )
  )
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))
  )
)

(define (decode bits tree)
  (define (choose-branch bit tree)
    (cond ((= bit 0) (left-branch tree))
          ((= bit 1) (right-branch tree))
          (else (error "bad bit" bit))
    )
  )
  (define (decode-bits bits start)
    (if (null? bits)
        '()
        (let ((next (choose-branch (car bits) start)))
          (if (leaf? next)
              (cons (leaf-symbol next)
                    (decode-bits (cdr bits) tree))
              (decode-bits (cdr bits) next)
          )
        )
    )
  )
  (decode-bits bits tree)
)
