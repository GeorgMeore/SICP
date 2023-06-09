(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)

(define (leaf? object)
  (and (pair? object) (eq? (car object) 'leaf))
)

(define (leaf-symbol leaf) (cadr leaf))

(define (leaf-weight leaf) (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
  )
)
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (leaf-symbol tree))
      (caddr tree)
  )
)

(define (weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)
  )
)
