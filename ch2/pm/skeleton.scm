(define (skeleton-evaluation? skel)
  (and (pair? skel) (eq? (car skel) ':))
)

(define (skeleton-expression skel)
  (cadr skel)
)
