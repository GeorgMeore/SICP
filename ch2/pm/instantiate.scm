(load "dict")
(load "skeleton")

(define (instantiate skel dict)
  (cond ((atom? skel) skel)
        ((skeleton-evaluation? skel)
          (dict-lookup (skeleton-expression skel) dict))
        (else
          (cons (instantiate (car skel) dict)
                (instantiate (cdr skel) dict)))))
