(define (make-dict) '())

(define (dict-add key value dict)
  (cons (list key value) dict))

(define (dict-has? key dict)
  (and
    (pair? dict)
    (or (equal? (caar dict) key)
        (dict-has? key (cdr dict)))))

(define (dict-lookup key dict)
  (if (null? dict)
      #f
      (if (equal? (caar dict) key)
          (cadar dict)
          (dict-lookup key (cdr dict)))))
