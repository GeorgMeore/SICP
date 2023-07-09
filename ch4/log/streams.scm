(define-syntax stream-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())

(define (singleton-stream x)
  (stream-cons x the-empty-stream))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-null? stream)
  (null? stream))

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (stream-cons (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (stream-for-each proc stream)
  (cond ((stream-null? stream)
          'done)
        (else
          (proc (stream-car stream))
          (stream-for-each proc (stream-cdr stream)))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (stream-cons
        (stream-car s1)
        (stream-append-delayed
          (stream-cdr s1)
          delayed-s2))))

(define (stream-interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (stream-cons
        (stream-car s1)
        (stream-interleave-delayed
          (force delayed-s2)
          (delay (stream-cdr s1))))))

(define (stream-flatten stream)
  (if (stream-null? stream)
      the-empty-stream
      (stream-interleave-delayed
        (stream-car stream)
        (delay (stream-flatten (stream-cdr stream))))))

(define (stream-flatmap proc stream)
  (stream-flatten (stream-map proc stream)))

(define (display-stream stream)
  (stream-for-each
    (lambda (x) (display x) (newline))
    stream))
