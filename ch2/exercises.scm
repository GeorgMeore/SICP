; ex1
(define (make-rat n d)
  (define (sign x)
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (else 0)))
  (define (make s n d)
    (let ((g (gcd n d)))
      (cons (/ n g s) (/ d g))))
  (make (* (sign d) (sign n))
        (abs n)
        (abs d)))

; ex2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p) (print "(" (x-point p) ", " (y-point p) ")"))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

; ex3
(define (distance-point p1 p2)
    (let ((dx (- (x-point p1) (x-point p2)))
          (dy ( -(y-point p1) (y-point p2))))
      (sqrt (+ (* dx dx) (* dy dy)))))

(define (perimeter-rectange r)
  (+ (* 2 (width-rectangle r)) (* 2 (height-rectangle r))))
(define (area-rectange r)
  (* (width-rectangle r) (height-rectangle r)))

; 3-point implementation (without orthogonality checks)
; (B).-----.
;    |     |
; (A).-----.(D)

(define (make-rectangle a b d)
    (cons a (cons b d)))
(define (width-rectangle r)
  (distance-point (car r) (car (cdr r))))
(define (height-rectangle r)
  (distance-point (car r) (cdr (cdr r))))

; ex4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; (car z) -> (z (lambda (p q) p)) -> ((lambda (p q) p) x y) -> x

(define (cdr z)
  (z (lambda (p q) q)))

; ex5
(define (factor-power x a)
  (define (iter n acc)
    (if (= 0 (remainder n a)) (iter (/ n a) (+ 1 acc)) acc))
  (iter x 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (car z)
  (factor-power z 2))
(define (cdr z)
  (factor-power z 3))

; ex6
(define zero
  (lambda (f) (lambda (x) x)))
(define (succ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; ex17
(define (last-pair lst)
  (if (null? (cdr lst)) lst (last-pair (cdr lst))))

; ex18
(define (reverse lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (cons (car lst) acc))))
  (iter lst '()))

; ex20
;(define (filter pred? lst)
;  (cond ((null? lst)
;          lst)
;        ((pred? (car lst))
;          (cons (car lst) (filter pred? (cdr lst))))
;        (else
;          (filter pred? (cdr lst)))))

(define (same-parity integers)
  (if (even? (car integers))
      (filter even? integers)
      (filter odd? integers)))

; ex23
(define (for-each proc lst . dummy)
  (if (not (null? lst))
      (for-each proc (cdr lst) (proc (car lst)))))

; ex27
(define (deep-reverse lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst)
              (cons (if (pair? (car lst))
                        (deep-reverse (car lst))
                        (car lst))
                    acc))))
  (iter lst '()))

; ex28
(define (fringe lst)
  (cond ((null? lst)
          lst)
        ((not (pair? lst))
          (list lst))
        (else
          (append (fringe (car lst)) (fringe (cdr lst))))))

; ex29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (cdr m))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cdr b))

(define (simple-weight? m)
  (not (pair? m)))

(define (total-weight m)
  (if (simple-weight? m)
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

(define (torque b)
  (* (branch-length b) (total-weight (branch-structure b))))

(define (balanced? m)
  (if (simple-weight? m)
      #t
      (and (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m)))
           (= (torque (left-branch m)) (torque (right-branch m))))))

; ex31
(define (tree-map func tree)
  (cond ((null? tree)
          tree)
        ((pair? tree)
          (cons (tree-map func (car tree)) (tree-map func (cdr tree))))
        (else
          (func tree))))

; ex32
; We can use the following observations:
; 1) Set of all subsets of an empty set ({})
;    is a set consisting of an empty set ({{}}).
; 2) Set of all subsets of a non-empty set {a, b, ...}
;    consists of two kinds of sets: ones that contain `a`
;    and ones that don't.
;    The second kind of subsets forms a set of all subsets
;    of the set {b, ...}.
;    If we take any subset S of the first kind and look at S \ {a} -
;    we should get a subset of the second kind,
;    and likewise each subset of the second kind S' united with {a}
;    is a subset of the first kind.
; Therefore, the set S of all subsets of the set {a, b, ...}
; is S" U S', where S' is a set of all subsets of the set {b, ...}
; and S" := {S U {a} where S is from S'}

(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

; ex33
(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x a) (+ 1 a)) 0 seq))

; ex34
(define (horner-eval x coeffs)
  (accumulate (lambda coeff acc) (+ coeff (* x acc))
              0
              coeffs))

; ex35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

; ex36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ex37
(define (vector*vector v w)
  (accumulate + 0 (map * v w)))

(define (matrix*vector m v)
  (map (lambda (row) (vector*vector row v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix*matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix*vector cols row)) m)))

; ex39
(define (reverse-right seq)
  (fold-right (lambda (x acc) (append acc (list x))) '() seq))

(define (reverse-left seq)
  (fold-left (lambda (acc x) (cons x acc)) '() seq))

; ex40
;(define (flatmap func seq)
;  (foldr append '() (map func seq)))

;(define (seq start stop)
;  (if (> start stop) '() (cons start (seq (+ start 1) stop))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (seq 1 (- i 1))))
           (seq 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

; ex41
(define (tuples len lower upper)
  (if (= len 1)
      (map list (seq lower upper))
      (flatmap (lambda (i)
                 (let ((rest-tuples (tuples (- len 1) (+ i 1) upper)))
                   (map (lambda (t) (cons i t)) rest-tuples)))
               (seq lower upper))))
(define (sums-to? t s)
  (= (foldr + 0 t) s))
(define (unique-triplets n s)
  (filter (lambda (t) (sums-to? t s)) (tuples 3 1 n)))

; ex54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))

; ex 59
(define (element-of-set? x s)
  (and (not (null? s))
       (or (equal? x (car s))
           (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
  (if (element-of-set? x s) s (cons x s)))

(define (union-set s1 s2)
  (if (null? s1)
      s2
      (union-set (cdr s1) (adjoin-set (car s1) s2))))

; ex 61
(define (adjoin-set x s)
  (cond ((or (null? s) (< x (car s)))
          (cons x s))
        ((= x (car s))
          s)
        (else
          (cons (car s) (adjoin-set x (cdr s))))))

; ex 62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((= (car s1) (car s2))
          (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2))
          (cons (car s1) (union-set (cdr s1) s2)))
        (else
          (cons (car s2) (union-set s1 (cdr s2))))))
