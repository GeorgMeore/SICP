; ex2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* (- 6 2) (- 2 7)))

; ex3
(define (maxsqsum x y z)
  (define (sq x) (* x x))
  (define (sqsum x y) (+ (sq x) (sq y)))
  (cond ((>= x y) (sqsum x (if (>= z y) z y)))
        ((>= y z) (sqsum y (if (>= z x) z x)))
        (else     (sqsum y z))))

; ex7
(define (good-enough? old-guess new-guess)
  (< (abs (/ (- new-guess old-guess) new-guess))
     0.001))

(define (square-root x)
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (step old-guess new-guess)
    (if (good-enough? old-guess new-guess)
        new-guess
        (iter new-guess)))
  (define (iter guess)
    (step guess (improve guess)))
  (iter 1.0))

; ex8
(define (cube-root x)
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (step old-guess new-guess)
    (if (good-enough? old-guess new-guess)
        new-guess
        (iter new-guess)))
  (define (iter guess)
    (step guess (improve guess)))
  (iter 1.0))

; ex10
; (define (f n) (A 0 n)) : n -> 2*n
; (define (g n) (A 1 n)) : n -> 2^n
;   A(1, n) -> A(0, A(1, n-1)) -> 2*A(1, n-1) => g(n+1) = 2*g(n)
;   A(1, 1) -> 2 => g(1) = 2
; (define (h n) (A 2 n)) : n -> 2^(2^(...)) n-1 times
;   A(2, n) -> A(1, A(2, n-1)) -> 2^A(2, n-1) => g(n+1) = 2^g(n)
;   A(2, 1) -> 2 => g(1) = 2

; ex11
(define (f-next ppp pp p) (+ p (* 2 pp) (* 3 ppp)))

(define (f1 n)
  (if (< n 3) n (f-next (f1 (- n 3)) (f1 (- n 2)) (f1 (- n 1)))))

(define (f2 n)
  (define (iter ppp pp p n)
    (cond ((= n 0) p)
          (else (iter pp p (f-next ppp pp p) (- n 1)))))
  (if (< n 3) n (iter 0 1 2 (- n 2))))

; ex12
(define (C n k)
  (cond ((or (= k 0) (= n k)) 1)
        (else (+ (C (- n 1) (- k 1))
                 (C (- n 1) k)))))

; ex16
(define (fexp x n)
  (define (iter acc base n)
    (cond ((= n 0)
            acc)
          ((even? n)
            (iter acc (* base base) (/ n 2)))
          (else
            (iter (* acc base) base (- n 1)))))
  (iter 1 x n))

; ex17
(define (fmul1 a b)
  (cond ((= b 0)
          0)
        ((= b 1)
          a)
        ((even? b)
          (* 2 (fmul1 a (/ b 2))))
        (else
          (+ a (fmul1 a (- b 1))))))

; ex18
(define (fmul2 a b)
  (define (iter acc a b)
    (cond ((= b 0)
            acc)
          ((even? b)
            (iter acc (* a 2) (/ b 2)))
          (else
            (iter (+ acc a) a (- b 1)))))
  (iter 0 a b))

; ex19
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0)
            b)
          ((even? count)
            (iter a
                  b
                  (+ (* p p) (* q q))
                  (+ (* q q) (* 2 p q))
                  (/ count 2)))
          (else
            (iter (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1)))))
  (iter 1 0 0 1 n))

; ex29
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (next a) (+ a h h))
  (* (/ h 3)
     (+ (f a)
        (* 4 (sum f (+ a h) next b))
        (* 2 (sum f (+ a h h) next (- b h)))
        (f b))))

; ex30
(define (sum term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (+ acc (term a)))))
  (iter a 0))

; ex31
(define (product term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (* acc (term a)))))
  (iter a 1))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (next a) (+ a 1))
  (product identity 1 next n))

(define (pi n)
  (define (term a)
    (define 2a (* 2 a))
    (* (/ 2a (- 2a 1))
       (/ 2a (+ 2a 1))))
  (define (next a) (+ a 1))
  (define (pi/2)
    (product term 1 next n))
  (* 2 (pi/2)))

; ex32
(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combiner (term a) acc))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

; ex33
(define (filtered-accumulate filter combiner null term a next b)
  (define (iter a acc)
    (cond ((> a b)
            acc)
          ((filter a)
            (iter (next a) (combiner (term a) acc)))
          (else
            (iter (next a) acc))))
  (iter a null))

(define (prime? a)
  (define (iter i)
    (cond ((> (* i i) a) #t)
          ((= 0 (remainder a i)) #f)
          (else (iter (+ i 1)))))
  (iter 2))

(define (sum-prime-squares a b)
  (define (term a) (* a a))
  (define (next a) (+ a 1))
  (filtered-accumulate prime? + 0 term a next b))

(define (relative-prime? i n)
  (= (gcd i n) 1))

(define (sum-relative-primes n)
  (define (filter a) (relative-prime? a n))
  (define (next a) (+ a 1))
  (filtered-accumulate filter + 0 identity 1 next n))

; ex34
; (define (f g) (g 2))
; (f f) -> (f 2) -> (2 2) -> Error: call of non-procedure

; ex37
(define (cont-frac n d k)
  (define (helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 1))

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (1/phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

; ex38
(define (e-2 k)
  (define (d i)
    (if (= 2 (remainder i 3))
        (* 2 (+ (quotient i 3) 1))
        1))
  (cont-frac (lambda (i) 1.0) d k))

; ex39
(define (tan-cf x k)
  (define (d i) (- (* 2 i) 1))
  (define (n i) (if (= i 1) x (* x x -1)))
  (cont-frac n d k))

; ex41
(define (double f)
  (lambda (x) (f (f x))))

; ex42
(define (compose f g)
  (lambda (x) (f (g x))))

; ex43
(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))

; ex44
(define dx 0.0001)

(define (smooth f)
  (let ((sum (+ (f (- x dx)) (f x) (f (+ x dx)))))
    (lambda (x) (/ sum 3))))

(define (n-fold-smooth f n)
  (repeated smooth n) f)
