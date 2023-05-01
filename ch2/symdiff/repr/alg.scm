; standard algebraric notation (without any reductions)
(load "reduce")

(define variable? symbol?)
(define same-variable? eq?)

(define (split lst sym)
	(cond
		((null? lst)
			(cons '() '())
		)
		((eq? (car lst) sym)
			(cons '() (cdr lst))
		)
		(else
			(let ((cdr-split (split (cdr lst) sym)))
				(cons
					(cons (car lst) (car cdr-split))
					(cdr cdr-split)
				)
			)
		)
	)
)
(define (adjoin lst x)
	(foldr
		(lambda (term acc)
			(if (null? acc)
				(cons term acc)
				(cons term (cons x acc))
			)
		)
		'()
		lst
	)
)
(define (unlist lst)
	(if (null? (cdr lst))
		(car lst)
		lst
	)
)

(define (getop exp)
	(if (pair? exp)
		(cond
			; from lowest to highest priority
			((memq '+ exp) '+)
			((memq '* exp) '*)
			((memq '** exp) '**)
		)
	)
)

(define (make-sum . terms)
	(let ((reduced (sum-reduce terms)))
		(if (= (length reduced) 1)
			(car reduced)
			(adjoin reduced '+)
		)
	)
)
(define (sum? exp)
	(eq? (getop exp) '+)
)
(define (addend s)
	(unlist (car (split s '+)))
)
(define (augend s)
	(unlist (cdr (split s '+)))
)

(define (make-product . terms)
	(let ((reduced (product-reduce terms)))
		(if (= (length reduced) 1)
			(car reduced)
			(adjoin reduced '*)
		)
	)
)
(define (product? exp)
	(eq? (getop exp) '*)
)
(define (multiplier p)
	(unlist (car (split p '*)))
)
(define (multiplicand p)
	(unlist (cdr (split p '*)))
)

(define (make-exponentiation b e)
	(if (not (number? e))
		(error "number expected" e)
	)
	(cond
		((= e 0) 1)
		((= e 1) b)
		((number? b) (expt b e))
		(else (list b '** e))
	)
)
(define (exponentiation? exp)
	(eq? (getop exp) '**)
)
(define (base p)
	(unlist (car (split p '**)))
)
(define (exponent p)
	(unlist (cdr (split p '**)))
)
