(define (sum-flatten terms)
	(if (null? terms)
		'()
		(let ((head (car terms)) (tail (cdr terms)))
			(if (sum? head)
				(sum-flatten (cons (addend head) (cons (augend head) tail)))
				(cons head (sum-flatten tail))
			)
		)
	)
)

(define (sum-reduce-numbers terms)
	(define (reductor term acc)
		(let ((num (car acc)) (other (cdr acc)))
			(if (number? term)
				(cons (+ num term) other)
				(cons num (cons term other))
			)
		)
	)
	(let ((reduced (foldr reductor '(0) terms)))
		(cond
			((null? (cdr reduced)) reduced)
			((= (car reduced) 0) (cdr reduced))
			(else reduced)
		)
	)
)

(define (chain argument . functions)
	(foldl
		(lambda (v f) (f v))
		argument
		functions
	)
)

(define (sum-reduce terms)
	(chain terms
		sum-flatten
		sum-reduce-numbers
	)
)

(define (product-flatten terms)
	(if (null? terms)
		'()
		(let ((head (car terms)) (tail (cdr terms)))
			(if (product? head)
				(product-flatten (cons (multiplier head) (cons (multiplicand head) tail)))
				(cons head (product-flatten tail))
			)
		)
	)
)

(define (product-reduce-numbers terms)
	(define (reductor term acc)
		(let ((num (car acc)) (other (cdr acc)))
			(if (number? term)
				(cons (* num term) other)
				(cons num (cons term other))
			)
		)
	)
	(let ((reduced (foldr reductor '(1) terms)))
		(cond
			((null? (cdr reduced)) reduced)
			((= (car reduced) 0) '(0))
			((= (car reduced) 1) (cdr reduced))
			(else reduced)
		)
	)
)

(define (product-reduce terms)
	(chain terms
		product-flatten
		product-reduce-numbers
	)
)
