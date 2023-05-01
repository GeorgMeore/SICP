(load "vect")
(load "frame")

(define (rotate painter deg)
	(define pi/4 (atan 1))
	(define (degrees->radians deg)
		(* deg (/ pi/4 45)))
	(define (normalize x n)
		(- x (* n (floor (/ x n)))))
	(define (rotate-frame frame deg)
		(let (
			(deg
				(normalize deg 360))
			(x
				(/ (+ (tan (- (degrees->radians (normalize deg 90)) pi/4)) 1) 2))
		)
			(cond
				((< deg 90)
					(transform-frame frame
						(make-vect x 0)
						(make-vect (- 1 x) x)
						(make-vect (- x) (- 1 x))
					)
				)
				((< deg 180)
					(transform-frame frame
						(make-vect 1 x)
						(make-vect (- x) (- 1 x))
						(make-vect (- x 1) (- x))
					)
				)
				((< deg 270)
					(transform-frame frame
						(make-vect (- 1 x) 1)
						(make-vect (- x 1) (- x))
						(make-vect x (- x 1))
					)
				)
				(else
					(transform-frame frame
						(make-vect 0 (- 1 x))
						(make-vect x (- x 1))
						(make-vect (- 1 x) x)
					)
				)
			)
		)
	)
	(lambda (frame)
		(painter (rotate-frame frame deg))
	)
)

(define (flip-vert painter)
	(lambda (frame)
		(painter
			(transform-frame frame
				(make-vect 1 0)
				(make-vect -1 0)
				(make-vect 0 1)
			)
		)
	)
)

(define (flip-horiz painter)
	(lambda (frame)
		(painter
			(transform-frame frame
				(make-vect 0 1)
				(make-vect 1 0)
				(make-vect 0 -1)
			)
		)
	)
)

(define (flip-diag painter)
	(flip-vert (flip-horiz painter))
)

(define (beside-scaled left right scale)
	(lambda (frame)
		(left
    		(transform-frame frame
    			(make-vect 0 0)
    			(make-vect scale 0)
    			(make-vect 0 1)
    		)
		)
		(right
			(transform-frame frame
				(make-vect scale 0)
				(make-vect (- 1 scale) 0)
				(make-vect 0 1)
			)
		)
	)
)

(define (below-scaled top bottom scale)
	(lambda (frame)
		(top
			(transform-frame frame
				(make-vect 0 (- 1 scale))
				(make-vect 1 0)
				(make-vect 0 scale)
			)
		)
		(bottom
			(transform-frame frame
				(make-vect 0 0)
				(make-vect 1 0)
				(make-vect 0 (- 1 scale))
			)
		)
	)
)

(define (beside left right)
	(beside-scaled left right 1/2)
)

(define (below top bottom)
	(below-scaled top bottom 1/2)
)

(define (overlay bottom top)
	(lambda (frame)
		(bottom frame)
		(top frame)
	)
)

(define (right-split painter n)
	(if (> n 0)
		(let ((rest (right-split painter (- n 1))))
			(beside painter (below rest rest))
		)
		painter
	)
)

(define (corner-split painter n)
	(if (> n 0)
		(let ((rest (corner-split painter (- n 1))))
			(beside
				(below rest painter)
				(below rest rest)
			)
		)
		painter
	)
)

(define (square-of-four tl tr bl br)
	(lambda (painter)
		(below
			(beside (tl painter) (tr painter))
			(beside (bl painter) (br painter))
		)
	)
)

(define (row . painters)
	(define (n-row painters n)
		(if (> n 1)
			(beside-scaled
				(car painters)
				(n-row (cdr painters) (- n 1))
				(/ 1 n)
			)
			(car painters)
		)
	)
	(n-row painters (length painters))
)

(define (column . painters)
	(define (n-column painters n)
		(if (> n 1)
			(below-scaled
				(car painters)
				(n-column (cdr painters) (- n 1))
				(/ 1 n)
			)
			(car painters)
		)
	)
	(n-column painters (length painters))
)
