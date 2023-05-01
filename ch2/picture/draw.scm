(load "vect")

(define (start-image)
	(display ".PS\n")
)

(define (end-image)
	(display ".PE\n")
)

(define (draw-line a b)
	(display "line from")
	(for-each display (list " (" (vect-x a) ", " (vect-y a) ") "))
	(display "to")
	(for-each display (list " (" (vect-x b) ", " (vect-y b) ")\n"))
)
