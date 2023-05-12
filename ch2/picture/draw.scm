(load "vect")

(define (start-image)
  (display "%!PS-Adobe-3.0 EPSF-3.0\n")
  (display "%%BoundingBox: 0 0 400 400\n")
  (display "%%EndComments\n")
  (display ".25 setlinewidth\n"))

(define (end-image)
  (display "showpage\n")
  (display "%%EOF\n"))

(define (draw-line a b)
  (display "newpath\n")
  (for-each display (list (* 400. (vect-x a)) " " (* 400. (vect-y a)) " moveto\n"))
  (for-each display (list (* 400. (vect-x b)) " " (* 400. (vect-y b)) " lineto\n"))
  (display "stroke\n"))
