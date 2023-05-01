(load "enc")

(define sha-na-text
  '(
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM
  )
)

(define sha-na-tree
  (generate-huffman-tree
    '(
      (A 2)
      (NA 16)
      (BOOM 1)
      (SHA 3)
      (GET 2)
      (YIP 9)
      (JOB 2)
      (WAH 1)
    )
  )
)

(define sha-na-code
  (encode sha-na-text sha-na-tree)
)
