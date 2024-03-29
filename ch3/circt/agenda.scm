(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))
(define (segment-queue s)
  (cdr s))

(define (make-agenda)
  (cons 0 '()))

(define (current-time agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (make-new-segment time action)
    (let ((queue (make-queue)))
      (insert-queue! queue action)
      (make-time-segment time queue)))
  (define (insort! time action head)
    (let ((segments (cdr head)))
      (cond ((null? segments)
              (set-cdr! head (list (make-new-segment time action))))
            ((< time (segment-time (car segments)))
              (set-cdr! head (cons (make-new-segment time action) segments)))
            ((= time (segment-time (car segments)))
              (insert-queue! (segment-queue (car segments)) action))
            (else
              (insort! time action segments)))))
  (insort! time action agenda))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "first-agenda-item called with empty agenda")
      (let ((first (first-segment agenda)))
        (set-current-time! agenda (segment-time first))
        (front-queue (segment-queue first)))))

(define (delete-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
          (set-segments! agenda (rest-segments agenda)))))
