#lang sicp
; queue
(define (front-ptr queue) (car queue))
(define (rear-ptr  queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr!  queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-cdr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue) (car queue))

; 3.21
(display "\n---> begin test 3.21\n")
(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

; 3.22
(display "\n---> begin test 3.22\n")
(define (make-queue-by-local-status)
  (let ((front-ptr '())
        (rear-ptr  '()))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
      (error "FRONT called with an empty queue")
      (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr  new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'empty?) empty-queue?)
            ((eq? m 'front) front-queue)
            ((eq? m 'push!) insert-queue!)
            ((eq? m 'pop!) delete-queue!)
            (else (error "UNSUPPORTED method" m))))

    dispatch))

(define q2 (make-queue-by-local-status))
((q2 'empty?))
((q2 'push!) 'a)
((q2 'push!) 'b)
((q2 'push!) 'c)
((q2 'pop!))
((q2 'empty?))


; 3.23
(display "\n---> begin test 3.23\n")
(define (make-dequeue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (or (null? front-ptr) (null? rear-ptr)))

    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (back)
      (if (empty?)
          (error  "BACK called with an empty queue")
          (car rear-ptr)))

    (define (make-node item) (list item '() '()))

    (define (node-value node) (car node))

    (define (node-next-ptr node) (cadr node))

    (define (node-prev-ptr node) (caddr node))

    (define (set-next-ptr! node v) (set-car! (cdr node) v))

    (define (set-prev-ptr! node v) (set-car! (cddr node) v))

    (define (push-front! item)
      (let ((new-pair (make-node item)))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-prev-ptr! front-ptr new-pair)
               (set-next-ptr! new-pair front-ptr)
               (set! front-ptr new-pair)
               front-ptr))))

    (define (push-back! item)
      (let ((new-pair (make-node item)))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-next-ptr! rear-ptr new-pair)
               (set-prev-ptr! new-pair rear-ptr)
               (set! rear-ptr new-pair)
               front-ptr))))

    (define (pop-front!)
      (cond ((empty?) (error "POP-FRONT! called with an empty queue"))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '())
             front-ptr)
            (else
             (set-prev-ptr! (node-next-ptr front-ptr) '())
             (set! front-ptr (node-next-ptr front-ptr))
             front-ptr)))

    (define (pop-back!)
      (cond ((empty?) (error "POP-BACK! called with an empty queue"))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '())
             front-ptr)
            (else
             (set-next-ptr! (node-prev-ptr rear-ptr) '())
             (set! rear-ptr (node-prev-ptr rear-ptr))
             front-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'back) back)
            ((eq? m 'push-front!) push-front!)
            ((eq? m 'push-back!) push-back!)
            ((eq? m 'pop-front!) pop-front!)
            ((eq? m 'pop-back!) pop-back!)
            ((eq? m 'print) print)
            ((eq? m 'reverse-print) reverse-print)
            (else (error "UNSUPPORTED method" m))))

    (define (print)
      (define (iter node)
        (if (null? node)
            '()
            (cons (node-value node) (iter (node-next-ptr node)))))
      (iter front-ptr))

    (define (reverse-print)
      (define (iter node)
        (if (null? node)
            '()
            (cons (node-value node) (iter (node-prev-ptr node)))))
      (iter rear-ptr))

    dispatch))

(define q3 (make-dequeue))
((q3 'push-back!) 'a)
((q3 'push-back!) 'b)
((q3 'push-back!) 'c)
((q3 'push-back!) 'd)
((q3 'print))
((q3 'reverse-print))
((q3 'push-front!) 'A)
((q3 'push-front!) 'B)
((q3 'push-front!) 'C)
((q3 'print))
((q3 'reverse-print))
((q3 'pop-front!))
((q3 'pop-back!))
((q3 'print))
