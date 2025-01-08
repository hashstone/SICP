#lang sicp

(define (make-1d-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define (make-2d-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define operation-table (make-2d-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; 3.24
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (same? x y)
  (let ((tolerance 0.002))
    ;(display x) (display " ") (display y) (newline)
    ;(display (and (real? x) (real? y)))
    ;(newline)
    (cond ((equal? x y) true)
          ((and (real? x) (real? y) (< (abs (- x y)) tolerance)) true)
          (else false))))

(define t1 (make-2d-table equal?))
((t1 'insert-proc!) 'a 0.011 'a1)
((t1 'insert-proc!) 'a 0.013 'a2)
((t1 'lookup-proc) 'a 0.014)
((t1 'lookup-proc) 'a 0.011)

(define t2 (make-2d-table same?))
((t2 'insert-proc!) 'a 0.011 'a1)
((t2 'insert-proc!) 'a 0.013 'a2)
((t2 'lookup-proc) 'a 0.010)
((t2 'lookup-proc) 'a 0.014)

; 3.25, 3.26 TODO

; 3.27
(define (memoize f)
  (let ((table (make-1d-table equal?)))
    (lambda (x)
      (let ((memoized-value ((table 'lookup-proc) x)))
        (or memoized-value
            (let ((result (f x)))
              ((table 'insert-proc!) x result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


(memo-fib 1)
(memo-fib 2)
(memo-fib 3)
(memo-fib 4)
(memo-fib 5)
(memo-fib 6)
; (memoize fib) is not worked, memoize is called only once
