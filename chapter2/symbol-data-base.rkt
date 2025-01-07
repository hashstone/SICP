(define (memq item l)
  (cond ((null? l) false)
        ((eq? item (car l)) true)
        (else (memq item (cdr l)))))
