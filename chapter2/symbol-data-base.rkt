(define (memq item l)
  (cond ((null? l) false)
        ((eq? item (car l)) l)
        (else (memq item (cdr l)))))