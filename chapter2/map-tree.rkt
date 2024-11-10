#lang sicp
(define atree (cons (list 1 2) (list 3 4 5)))
;(length atree)

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(count-leaves atree)

(define (scale-tree-raw tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree-raw (car tree) factor)
                    (scale-tree-raw (cdr tree) factor)))))

(scale-tree-raw (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(scale-tree-raw atree 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(scale-tree atree 10)
