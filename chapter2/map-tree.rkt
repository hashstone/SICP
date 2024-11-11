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


; 2.30
(newline)
(define (square-tree-nomap tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-nomap (car tree))
                    (square-tree-nomap (cdr tree))))))

(square-tree-nomap (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-nomap atree)

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree atree)


; 2.31
(newline)
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square x) (* x x))
(define (square-tree-map tree) (tree-map square tree))

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-map atree)

; 2.32
; (()) -> (() (3)) -> (() (3) (2) (2 3)) -> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(newline)
(define clist (list 1 2 3))
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(subsets clist)
