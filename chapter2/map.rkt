#lang sicp
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* x x)) (list 1 2 3 4 5 6))

; 2.21
(newline)
(define (square x)
  (* x x))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons ((lambda (x) (* x x)) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(define alist (list 1 2 3 4 5 6))

(square-list-1 alist)
(square-list-2 alist)

; 2.22
(newline)
(define (square-list-iter-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items nil))

(square-list-iter-1 alist)

(define (square-list-iter-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

(square-list-iter-2 alist)

; 2.23
(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items)) (for-each proc (cdr items)))))

(for-each (lambda (x) (display x) (newline)) alist)

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(newline)
(define atree (cons (list 1 2) (list 3 4 5)))
(length atree)

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(count-leaves atree)

; 2.24
; (list 1 (list 2 (list 3 4)))
; +---+
; |   |
; 1   +----+
;     |    |
;     2    +----4
;          |
;          3


; 2.25
(newline)
(define li1 (list 1 3 (list 5 7) 9))
(define li2 (list (list 7)))
(define li3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr li1)))))
(car (car li2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr li3))))))))))))

; 2.26
(define li4 (list 1 2 3))
(define li5 (list 4 5 6))
(append li4 li5)
(cons li4 li5)
(list li4 li5) ; = (cons li4 (cons li5 nil))

; 2.27
(newline)
(define (reverse l)
  (define (reverse-iter ol nl)
    (if (null? ol)
        nl
        (reverse-iter (cdr ol) (cons (car ol) nl))))
  (reverse-iter l nil))

(define (deep-reverse l)
  (define (reverse-iter ol nl)
    (cond ((null? ol) nl)
          ((not (pair? ol)) ol)
          (else (reverse-iter (cdr ol) (cons (deep-reverse (car ol)) nl)))))
  (reverse-iter l nil))

(display li1)
(newline)
(reverse li1)
(deep-reverse li1)

; 2.28
(newline)
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (cons tree nil)) ; construct a list for using append operation
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(fringe li1)
(fringe li3)

; 2.29
(newline)
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile))
     (total-branch-weight (right-branch mobile))))

(define branch1 (make-branch 11 10))
(define branch2 (make-branch 10 11))
(define mobile1 (make-mobile branch1 branch2))

(define branch3 (make-branch 4 16))
(define branch4 (make-branch 3 mobile1))
(define mobile2 (make-mobile branch3 branch4))

(total-weight mobile1)
(total-weight mobile2)

(define (is-balance mobile)
  (define (is-balance-branch branch)
    (if (pair? (branch-structure branch))
        (is-balance (branch-structure branch))
        #t))
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (and (is-balance-branch lb)
         (is-balance-branch rb)
         (= (* (branch-length lb) (total-branch-weight lb))
            (* (branch-length rb) (total-branch-weight rb))))))

(is-balance mobile1)
(is-balance mobile2)
