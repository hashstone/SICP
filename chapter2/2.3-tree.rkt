#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-2 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '() '()))))
(define tree2 (make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

; a)
(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

; b)
; tree->list-1 (n * append + n * cons)
; tree->list-2 (n * cons)

; 2.64
(newline)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)

  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          ;(display "------")
          ;(newline)
          ;(display left-result)
          ;(newline)
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                ;(display this-entry)
                ;(display left-tree)
                ;(display right-tree)
                ;(newline)
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

; 2.65
(newline)
(define (intersection-sorted-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-sorted-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-sorted-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-sorted-set set1 (cdr set2)))))))

(define (union-sorted-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (if (< x1 x2)
               (cons x1 (union-sorted-set (cdr set1) set2))
               (cons x2 (union-sorted-set set1 (cdr set2))))))))

(define (union-tree tree1 tree2)
  (let ((sorted-list (union-sorted-set (tree->list-2 tree1) (tree->list-2 tree2))))
    (list->tree sorted-list)))

(define (intersection-tree tree1 tree2)
  (let ((sorted-list (intersection-sorted-set (tree->list-2 tree1) (tree->list-2 tree2))))
    (list->tree sorted-list)))

(define tree-odd (list->tree (list 1 3 5 7 9 11)))
(define tree-ov (list->tree (list 2 3 7 8 10 11)))

(union-tree tree-odd tree-ov)
(intersection-tree tree-odd tree-ov)

; 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records)) (entry set-of-records))
        ((< given-key (entry set-of-records)) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))
