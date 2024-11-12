#lang sicp
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5 6 7))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 3 9)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define atree (list 2 (list 3 (list 5 7) (list 8))))
(enumerate-tree atree)

(define (square x) (* x x))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares atree)

; 2.33
(newline)
(define (map-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define alist (list 0 9 8 7 6 5))
(map-accumulate square alist)

(define (append-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

(define blist (list 11 13 17 19))
(append-accumulate alist blist)

(define (length-accumulate sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length-accumulate alist)
(length-accumulate blist)

; 2.34
(newline)
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 1 + 3x + 5x^3 + x^5
(horner-eval 2 (list 1 3 0 5 0 1))

; 2.35
;(define (count-leaves tree)
;  (cond ((null? tree) 0)
;        ((not (pair? tree)) 1)
;        (else (+ (count-leaves (car tree))
;                 (count-leaves (cdr tree))))))
(newline)
(define (count-leaves-1 t)
  (accumulate +
              0
              (map (lambda (x) (+ 0 1)) (enumerate-tree t))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))

(count-leaves-1 atree)
(count-leaves atree)

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define list-of-list (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 list-of-list)


; 2.37 matrix
(newline)
(define avector (list 1 2 1 2))
(define amatrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define bmatrix (list (list 1 0 0) (list 0 1 0) (list 0 0 1) (list 1 0 0)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-vector amatrix avector)
(transpose amatrix)
(matrix-*-matrix amatrix bmatrix)

; 2.38
(newline)
(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter init sequence))

(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(fold-right + 0 (list 1 2 3)) ; 满足交换律
(fold-left + 0 (list 1 2 3))
(fold-right cons nil (list 1 2 3))
(fold-left cons nil (list 1 2 3))

; 2.39
; (op 1st (op 2nd (op 3rd init)))
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

; (op (op (op init 1st) 2nd) 3rd)
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-left (list 1 2 3 4 5))
(reverse-right (list 1 2 3 4 5))
