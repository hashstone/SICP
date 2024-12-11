#lang sicp

;; Huffman Tree
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

; hint, symbols & weight interface make adjoin-set could process leaf and tree-node
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display sample-message)
(newline)
; '(A D A B B C A)
(decode sample-message sample-tree)

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (define (iter sub-tree code)
    (cond ((null? sub-tree) '())
          ((leaf? sub-tree)
           (if (eq? (symbol-leaf sub-tree) char)
               code
               '()))
          (else (append (iter (left-branch sub-tree) (append code '(0)))
                        (iter (right-branch sub-tree) (append code '(1)))))))
  (let ((code (iter tree '())))
    (if (null? code)
        (error "bad char -- not support " char)
        code)))

(encode '(A D A B B C A) sample-tree)
; test error message
;(encode '(A D A B B C E) sample-tree)


; 2.69
(newline)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define test-pairs '((A 4) (C 1) (B 2) (D 1)))

(make-leaf-set test-pairs)

(define (successive-merge sorted-pairs)
  (if (null? (cdr sorted-pairs))
      (car sorted-pairs)
      (successive-merge (adjoin-set
                         (make-code-tree (car sorted-pairs)
                                         (cadr sorted-pairs))
                         (cddr sorted-pairs)))))

(successive-merge (make-leaf-set test-pairs))
(display sample-tree)

; 2.70
(define music-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define music-huffman-tree (generate-huffman-tree music-pairs))
;(display music-huffman-tree)
(define lyric '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(newline)
(encode lyric music-huffman-tree)

; 2.71
; most frequently use 1 bit
; least frequently use n-1 bit

; 2.72
; ?
; most frequently O(n)
; least frequently O(n^2)