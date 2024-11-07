#lang sicp
(define (list-ref l n)
  (cond ((null? l) nil)
        ((= n 0) (car l))
        (else (list-ref (cdr l) (- n 1)))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define alist (list 0 1 2 3 4 5 6 7))
(define blist (list 11 12 13 14))

(list-ref alist 10)
(list-ref alist 1)
(list-ref alist 4)
(length alist)
(append alist blist)
(append blist alist)

; 2.17
(newline)
(define (last-pair l)
  (define (last-pair-iter li next)
    (if (null? next)
        li
        (last-pair-iter (cdr li) (cdr next))))
  (last-pair-iter l (cdr l)))

(last-pair alist)
(last-pair blist)

; 2.18
(newline)
(define (reverse l)
  (define (reverse-iter ol nl)
    (if (null? ol)
        nl
        (reverse-iter (cdr ol) (cons (car ol) nl))))
  (reverse-iter l nil))

(reverse alist)
(reverse blist)

; 2.19
(newline)

(define (except-first-denomination li)
  (cdr li))

(define (first-denomination li)
  (car li))

(define (no-more? li)
  (null? li))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins (list 25 50 10 1 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)
(cc 50 uk-coins)

; 2.20
(newline)
(define (same-parity . args)
  (define (same-parity-iter ans i)
    (if (not (null? ans))
        (if (= (remainder i 2) 1)
            (cons (car ans) (same-parity-iter (cdr ans) (+ i 1)))
            (same-parity-iter (cdr ans) (+ i 1)))
        nil))
  ;(display args)
  ;(newline)
  (same-parity-iter args 1))

(same-parity 1 2 3 4 5 6)
(same-parity 2 3 4 5 6 7 8)
