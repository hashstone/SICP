#lang sicp

; 3.12
(display "\n---> begin test 3.12\n")
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

; ('b)
(cdr x)

(define w (append! x y))
; ('b 'c 'd)
(cdr x)

; 3.13
(define (make-cycle x)
  (append! x x))

(define loop-z (make-cycle (list 'a 'b 'c)))
; forever loop
;(last-pair loop-z)

; 3.14
(display "\n---> begin test 3.14\n")
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define l (list 'a 'b 'c 'd))
(define reverse-l (mystery l))
; (a) <--
(display l)
(newline)
; ('d 'c 'b 'a)
(display reverse-l)
(newline)

; 3.16
(display "\n---> begin test 3.16\n")
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (dup-pair x)
  (cons x x))

(define one-pair (cons 'a '()))
(define two-pairs (cons 'b one-pair))
(define three-pairs (cons 'c two-pairs))
(define four-pairs (cons two-pairs one-pair))
(define dup-one-pair (dup-pair one-pair))
(define seven-pairs (dup-pair dup-one-pair))

(count-pairs one-pair)
(count-pairs two-pairs)
(count-pairs three-pairs)
(count-pairs four-pairs)
(count-pairs seven-pairs)

; 3.17
(display "\n---> begin test 3.17\n")
(define (count-distinct-pairs x)
  (define visited '())

  (define (exist? l x)
    (cond ((null? l) #f)
          ((eq? (car l) x) #t)
          (else (exist? (cdr l) x))))

  (define (visit x)
    (set! visited (cons x visited)))

  (define (dump l)
    (if (null? l)
        0
        (begin (display (car l))
               (newline)
               (dump (cdr l)))))

  (define (iter x)
    (if (not (pair? x))
        0
        (if (exist? visited x)
            0
            (begin (visit x)
                   (iter (car x))
                   (iter (cdr x))))))

  (iter x)
  (dump visited)
  (length visited))

(count-distinct-pairs four-pairs)
(count-distinct-pairs seven-pairs)

; functional program
(define (memq item l)
  (cond ((null? l) false)
        ((eq? item (car l)) true)
        (else (memq item (cdr l)))))

(define (count-dist-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x) (not (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))

(count-dist-pairs four-pairs)
(count-dist-pairs seven-pairs)


; 3.18 - 3.19
(display "\n---> begin test 3.18\n")
(define (has-cycle? l)
  (define (check slow fast)
    (if (or (null? fast) (null? (cdr fast)))
        #f
        (let ((next-slow (cdr slow))
              (next-fast (cddr fast)))
          (if (eq? next-slow next-fast)
              #t
              (check next-slow next-fast)))))
  (check l l))

(has-cycle? loop-z)
(has-cycle? l)
(has-cycle? (make-cycle l))
