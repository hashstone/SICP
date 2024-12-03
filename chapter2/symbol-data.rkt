#lang racket
(include "symbol-data-base.rkt")

; memq
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

; 2.53
; (a b c)
(list 'a 'b 'c)
; ((george))
(list (list 'george))
;((y1 y2))
(cdr '((x1 x2) (y1 y2)))
; (y1, y2)
(cadr '((x1 x2) (y1 y2)))
; #f
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; (red shoes blue socks)
(memq 'red '(red shoes blue socks))

; 2.54
(newline)
(define (equal? x y)
   (cond (
          (null? x) (null? y))
          ((null? y) false)
          ((pair? x)
                 (if (pair? y)
                        (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
                        false
                 )
          )
          ((pair? y) false)
          (else (eq? x y))
   )
)

(equal? '() '())
(equal? '() '(a b c))
(equal? '(a b c) '())
(equal? '(a (b c)) '(a (c b)))
(equal? '(x y z) '(x y z))

; 2.55
(newline)
'a
''a
(car ''abracadabra)
(cdr ''abracadabra)
(cadr ''abracadabra)

; deriv
; variable
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? x num)
  (and (number? x) (= x num)))

; pre-order sum
;(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

; (define (augend s) (caddr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

; pre-order mul
;(define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

; pre-order exponentiation, 2.56
(define (make-exponentiation base exponent)
  (cond ((number? base) (error "not support number base" (list '** base exponent)))
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

; deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(newline)
(deriv '(+ 3 x) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 2.56 test
(newline)
(deriv '(** x 5) 'y)
(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)
(deriv '(** x 5) 'x)
(deriv '(** (* x y) 6) 'x)

; 2.57 test
(newline)
(deriv '(* x y (+ x 3)) 'x)

; 2.58
; mid-order sum
(define (mid-order-sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (mid-order-addend s) (car s))

(define (mid-order-augend s)
  (if (or (null? (cdddr s)) (eq? (cadddr s) '+))
      (caddr s)
      (cddr s)))

; mid-order mul
(define (mid-order-product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (mid-order-multiplier p) (car p))

(define (mid-order-multiplicand p) (caddr p))

; mid-order exponentiation, 2.56
(define (mid-order-exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (mid-order-base e) (car e))

(define (mid-order-exponent e) (caddr e))

(define (mid-order-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((mid-order-sum? exp)
         (make-sum (mid-order-deriv (mid-order-addend exp) var)
                   (mid-order-deriv (mid-order-augend exp) var)))
        ((mid-order-product? exp)
         (make-sum
          (make-product (mid-order-multiplier exp)
                        (mid-order-deriv (mid-order-multiplicand exp) var))
          (make-product (mid-order-deriv (mid-order-multiplier exp) var)
                        (mid-order-multiplicand exp))))
        ((mid-order-exponentiation? exp)
         (make-product
          (make-product (mid-order-exponent exp)
                        (make-exponentiation (mid-order-base exp) (- (mid-order-exponent exp) 1)))
          (mid-order-deriv (mid-order-base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

; 2.58.a
(newline)
(mid-order-deriv '(x + (3 * (x + (y + 2)))) 'x)
(mid-order-deriv '(x + (3 * (x + (y + 2)))) 'y)
(mid-order-deriv '((x ** 3) + (3 * (x + (y + 2)))) 'x)


; 2.58.b
; not work yet!
(newline)
(mid-order-deriv '(x + 3 * (x + y + 2)) 'x)
;(mid-order-deriv '(x + x * y * x + x * x * x) 'x)


;;;; symbol set data-structure
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
(newline)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(define set1 (list 1 2 3 4 5))
(define set2 (list 3 5 7 8 9))
(element-of-set? 10 set1)
(element-of-set? 3 set2)
(define set3 (adjoin-set 10 set1))
(element-of-set? 10 set3)
(intersection-set set1 set2)
(union-set set1 set2)

; 2.60 multi-set
(define (adjoin-multi-set x set)
  (cons x set)
)

(define (union-multi-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (append set1 set2))
  )
)