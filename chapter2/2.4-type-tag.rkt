#lang racket
(include "put-get.rkt")

(define (square x) (* x x))

; Rectangular Coordinates
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag '(rectangular)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(rectangular)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar Coordinates
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-mag-ang r a)
    (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag '(polar)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)



(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these type -- APPLY-GENERIC" (list op type-tags)))
    )
  )
)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))

; get <op> <type>
; <type> should be '(xxx), not 'xxx
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag '(rectangular)) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang '(polar)) r a))

(put 'square 'num (lambda (x) (* x x)))
((get 'square 'num) 3)

(install-rectangular-package)
(install-polar-package)
(real-part (make-from-real-imag 3 4))
(imag-part (make-from-real-imag 3 4))
(magnitude (make-from-real-imag 3 4))
(angle (make-from-real-imag 3 4))


; 2.73
(newline)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? x num)
  (and (number? x) (= x num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a)
; 因为 number? 和 same-variable? 是内置操作，用于判定exp 为基础操作数（数字或符号）；
; 当然也是可以将其安装进入表格的, 例如： (tag 'number number) (tag 'symbol symbol)

; b)
(define (install-sum-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (addend s) (car s))

  ; (define (augend s) (caddr s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ;; interface
  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (multiplier p) (car p))

  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (cons '* (cdr p))))

  (define (deriv-mul exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  ;; interface
  (put 'make-product '* make-product)
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  (put 'deriv '* deriv-mul)
  'done)

(install-sum-package)
(install-product-package)
(define make-sum (get 'make-sum '+)) ; WARNING! must after (install-sum-package), or the procedure is '()
(define make-product (get 'make-product '*))

; c)

; d)
; (put '+ 'deriv deriv-sum)
; (put '* 'deriv deriv-mul)


(deriv '(+ x y) 'x)
(deriv '(* x y x) 'x)
(deriv '(+ x x x (* y x)) 'x)
(deriv '(* x y (+ x 3)) 'x)

; 2.75
(newline)
(define (make-from-real-imag-msg x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang-msg r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define matest (make-from-mag-ang-msg 6 1))
(matest 'real-part)
(matest 'imag-part)
(matest 'magnitude)
(matest 'angle)


; 2.76
; 显式分派: 这种策略在增加新操作时需要使用者避免命名冲突,而且每当增加新类型时,所有通用操作都需要做相应的改动,这种策略不具有可加性,因此无论是增加新操作还是增加新类型,这种策略都不适合。
; 数据导向: 数据导向可以很方便地通过包机制增加新类型和新的通用操作,因此无论是增加新类型还是增加新操作,这种策略都很适合。
; 消息传递: 消息传递将数据对象和数据对象所需的操作整合在一起,因此它可以很方便地增加新类型,但是这种策略不适合增加新操作,因为每次为某个数据对象增加新操作之后,这个数据对象已有的实例全部都要重新实例化才能使用新操作。
