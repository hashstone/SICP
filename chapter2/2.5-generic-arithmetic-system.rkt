#lang racket
(require racket/trace)
(include "put-get.rkt")

; utils
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((integer? datum) 'scheme-number) ; tricky, in scheme, (integer? 2.0) => #t, can't distingush its type
        ((real? datum) 'scheme-real)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (square x) (* x x))

; 2.83 - 2.84
(define (install-type-level)
  ; type level
  (put 'scheme-number 'typelevel 0)
  (put 'rational 'typelevel 1)
  (put 'scheme-real 'typelevel 2)
  (put 'complex 'typelevel 3)

  'typelevel-done
)
(install-type-level)

(define (compare t1 t2)
  (- (get t1 'typelevel) (get t2 'typelevel)))

; version 1
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if (not (null? proc))
;          (apply proc (map contents args))
;          (error "No method for these type -- APPLY-GENERIC" (list op type-tags)))
;    )
;  )
;)

; version 2
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if (not (null? proc))
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (equal? type1 type2)
;                    (error "No method for these types" (list op type-tags))
;                    (let ((t1->t2 (get-coercion type1 type2))
;                          (t2->t1 (get-coercion type2 type1)))
;                      (cond ((not (null? t1->t2)) (apply-generic op (t1->t2 a1) a2))
;                            ((not (null? t2->t1)) (apply-generic op a1 (t2->t1 a2)))
;                            (else (error "No method for these types" (list op type-tags)))))))
;              (error "No method for these types" (list op type-tags)))))))
; 2.83-2.85
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((flag (compare type1 type2)))
                      (if (> flag 0)
                          (apply-generic op a1 (raise a2))
                          (apply-generic op (raise a1) a2)))))
              (error "No method for these types" (list op type-tags)))))))

; scheme-number stand for integer
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'drop '(scheme-number) (lambda (x) (tag x)))
  (put 'can-drop? '(scheme-number) (lambda (x) (#f)))
  'done)

; scheme-real stand for float
(define (install-scheme-real-package)
  (define (tag x)
    (attach-tag 'scheme-real x))
  (put 'add '(scheme-real scheme-real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-real scheme-real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-real scheme-real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-real scheme-real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-real
       (lambda (x) (tag x)))
  (put 'equ '(scheme-real scheme-real)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-real)
       (lambda (x) (= x 0.0)))
  (put 'exp '(scheme-real scheme-real)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  (put 'raise '(scheme-real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

; rational-number
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom y))))
  (define (=zero? x) (= (numer x) 0))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ '(rational rational)
       (lambda (x y) (equ-rat x y)))
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational)
       (lambda (x) (make-scheme-real (/ (* 1.0 (numer x)) (denom x)))))
  (put 'project '(rational) (lambda (x) (make-scheme-number (numer x))))
  'done)

; complex number
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

(install-rectangular-package)
(install-polar-package)
;(get 'make-from-real-imag '(rectangular))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z) (= (magnitude z) 0))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex)
       (lambda (z1 z2) (equ-complex z1 z2)))
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) (lambda (z) (make-scheme-real (real-part z))))

  ; 注意 '(complex), 而不是 'complex，因为 apply-generic::(map type-tag args) 返回的是list
  (put 'imag-part '(complex) imag-part)
  (put 'real-part '(complex) real-part)
  (put 'angle     '(complex) angle)
  (put 'magnitude '(complex) magnitude)
  'done)


; install packages
(install-scheme-number-package)
(install-rational-package)
(install-scheme-real-package)
(install-complex-package)
(install-type-level)

; package export API
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-scheme-real f)
  ((get 'make 'scheme-real) f))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; generic calc system
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y)) ; 2.81 a)

(define (raise x)
  (let ((raise-proc (get 'raise (list (type-tag x)))))
    (if raise-proc
        (raise-proc (contents x))
        #f)))

(define (project x)
  (let ((proc (get 'project (list (type-tag x)))))
    (if proc
        (proc (contents x))
        #f)))

(define (equ?-print x y)
  (display x) (display " = ") (display y) (display ":") (display (equ? x y))
  (apply-generic 'equ x y)
  (newline)
)

(define (equ? x y)
  (if (eq? (type-tag x) (type-tag y))
      (apply-generic 'equ x y)
      #f))

(define (drop x)
  (if (pair? x) ; 过滤 #t、#f 等没有 type-tag 的参数
      (let ((x-project (project x)))
        (if (and x-project
                 (equ? (raise x-project) x))
            (drop x-project)
            x))
      x))

; test cases
;(trace apply-generic)
(add (make-scheme-number 3) (make-scheme-number 4))
(sub (make-scheme-number 3) (make-scheme-number 4))
(mul (make-scheme-number 3) (make-scheme-number 4))
(div (make-scheme-number 3) (make-scheme-number 4))

(add (make-rational 4 9) (make-rational 2 3))
(sub (make-rational 4 9) (make-rational 2 3))
(mul (make-rational 4 9) (make-rational 2 3))
(div (make-rational 4 9) (make-rational 2 3))

(add (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 3 4))
(sub (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 3 4))
(mul (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 2 2))
(div (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 3 4))

; 2.77
; call complex::magnitude firstly, then call rectangular::magnitude
; so called apply-generic twice.
(newline)
(make-complex-from-real-imag 3 4)
(magnitude (make-complex-from-real-imag 3 4))

; 2.78
(display "\n====> test 2.78\n")
(add 4 8)
(add (make-scheme-number 11) (make-scheme-number 9))

; 2.79
(display "\n====> test 2.79\n")
(equ?-print 5 (make-scheme-number 5))
(equ?-print (make-scheme-number 3) (make-scheme-number 4))
(equ?-print (make-scheme-number 3) (make-scheme-number 3))
(equ?-print (make-rational 3 4) (make-rational 9 12))
(equ?-print (make-rational 2 -2) (make-rational 3 1))
(equ?-print (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 2 2))
(equ?-print (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))
(equ?-print (make-complex-from-mag-ang 3 2) (make-complex-from-mag-ang 3 1))
(equ?-print (make-complex-from-mag-ang 3 2) (make-complex-from-mag-ang 3 2))
;(equ?-print (make-rational 2 3) (make-complex-from-mag-ang 2 3))

; 2.80
(display "\n====> test 2.80\n")
(define (=zero? x) (apply-generic '=zero? x))
(=zero? 0)
(=zero? 9)
(=zero? (make-scheme-number 0))
(=zero? (make-scheme-number 3))
(newline)
(=zero? (make-rational 0 1))
(=zero? (make-rational 0 111))
(=zero? (make-rational 1 111))
(newline)
(=zero? (make-complex-from-mag-ang 0 1))
(=zero? (make-complex-from-mag-ang 0 20))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-real-imag 3 0))
(=zero? (make-complex-from-real-imag 0 3))


;; type-transform, type-[force, raise(promotion), demotion]
;; type force
(newline)

(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *coercion-table* (make-hash))

(define (put-coercion op type proc)
  (hash-set! *coercion-table* (list op type) proc)
)

(define (get-coercion op type)
  (hash-ref *coercion-table* (list op type) '())
)

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex x) x)

(put-coercion 'scheme-number 'complex scheme-number->complex)
;(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)

;(trace apply-generic)
(exp 5 3)
;(exp (make-complex-from-real-imag 3 1) (make-complex-from-real-imag 5 2))

; 2.82
; 寻找公共父节点

; 2.83 - 2.84
(newline)
(display "\n====> test 2.83 - 2.85\n")
(add 3 (make-rational 2 1))
(sub (make-rational 3 2) 5)
(add (make-rational 3 9) (make-scheme-real 1.5))
(div (make-scheme-real 3.0) 2)

;(trace apply-generic)
;(trace drop)
;(trace raise)
;(trace project)
; TODO fix (add 2 (make-scheme-real 1.33)) problem
(add 1.9 (make-scheme-real 1.3333))
(add 1.2 (make-complex-from-real-imag 3 5))

; 2.85
(newline)
(add (make-complex-from-real-imag 2 -5) (make-complex-from-real-imag 3 5))
(add (make-complex-from-real-imag 2 -5.5) (make-complex-from-real-imag 3.2 5.5))

(div (make-rational 4 5) (make-rational 2 5))
