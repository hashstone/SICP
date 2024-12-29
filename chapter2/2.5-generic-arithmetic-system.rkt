#lang racket
(require racket/trace)
(include "2.5-algebra.rkt")

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

; 2.83 - 2.85
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

; 2.87
(newline)
(display "\n====> test polynomial ... \n")
; 3x^2 + (2+3i)x + 7
(define normal-p1 (make-polynomial 'x (list (make-term 2 3) (make-term 1 (make-complex-from-real-imag 2 3)) (make-term 0 7))))
; x^4 + (2/3)x^2 + (5+3i)
(define normal-p2 (make-polynomial 'x (list (make-term 4 1) (make-term 2 (make-rational 2 3)) (make-term 0 (make-complex-from-real-imag 5 3)))))

(define zero-test-p1 (make-polynomial 'x (list (make-term 4 (make-rational 0 4)) (make-term 3 0) (make-term 2 (make-complex-from-real-imag 0 3)))))
(define zero-test-p2 (make-polynomial 'x (list (make-term 4 (make-rational 0 4)) (make-term 3 0) (make-term 2 (make-complex-from-mag-ang 0 60)))))
(define zero-test-p3 (make-polynomial 'y (list (make-term 4 0) (make-term 3 zero-test-p2))))
(define zero-test-p4 (make-polynomial 'y (list (make-term 4 0) (make-term 3 zero-test-p1))))

(display normal-p1) (newline)
(display normal-p2) (newline)

(=zero? normal-p1)
(=zero? normal-p2)
(=zero? zero-test-p1)
(=zero? zero-test-p2)
(=zero? zero-test-p3)
(=zero? zero-test-p4)

; 2.88
;(trace add)
;(trace drop)
(newline)
(add normal-p1 normal-p2)
(sub normal-p1 normal-p2)
(sub normal-p2 normal-p1)
(sub normal-p1 normal-p1)

; 2.91
;(trace div)
(newline)
(define div-p1 (make-polynomial 'x (list (make-term 5 1) (make-term 3 2) (make-term 2 -4) (make-term 0 -1))))
(define div-p2 (make-polynomial 'x (list (make-term 2 1) (make-term 0 -1))))
(div div-p1 div-p2)
(div div-p2 div-p1)