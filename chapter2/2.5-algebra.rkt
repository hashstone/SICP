(include "put-get.rkt")

; utils
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((integer? datum) 'scheme-real) ; tricky, in scheme, (integer? 2.0) => #t, can't distingush its type
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
  ;(put 'polynomial 'typelevel 4)
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
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
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
  'integer-done)

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
  (put 'neg '(scheme-real)
       (lambda (x) (- x)))
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
  'real-done)

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
  (define (neg-rat x)
    (make-rat (- (numer x)) (denom x)))
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
  (put 'neg '(rational)
       (lambda (x) (tag (neg-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ '(rational rational)
       (lambda (x y) (equ-rat x y)))
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational)
       (lambda (x) (make-scheme-real (/ (* 1.0 (numer x)) (denom x)))))
  (put 'project '(rational) (lambda (x) (make-scheme-number (floor (/ (numer x) (denom x))))))
  'rational-done)

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
  'rectangular-done)

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
  'polar-done)

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
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z)) (- (imag-part z))))
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
  (put 'neg '(complex)
       (lambda (z) (tag (neg-complex z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex)
       (lambda (z1 z2) (equ-complex z1 z2)))
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) (lambda (z) (make-scheme-real (real-part z))))

  ; 注意 '(complex), 而不是 'complex,因为 apply-generic::(map type-tag args) 返回的是list
  (put 'imag-part '(complex) imag-part)
  (put 'real-part '(complex) real-part)
  (put 'angle     '(complex) angle)
  (put 'magnitude '(complex) magnitude)
  'complex-done)


; polynomial-package
; term layer is responsible for expressing degrees and coefficients of polynomials
;    * Dense  polynomial, (1 2 0 3 -2 -5) stands for (x^5 + 2*x^4 + 3*x^2 - 2*x - 5)
;    * Sparse polynomial, ((100 1) (2 2) (0 1)) stands for (x^100 + 2*x^2 + 1)
; term-ops:
;    * neg-terms
;    * add-terms
;    * mul-terms
;    * mul-term-by-all-terms
;
; poly layer is responsible for expressing polynomials by compose of terms-operation
;    * add-poly
;    * mul-poly
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; variable
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; terms
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; representation of terms and term lists
  (define (neg-terms l)
    (if (empty-termlist? l)
        l
        (let ((term (first-term l)))
          (adjoin-term
           (make-term (order term) (neg (coeff term)))
           (neg-terms (rest-terms l))))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1) (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))

  ; 2.87
  (define (zero-polynomial? p)
    (define (iter term-list)
      (cond ((null? term-list) #t)
            ((eq? (type-tag (first-term term-list)) 'polynomial)
             (and (zero-polynomial? (first-term term-list)) (iter (rest-terms term-list))))
            ((not (=zero? (coeff (first-term term-list)))) #f)
            (else (iter (rest-terms term-list)))))
    (iter (term-list p)))

  (define (zero-order-coeff p)
    (define (iter term-list)
      (cond ((null? term-list) 0)
            ((= (order (first-term term-list)) 0) (coeff (first-term term-list)))
            (else (iter (rest-terms term-list)))))
    (iter (term-list p)))

  ; 2.91
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms
                                       (add-terms L1 (neg-terms (mul-terms L2 (list (make-term new-o new-c)))))
                                       L2)))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (neg-poly p2))))) ; 2.88
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'make-term 'polynomial
       (lambda (order coeff) (make-term order coeff)))
  (put '=zero? '(polynomial) zero-polynomial?)
  ;(trace add-terms)
  ;(trace neg-terms)
  ;(trace mul-terms)
  ;(trace zero-polynomial?)
  ;(trace div-terms)
  'polynomial-done)

; generic calc system
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y)) ; 2.81 a)
(define (neg x)   (apply-generic 'neg x))
(define (=zero? x) (apply-generic '=zero? x))

(define (raise x)
  (let ((raise-proc (get 'raise (list (type-tag x)))))
    (if (not (null? raise-proc))
        (raise-proc (contents x))
        #f)))

(define (project x)
  (let ((proc (get 'project (list (type-tag x)))))
    (if (not (null? proc))
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

(define (make-polynomial var term-list)
  ((get 'make 'polynomial) var term-list))
(define (make-term order coeff)
  ((get 'make-term 'polynomial) order coeff))

; install packages
(install-scheme-number-package)
(install-rational-package)
(install-scheme-real-package)
(install-complex-package)
(install-polynomial-package)
(install-type-level)
