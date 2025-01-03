#lang racket
(require rnrs/arithmetic/fixnums-6) ; fxxor
;(require racket/trace)

; 3.1
(display "=====> begin test section 3.1\n")
(define (make-accumulator sum)
  (define (add num)
    (set! sum (+ sum num))
    sum)

  add)

(define A1 (make-accumulator 5))
(define A2 (make-accumulator 5))

(A1 10)
(A1 10)
(A2 13)

; 3.2
(display "=====> begin test section 3.2\n")
(define (make-monitored f)
  (define times 0)
  (define (wrapper-f arg)
    (set! times (+ times 1))
    (f arg))
  (define (how-many-calls?) times)
  (define (reset-count) (set! times 0))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (how-many-calls?))
          ((eq? m 'reset-count) (reset-count))
          (else (wrapper-f m))))

  dispatch)

(define s (make-monitored sqrt))
(s 100)
(s 9)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 121)
(s 81)
(s 'how-many-calls?)

; 3.3, 3.4
(display "=====> begin test section 3.3 - 3.4\n")
(define (make-account balance password)
  (define failtimes (make-monitored (make-accumulator 0)))

  (define (call-the-cops) (display "calling the cops"))

  (define (verify pass)
    (cond ((not (eq? password pass))
           (failtimes 1)
           #f)
          (else (failtimes 'reset-count) #t)))

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (errorhandle m)
    (if (> (failtimes 'how-many-calls?) 3)
        (call-the-cops)
        "Incorrect password"))

  (define (dispatch pass m)
    (if (verify pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) (lambda () balance))
              (else (error "Unknow request --MAKE-ACCOUNT" m)))
        errorhandle))

  dispatch)

;(trace make-account)
(define acc (make-account 100 'hahaha))
((acc 'hahaha 'withdraw) 50)
((acc 'hahaha 'withdraw) 60)
((acc 'hahaha 'deposit) 40)
((acc 'hahaha 'withdraw) 60)
((acc 'hahaha 'balance))
((acc 'aaa 'withdraw) 60)
((acc 'aaa 'withdraw) 60)
((acc 'aaa 'withdraw) 60)
((acc 'aaa 'withdraw) 60)

; 3.5
(newline)
(display "=====> begin test section 3.5\n")
(define (square x) (* x x))

;; https://stackoverflow.com/questions/1537921/simple-pseudo-random-algorithm/23875298
(define random-init 1)

(define random-max #x7fffffff)

(define (rand-update x0)
  (let* ((x1 (fxxor x0 (fxarithmetic-shift-right x0 13)))
         (x2 (fxxor x1 (fxarithmetic-shift-left x1 18))))
    (fxand x2 random-max)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;;; fix me! random@racket only support integer
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (let ((test (lambda () (P (random-in-range x1 x2)
                            (random-in-range y1 y2)))))
    (* (monte-carlo trials test)
       (- x2 x1)
       (- y2 y1))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (let ((pred (lambda (x y)
                (<= (+ (square x) (square y)) 1))))
    (estimate-integral pred -1 -1 1 1 trials)))

(estimate-pi 1000000)

; 3.6
(display "=====> begin test section 3.6\n")
; 注意闭包写法 (define {closure-name} (lambda (m) (cond ...)))
(newline)
(define rand-opt
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)

    (define (reset nx)
      (set! x nx)
      x)

    (lambda (m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "not support" m))))))

(rand-opt 'generate)
(rand-opt 'generate)
(rand-opt 'generate)
((rand-opt 'reset) 1)
(rand-opt 'generate)
(rand-opt 'generate)
(rand-opt 'generate)

; 3.7
(display "=====> begin test section 3.7\n")
(define (make-joint user raw-password joint-password)
  (define (verify pass)
    (if (eq? joint-password pass)
        #t
        #f))

  (define (errorhandle m)
    (lambda (x)
      "Incorrect password"))

  (define (dispatch pass m)
    (if (verify pass)
        (user raw-password m)
        (errorhandle m)))

  dispatch)

(define peter-acc (make-account 100 'peter))
(define paul-acc (make-joint peter-acc 'peter 'paul))
((peter-acc 'peter 'balance)) ; 100
((paul-acc 'peter 'withdraw) 20)
((paul-acc 'paul 'withdraw) 20)
((peter-acc 'peter 'balance)) ; 80
((peter-acc 'peter 'withdraw) 13)
((paul-acc 'paul 'balance))


; 3.8
(display "=====> begin test section 3.8\n")
(define f
  (let ((l '()))
    (lambda (x)
      (if (null? l)
          (begin
            (set! l (cons x l))
            (car l))
          (car l)))))

;(f 1)
;(f 0)
(f 0)
(f 1)

