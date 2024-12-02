#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(include "graph-lang-base.rkt")

; for draw-line
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define v1 (make-vect 5 7))
(define v2 (make-vect 3 5))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 2 v1)
(scale-vect 2 v2)

; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (corner-split einstein 1))

(paint (corner-split einstein 4))

(newline)
(paint (flip-vert einstein))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


;(define (flipped-pairs painter)
;  (let ((pattern2 (beside painter (flip-vert painter))))
;    (below pattern2 pattern2)))
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))
(paint (flipped-pairs einstein))

;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) quarter)))
;      (below (flip-vert half) half))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
(paint (square-limit einstein 2))

; 2.45
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))
(define (split old new)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split old new) painter (- n 1))))
          (old painter (new smaller smaller))))))

(newline)
(define r-s (split beside below))
(define u-s (split below beside))
(paint (r-s einstein 3))
(paint (u-s einstein 3))


; 2.46
(newline)


; 2.47
(newline)
(define f1 (make-frame (make-vect 0 0) (make-vect 20 100) (make-vect 100 20)))

(origin-frame f1)
(edge1-frame f1)
(edge2-frame f1)

(define draw (draw-viewport vp))
(define clear (clear-viewport vp))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (car(cdr v))))


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (line
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))
     segment-list)))


; 2.49
(define origin (make-vect 0 0))
(define top-left (make-vect 0 1))
(define top-right (make-vect 1 1))
(define bot-right (make-vect 1 0))

(define bounding-painter (segments->painter (list (make-segment origin top-left)
                                                  (make-segment top-left top-right)
                                                  (make-segment top-right bot-right)
                                                  (make-segment bot-right origin))))

(define x-painter (segments->painter (list (make-segment origin top-right)
                                           (make-segment top-left bot-right))))

(define wave-painter (segments->painter (list
                                         (make-segment (make-vect 0 0.65) (make-vect 0.14 0.39))
                                         (make-segment (make-vect 0.14 0.39) (make-vect 0.29 0.58))
                                         (make-segment (make-vect 0.29 0.58) (make-vect 0.34 0.49))
                                         (make-segment (make-vect 0.34 0.49) (make-vect 0.24 0))
                                         (make-segment (make-vect 0.4 0) (make-vect 0.49 0.28))
                                         (make-segment (make-vect 0.49 0.28) (make-vect 0.59 0))
                                         (make-segment (make-vect 0.71 0)  (make-vect 0.59 0.45))
                                         (make-segment (make-vect 0.59 0.45) (make-vect 0.99 0.15))
                                         (make-segment (make-vect 0.99 0.35) (make-vect 0.74 0.64))
                                         (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
                                         (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
                                         (make-segment (make-vect 0.59 0.64) (make-vect 0.64 0.85))
                                         (make-segment (make-vect 0.64 0.85) (make-vect 0.59 1))
                                         (make-segment (make-vect 0.39 1) (make-vect 0.34 0.85))
                                         (make-segment (make-vect 0.34 0.85) (make-vect 0.39 0.64))
                                         (make-segment (make-vect 0.39 0.64) (make-vect 0.29 0.64))
                                         (make-segment (make-vect 0.29 0.64) (make-vect 0.14 0.6))
                                         (make-segment  (make-vect 0.14 0.6) (make-vect 0 0.84)))))

(bounding-painter f1)
(x-painter f1)

; 2.50
(define f2 (make-frame (make-vect 200 300) (make-vect 0 100) (make-vect 100 0)))
((my-rotate180 wave-painter) f2)

(define f3 (make-frame (make-vect 0 200) (make-vect 0 100) (make-vect 100 0)))
((my-below-1 wave-painter wave-painter) f3)

(define f4 (make-frame (make-vect 200 0) (make-vect 0 100) (make-vect 100 0)))
((my-beside wave-painter wave-painter) f4)

