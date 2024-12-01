#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

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

; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (corner-split einstein 1))
(paint (corner-split einstein 4))


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
(paint (r-s einstein 2))
(paint (u-s einstein 2))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
; 2.46
(newline)
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define v1 (make-vect 5 7))
(define v2 (make-vect 3 5))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 2 v1)
(scale-vect 2 v2)

; 2.47
(newline)
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cadr (cdr f)))

(define f1 (make-frame (make-vect 50 60) (make-vect 100 150) (make-vect 200 130)))

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

; 2.48
(newline)
(define (make-segment vect-start vect-end)
  (list vect-start vect-end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))

(define seg1 (make-segment (make-vect 1 2) (make-vect 3 1)))

(start-segment seg1)
(end-segment seg1)

; 2.49
; a. bounding painter
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


(bounding-painter f1)
(x-painter f1)