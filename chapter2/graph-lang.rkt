#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

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