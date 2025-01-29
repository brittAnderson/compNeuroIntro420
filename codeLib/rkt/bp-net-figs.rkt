#lang racket

(require racket/draw
         racket/snip)

(provide linear-net-fig)



(define (linear-net-fig)
  (let* ([af (make-bitmap 400 400)]
        [af-dc (new bitmap-dc% [bitmap af])])
    (send af-dc set-brush "green" 'solid)
    (send af-dc draw-ellipse 40 200 40 40)
    (send af-dc draw-ellipse 140 200 40 40)
    (send af-dc draw-ellipse 240 200 40 40)
    (send af-dc draw-ellipse 340 200 40 40)
    (send af-dc set-pen "black" 5 'solid)
    (send af-dc draw-line 80  220 140 220)
    (send af-dc draw-line 180 220 240 220)
    (send af-dc draw-line 280 220 340 220)
    (send af-dc draw-text "W1" 130 170)
    (send af-dc draw-text "W2" 230 170)
    (send af-dc draw-text "W3" 330 170)
    (make-object image-snip% af)))

