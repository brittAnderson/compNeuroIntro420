#lang racket

(require racket/draw
         racket/gui)

(provide (all-defined-out))


(define (and-fig)
  (let* ([af (make-bitmap 400 400)]
        [af-dc (new bitmap-dc% [bitmap af])])
    (send af-dc set-brush "green" 'solid)
    (send af-dc draw-ellipse 320 40 40 40)
    (send af-dc set-brush "red" 'solid)
    (send af-dc draw-ellipse 40 40 40 40)
    (send af-dc draw-ellipse 40 320 40 40)
    (send af-dc draw-ellipse 320 320 40 40)
    (send af-dc set-pen "black" 5 'solid)
    (send af-dc draw-line 250 0 400 250)
    (make-object image-snip% af)))

(define (or-fig)
  (let* ([af (make-bitmap 400 400)]
        [af-dc (new bitmap-dc% [bitmap af])])
    (send af-dc set-brush "green" 'solid)
    (send af-dc draw-ellipse 320 40 40 40)
    (send af-dc draw-ellipse 40 40 40 40)
    (send af-dc draw-ellipse 320 320 40 40)
    (send af-dc set-brush "red" 'solid)
    (send af-dc draw-ellipse 40 320 40 40)
    (send af-dc set-pen "black" 5 'solid)
    (send af-dc draw-line 0 250 300 400)
    (make-object image-snip% af)))

(define (xor-fig)
  (let* ([af (make-bitmap 400 400)]
        [af-dc (new bitmap-dc% [bitmap af])])
    (send af-dc set-brush "red" 'solid)
    (send af-dc draw-ellipse 40 320 40 40)
    (send af-dc draw-ellipse 320 40 40 40)
    (send af-dc set-brush "green" 'solid)
    (send af-dc draw-ellipse 40 40 40 40)
    (send af-dc draw-ellipse 320 320 40 40)
    (make-object image-snip% af)))
