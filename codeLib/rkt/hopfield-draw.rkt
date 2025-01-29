#lang racket

(require racket/draw
         racket/draw/arrow
         racket/snip)

(provide hopfield-net)



(define (hopfield-net)
  (let* ([hf (make-bitmap 400 400)]
        [hf-dc (new bitmap-dc% [bitmap hf])])
    (send hf-dc set-pen "black" 5 'solid)
    (send hf-dc set-brush "green" 'transparent)
    (send hf-dc draw-ellipse 340 30 40 40)
    (send hf-dc draw-ellipse 20 30 40 40)
    (send hf-dc draw-ellipse 20 340 40 40)
    (send hf-dc draw-ellipse 340 340 40 40)
    (send hf-dc set-font (make-font #:size 24 #:family 'roman
                                 #:weight 'bold))
    (send hf-dc draw-text "X" 15 20)
    ;;connections UL
    (draw-arrow hf-dc 60 60 60 315 0 0 #:arrow-head-size 12)
    (draw-arrow hf-dc 60 60 315 60 0 0 #:arrow-head-size 12)
    (draw-arrow hf-dc 60 60 315 315 0 0 #:arrow-head-size 12)
    ;;connections back
    (draw-arrow hf-dc 315 50 80 50 0 0 #:arrow-head-size 12)
    (draw-arrow hf-dc 315 295 110 70 0 0 #:arrow-head-size 12)
    (draw-arrow hf-dc 80 300 80 80 0 0 #:arrow-head-size 12)
    (send hf-dc set-brush "green" 'solid)
    (send hf-dc draw-ellipse 320 40 40 40)
    (send hf-dc draw-ellipse 40 40 40 40)
    (send hf-dc draw-ellipse 40 320 40 40)
    (send hf-dc draw-ellipse 320 320 40 40)
    (send hf-dc set-pen "black" 5 'solid)
    (make-object image-snip% hf)))
