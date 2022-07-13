#lang racket
(provide release-spring)

(define (s-of-t delta-t v s)
  (+ s (* v delta-t)))
(define (v-of-t delta-t a v)
  (+ v (* a delta-t)))
(define (a-of-t p s)
  (* -1 p s))

(define +init-v+ 0.0)
(define +init-s+ 10.0)
(define +p+ 4.0)
(define +delta-t+ 0.05)

(define (release-spring [max-time 30.0])
  (let ([al (list)]
        [vl (list)]
        [tl (list)]
        [sl (list)])
    (do ([time 0 (+ time +delta-t+)]
         [v +init-v+ (v-of-t +delta-t+ a v)]
         [s +init-s+ (s-of-t +delta-t+ v s)]
         [a (a-of-t +p+ +init-s+) (a-of-t +p+ s)])
      ((> time max-time) (list (reverse al) (reverse vl) (reverse tl) (reverse sl)))
      (begin (set! al (cons a al))
             (set! vl (cons v vl))
             (set! sl (cons s sl))
             (set! tl (cons time tl))))))
