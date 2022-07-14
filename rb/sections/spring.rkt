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
(define +p+ 2.0)
(define +delta-t+ 0.05)


(define (release-spring #:max-time [max-time 15] #:max-iter [max-iter 3000])
  (for/fold ([a (a-of-t +p+ +init-s+)]
             [v +init-v+]
             [s +init-s+]
             [time 0]
             [acc '()]
             #:result (reverse acc))
            ([n (in-range max-iter)])
    #:break (> time max-time)
    (let* ([acc (cons (list a v s time) acc)]
           [a (a-of-t +p+ s)]
           [v (v-of-t +delta-t+ a v)]
           [s (s-of-t +delta-t+ v s)]
           [time (+ time +delta-t+)])
      (values a v s time acc))))

;; (define (release-spring [max-time 30.0])
;;   (let ([al (list)]
;;         [vl (list)]
;;         [tl (list)]
;;         [sl (list)])
;;     (do ([time 0 (+ time +delta-t+)]
;;          [v +init-v+ (v-of-t +delta-t+ a v)]
;;          [s +init-s+ (s-of-t +delta-t+ v s)]
;;          [a (a-of-t +p+ +init-s+) (a-of-t +p+ s)])
;;       ((> time max-time) (list (reverse al) (reverse vl) (reverse tl) (reverse sl)))
;;       (begin (set! al (cons a al))
;;              (set! vl (cons v vl))
;;              (set! sl (cons s sl))
;;              (set! tl (cons time tl))))))

;;would for*/fold fix
;; (define (release-spring [max-repeats 5])
;;   (for/fold ([a (list (a-of-t +p+ +init-s+))]
;;              [v (list +init-v+)]
;;              [s (list +init-s+)]
;;              [t (list 0)])
;;             ([repeat (in-range max-repeats)])
;;     (let ([vc (first v)]
;;           [sc (first s)]
;;           [ac (first a)]
;;           [tc (first t)])
;;       (values (cons (a-of-t +p+ sc) a)
;;                      (cons (v-of-t +delta-t+ ac vc) v)
;;                      (cons (s-of-t +delta-t+ vc sc) s)
;;                      (cons (+ +delta-t+ tc) t)))))
