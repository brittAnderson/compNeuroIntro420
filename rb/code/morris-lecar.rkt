#lang racket

(require plot/pict)

(provide (all-defined-out))

;;practice and to avoid retyping
(define (list-to-alist l)
    (cond ((null? l) l)
          ( #t (cons (cons (first l) (second l)) (list-to-alist (list-tail l 2))))))

(define (assoc-key key l)
  (cdr (assoc key l)))

(define mlps (list 'c 20 'gca 4.4 'vca 120 'gk 8 'vk -84 'gl 2 'vl -60 'v1 -1.2 'v2 18 'v3 2 'v4 30 'phi 0.04 'tau-n 0.8 'curr 96.0 'time-step 0.05))

(define mlps-alist (list-to-alist mlps))

(define ( mss v v1 v2)
  (* 0.5 (+ 1 (tanh (/ (- v v1) v2)))))

(define ( nss v v3 v4)
  (* 0.5 (+ 1 (tanh (/ (- v v3) v4)))))

(define ( tau-n v v3 v4 phi)
  (/ 1 (* phi (cosh (/ (- v v3) (* 2 v4))))))

(define ( dndt v n v3 v4 phi)
  (/ (- (nss v v3 v4) n) (tau-n v v3 v4 phi)))

(define ( dvdt v n c curr gl gca gk vl vca vk v1 v2)
  (/ (- curr (+ (* gl (- v vl)) (* gca (mss v v1 v2) (- v vca)) (* gk n (- v vk)))) c))

(define (euler-update old-value rate-of-change time-step)
  (+ old-value (* rate-of-change time-step)))

(define (one-step v n time-step c curr gl gca gk vl vca vk v1 v2 v3 v4 phi)
  (list (euler-update v (dvdt v n c curr gl gca gk vl vca vk v1 v2) time-step) (euler-update n (dndt v n v3 v4 phi ) time-step)))

(define-syntax-rule (let-my-alist l body)
  (let ([mykeys (map car l)])
        `(let ,(for/list ([k mykeys])
          `(,k ,(assoc-key k l))) ,body)))

          
(define (test-loop-morris-lescar parameters-as-alist #:vinit [vinit -60] #:max-time [max-time 5.0] #:max-iter [max-iter 50000])
   (let-my-alist parameters-as-alist
                 `(for*/fold
                  ([t 0]
                   [v ,vinit]
                   [n (nss ,vinit v3 v4)]
                   [accum '()]
                   #:result (reverse accum))
                  ([num-iters (in-range ,max-iter)])
                   #:break (> t ,max-time)
                   (let ([os (one-step v n time-step c curr gl gca gk vl vca vk v1 v2 v3 v4 phi)])
                   (values (+ t time-step)
                           (first os)
                           (second os)
                           (cons (list t v n) accum))))))

(define (try-a-new-phi phi)
  (let ([outcome (eval (test-loop-morris-lescar (cons (cons 'phi phi) (remove (assoc 'phi mlps-alist) mlps-alist)) #:max-time 1000.0))])
    (plot-pict (lines (map vector (map first outcome) (map second outcome))))))

(define (poor-mans-phase-plot phi)
         (let ([outcome (eval (test-loop-morris-lescar (cons (cons 'phi phi) (remove (assoc 'phi mlps-alist) mlps-alist)) #:max-time 1000.0))])
           (plot-pict (lines (map vector (map second outcome) (map third outcome))))))
