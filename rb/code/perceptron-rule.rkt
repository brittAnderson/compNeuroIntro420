#lang racket

(require math/matrix
         math/array
         plot)

(define my-data (matrix [[ 0.3 0.7 1.0]
                         [-0.6 0.3 -1.0]
                         [0.7 0.3 1.0]
                         [-0.2 -0.8 -1.0]]))

(define (get-new-wts inp ws input-class #:threshold [threshold 0])
  (let* ([est-class (if (>= (matrix-ref
                            (matrix* inp ws) 0 0)
                           threshold)
                        1 -1)]
         [beta (* est-class input-class)])
    (matrix+ (matrix-scale (matrix-transpose inp) (* beta est-class ))
             ws)))
  
(define (wt-update ins wts)
  (let* ([num-cols (matrix-num-cols ins)]
         [inp (submatrix ins (list 0) (in-range (- num-cols 1)))]
         [input-class (matrix-ref ins 0 (- num-cols 1))])
    (get-new-wts inp wts input-class)))

(define (one-loop-through-data data starting-wts)
  (let ([first-row (submatrix data (list 0) (in-range (matrix-num-cols data)))])
    (for*/fold
     ([in-data first-row]
     [upd-wts (list starting-wts)]
    #:result (reverse (cons (wt-update
                             (submatrix data (list (- (matrix-num-rows data) 1))
                                        (in-range (matrix-num-cols data)))
                             (first upd-wts)) upd-wts)))
      ([r (in-range 1 (matrix-num-rows data))])
      (values (submatrix data (list r) (in-range (matrix-num-cols data)))
              (cons (wt-update in-data (first upd-wts)) upd-wts)))))

(define (parse-points lrnd)
  (for/list ([p lrnd])
    (list (list 0 0) (matrix->list p))))

(define (wt-plot lrnd)
  (plot (for/list ([pp (parse-points lrnd)]
                    [i (in-range 100)])
          (arrows pp #:color i
                  #:width (* i 1.0)))))
                
