#lang racket

(require racket/format
         racket/draw
         racket/gui)

(provide (all-defined-out))

(define (pad-eight ns)
  (~a ns 
    #:align 'right
    #:width 8
    #:pad-string "0"))

(define (make-rule rule-no)
  (let ([white-or-black? (lambda (in-char) (if (equal? in-char #\0) 'w 'b))]
        [test-char (list->string (reverse (string->list (pad-eight (number->string rule-no 2)))))])
      (lambda (ns)
      (match ns
        ((list 'w 'w 'w) (white-or-black? (string-ref test-char 0)))
        ((list 'w 'w 'b) (white-or-black? (string-ref test-char 1)))
        ((list 'w 'b 'w) (white-or-black? (string-ref test-char 2)))
        ((list 'w 'b 'b) (white-or-black? (string-ref test-char 3)))
        ((list 'b 'w 'w) (white-or-black? (string-ref test-char 4)))
        ((list 'b 'w 'b) (white-or-black? (string-ref test-char 5)))
        ((list 'b 'b 'w) (white-or-black? (string-ref test-char 6)))
        ((list 'b 'b 'b) (white-or-black? (string-ref test-char 7)))))))

(define test-set (list (list 'w 'w 'w) (list 'w 'w 'b) (list 'w 'b 'w) (list 'w 'b
'b) (list 'b 'w 'w) (list 'b 'w 'b) (list 'b 'b 'w) (list 'b 'b
'b)))

(define (rule-tester r-no t-s)
  (let ([my-rule (make-rule r-no)])
    (for ([c t-s])
      (display (format "in ~a out ~a\n" c (my-rule c))))))
  
(define (top-row n)
  (let-values ([(how-many-start how-many-end)
                (if (even? n)
                    (values (- (/ n 2) 1) (/ n 2))
                    (values (/ (- n 1) 2)  (/ (- n 1) 2)))])
    (append (make-list how-many-start 'w) (list 'b)
            (make-list how-many-end 'w))))

(define (pad-front in-list)
  (cons (list-ref in-list (- (length in-list) 1))
        (take in-list 2)))

(define (pad-end in-list)
  (append (list-tail in-list (- (length in-list) 2))
          (list (first in-list))))

(define (crawl-cols which-rule in-list)
  (for*/fold ([test-group (pad-front in-list)]
              [rem-list in-list]
              [output '()]
              #:result (reverse output))
             ([i (in-range (length in-list))])
    (cond
      [(< (length rem-list) 3)
       (values rem-list rem-list (cons (which-rule (pad-end in-list)) output))]
      [else
       ;(display (format "~a\n" test-group))
       ;(display (format "~a\n" rem-list))
       (values (take rem-list 3)
               (drop rem-list 1) (cons (which-rule test-group) output))])))

(define (build-rows which-rule [ncols 200] [nrows 200])
    (for/fold
     ([out-dat (list (top-row ncols))]
      #:result (reverse out-dat))
     ([ i (in-range nrows)])  
      (cons (crawl-cols which-rule (list-ref out-dat 0)) out-dat)))

(define (draw-rule-automata rule-dat [scale 1] #:file-flag [file-flag #f] #:rule-num [n 999])
  (let* ([row-n (length rule-dat)]
         [col-n (length (first rule-dat))]
         [img-w (* col-n scale)]
         [img-h (* row-n scale)]
         [target (make-bitmap img-w img-h)]
         [dc (new bitmap-dc% [bitmap target])])
    (for ([r (in-range row-n)])
      (let ([rd (list-ref rule-dat r)])
        (for ([c (in-range col-n)])
          (let ([cd (list-ref rd c)])
            (when (equal? cd 'b)
              (send dc set-brush "red" 'solid)
              (send dc set-pen "white" 1 'transparent)
              (send dc draw-rectangle (* scale c) (* scale r)
                    scale scale))))))
    (if file-flag
        (send target save-file (format "rule-~a.png" n) 'png)
        (make-object image-snip% target))))

(define (d-r-a n #:num-rows [num-rows 200] #:num-cols [num-cols 100]
               #:scale [scale 5] #:file-flag [file-flag #f])
  (draw-rule-automata
   (build-rows
    (make-rule n) num-rows num-cols)
   scale #:file-flag file-flag #:rule-num n))
