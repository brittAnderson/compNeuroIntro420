#lang racket


;;use racket's structure for creating a simple data
;;structure to hold the basics of a simple TM
(struct turing-machine (state tape head-location) #:transparent #:mutable)

;;defining the simple movements our machine can make
(define (move-left temp-tm )
   (let ([loc (turing-machine-head-location temp-tm)]
         [lst (turing-machine-tape temp-tm)])
      (if (= loc 0)
	  (set-turing-machine-tape! temp-tm (cons 0 lst))
	  (set-turing-machine-head-location! temp-tm (- loc 1)))))

(define (move-right temp-tm )
   (let ([loc (turing-machine-head-location temp-tm)]
         [lst (turing-machine-tape temp-tm)])
     (when (= (+ loc 1) (length lst))
       (set-turing-machine-tape! temp-tm (append lst (list 0))))
     (set-turing-machine-head-location! temp-tm (+ loc 1))))

;;this is not a very functional style of code. It would be nice to try and do
;;this without mutating the data structure, but here we are sort of
;;emulating writing and erasing to a single tape

(define (move tm dir)
  (cond
    [(equal? dir 'left) (move-left tm)]
    [(equal? dir 'right) (move-right tm)]
    [else (error "illegal direction")]))

;;;testing the state and value of a turing machine
(define (tm-equal-state-value tm state value)
  (and (equal? (turing-machine-state tm) state)
       (= (list-ref (turing-machine-tape tm) (turing-machine-head-location tm)) value)))

(define (upd-tape-location tm value)
  (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) value)))

(define (rule tm) ;;state value
    (cond
      ((tm-equal-state-value tm 'a 0)
       (set-turing-machine-state! tm 'b)
       (upd-tape-location tm 1)
       (move tm 'right))
      ((tm-equal-state-value tm 'a 1)
       (set-turing-machine-state! tm 'b)
       (upd-tape-location tm 1)
       (move tm 'left))
      ((tm-equal-state-value tm 'b 0)
       (set-turing-machine-state! tm 'a)
       (upd-tape-location tm 1)
       (move tm 'left))
      ((tm-equal-state-value tm 'b 1)
       (set-turing-machine-state! tm 'h)
       (upd-tape-location tm 1)
       (move tm 'right))))

(define initial-turing-machine (turing-machine 'a (list 0) 0))

(define (pretty-print-tm tm)
  (display (format "state:~a, tape: ~a, head: ~a\n" (turing-machine-state tm) (turing-machine-tape tm) (turing-machine-head-location tm))))
 
(define (busy-beaver-2-do tm)
  (pretty-print-tm tm)
  (do ([i 0 (+ i 1)])
    ((equal? (turing-machine-state tm) 'h)
     (display (format "Loops equaled ~a\n" i)))
    (rule tm)
    (pretty-print-tm tm))
  tm)

;;how to make a copy so you don't keep overwriting
;;the initial one:
;;(define working-tm (struct-copy turing-machine initial-turing-machine))