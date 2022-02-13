;;;; This is a short file to generate the plots for the and, or , and xor functions
;;;; When I wanted to replace the numpy matplotlib versions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "eazy-gnuplot" :silent t))

(defpackage #:linsep
  (:use #:common-lisp #:eazy-gnuplot))

(in-package #:linsep)


(defun and-plot (output &key (plot-data '()))
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png :xrange '(-0.5 1.5)
	      :yrange '(-0.5 1.5))
    (plot
     (lambda ()
       (format t "~&0 0")
       (format t "~&0 1")
       (format t "~&1 0")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "red"))
    (plot
     (lambda ()
       (format t "~&1 1")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "green")))
  output)


(defun or-plot (output &key (plot-data '()))
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png :xrange '(-0.5 1.5)
	      :yrange '(-0.5 1.5))
    (plot
     (lambda ()
       (format t "~&0 0")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "red"))
    (plot
     (lambda ()
       (format t "~&0 1")
       (format t "~&1 0")
       (format t "~&1 1")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "green")))
  output)

(defun xor-plot (output &key (plot-data '()))
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png :xrange '(-0.5 1.5)
	      :yrange '(-0.5 1.5))
    (plot
     (lambda ()
       (format t "~&0 0")
       (format t "~&1 1")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "red"))
    (plot
     (lambda ()
       (format t "~&0 1")
       (format t "~&1 0")
       )
     :using '(1 2)
     :with '(:circles :fill :solid :lc "green")))
  output)
