(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "eazy-gnuplot" :silent t))

(defpackage #:mytest
  (:use #:common-lisp #:eazy-gnuplot))

(in-package #:mytest)

(defparameter dt 0.05)
(defparameter max-t 10)
(defparameter init-t 0.0d0)
(defparameter start-time 1.0d0)
(defparameter stop-time 6.0d0)
(defparameter cap 1)
(defparameter res 2)
(defparameter threshold 3.0d0)
(defparameter spike-display 8.0d0)
(defparameter init-v 0.0d0)
(defparameter voltage init-v)
(defparameter injection-current 4.3d0)
(defparameter injection-time (cons start-time stop-time))
(defparameter tau (* res cap))

(defun update (old-value rate-of-change time-step)
  (+ (* rate-of-change time-step) old-value))

(defun dv-dt (localres locali localv)
  (* (/ 1 tau) (- (* localres locali) localv)))

(defun between (x &key (lower (car injection-time))
		    (upper (cdr injection-time))
		    (if-true injection-current)
		    (if-false 0.0d0))
  (if (and (>= x lower) (<= x upper)) if-true if-false))

(defun voltage-choice (curr-volt spike-status &key (thr threshold) (sd spike-display))
  (cond
    ((and (> curr-volt thr) (not spike-status)) sd)
    (spike-status 0.0d0)
    (t curr-volt)))

(defun run-iandf-sim (&key (tolerance 0.1d0))
  (do* ((ts)
	(is)
	(vs)
	(tme init-t (+ dt tme))
	(inj-cur 0.0d0 (between tme))
	(spike nil (< (abs (- voltage-now spike-display)) tolerance))
	(voltage-now init-v
		     (voltage-choice (update voltage-now (dv-dt res inj-cur voltage-now) dt) spike)))
       ((> tme max-t) (list (nreverse ts) (nreverse  is) (nreverse vs)))
    (push tme ts)
    (push inj-cur is)
    (push voltage-now vs)))
  


(defun iandf-plot (output plot-data)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png)
    (plot
     (lambda ()
       (loop for times in (first plot-data)
	     for volts in (third plot-data)
	     do (format t "~&~a ~a" times volts)))
     :with '(:lines)))
  output)





  ;; #+begin_src lisp :exports both :results graphics file "iandf.png"
  ;;   (iandf-plot "iandf.png")
  ;; #+end_src

  ;; #+RESULTS:
  ;; a[[file:iandf.png]]
  ;;
