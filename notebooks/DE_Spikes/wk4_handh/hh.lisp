;; need to add injected current into my hh neuron
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "eazy-gnuplot" :silent t))

(defpackage #:hodgkin-huxley
  (:nicknames "HH") (:use #:cl)
  (:import-from "EAZY-GNUPLOT"
		"WITH-PLOTS"
		"PLOT"
		"GP-SETUP"))

(in-package :hh)

;;this next class should be pulled out of this package;;
;; should probably be a subclass of something else to distinguish the IandF from the HandH
(defclass neuron-sim ()
  ((dt
   :initarg :dt
   :initform 0.05d0)
   (max-t
    :initarg :max-t
    :initform 10d0)
   (init-t
    :initarg :init-t
    :initform 0.0d0)
   (start-time
    :initarg :start-time
    :initform 1.0d0)
   (stop-time
    :initarg :stop-time
    :initform 6.0d0)
   (cap
    :initarg :cap
    :initform 1.0d0)
   (res
    :initarg :res
    :initform 2.0d0)
   (threshold
    :initarg :threshold
    :initform 3.0d0)
   (spike-display
    :initarg :spike-display
    :initform 8.0d0)
   (init-v
    :initarg :init-v
    :initform 0.0d0)
   (injection-current
    :initarg :injection-current
    :initform 4.3d0)
   (voltage)
   (injection-time)
   (tau)))

(defmethod initialize-instance :after ((neuron neuron-sim) &key)
  (let ((init-v (slot-value neuron 'init-v))
	(start-time (slot-value neuron 'start-time))
	(stop-time  (slot-value neuron 'stop-time))
	(res    (slot-value neuron 'res))
	(cap    (slot-value neuron 'cap)))
    (setf (slot-value neuron 'voltage) init-v)
    (setf (slot-value neuron 'injection-time) (cons start-time stop-time))
    (setf (slot-value neuron 'tau) (* res cap))))

(defclass neuron-hh (neuron-sim)
  ((ena
    :initarg :ena
    :type real
    :initform 115.0d0
    :accessor ena
    :documentation "Reversal Potential for Sodium")
   (gna
    :initarg :gna
    :type real
    :initform 120.0d0
    :accessor gna
    :documentation "Sodium Conductance")
   (ek
    :initarg :ek
    :type real
    :initform -12.0d0
    :accessor ek
    :documentation "Reversal Potential for Potassium")
   (gk
    :initarg :gk
    :type real
    :initform 36.0d0
    :accessor gk
    :documentation "Potassium Conductance")
   (el
    :initarg :el
    :type real
    :initform 10.6d0
    :accessor el
    :documentation "Reveral Leak Potential"
    )
   (gl
    :initarg :gl
    :initform 0.30d0
    :type real
    :accessor gl
    :documentation "Leak Conductance")))


(defun alpha-n (volt)
  (/ (* 0.01 (- 10 volt)) (- (exp (/ (- 10 volt) 10)) 1)))

(defun alpha-m (volt)
  (/ (* 0.1d0 (- 25 volt)) (- (exp (/ (- 25 volt) 10)) 1)))

(defun alpha-h (volt)
  (* 0.07 (exp (/ (* -1 volt) 20))))

(defun beta-n (volt)
  (* 0.125 (exp (/ (* -1 volt) 80))))

(defun beta-m (volt)
  (* 4.0d0 (exp (/ (* -1 volt) 18))))

(defun beta-h (volt)
  (/ 1.0d0 (+ (exp (/ (- 30 volt) 10)) 1)))

(defun m-dot (volt m)
  (- (* (alpha-m volt) (- 1 m)) (* (beta-m volt) m)))

(defun n-dot (volt n)
  (- (* (alpha-n volt) (- 1 n)) (* (beta-n volt) n)))

(defun h-dot (volt h)
  (- (* (alpha-h volt) (- 1 h)) (* (beta-h volt) h)))

(defun m-infinity (volt)
  (/ (alpha-m volt) (+ (alpha-m volt) (beta-m volt))))

(defun n-infinity (volt)
  (/ (alpha-n volt) (+ (alpha-n volt) (beta-n volt))))

(defun h-infinity (volt)
  (/ (alpha-h volt) (+ (alpha-h volt) (beta-h volt))))

(defun update (old-value rate-of-change time-step)
  (+ (* rate-of-change time-step) old-value))

(defun dvdt (voltage-now hh-m hh-n hh-h neuron-parameters)
  (with-slots (ena gna ek gk el gl) neuron-parameters
    (+ (* gna (expt hh-m 3.0d0) hh-h (- voltage-now ena))
       (* gk (expt hh-n 4.0d0) (- voltage-now ek))
       (* gl (- voltage-now el))))
  )


(defun run-hh-sim (nps)
  (with-slots 
	(dt max-t init-t init-v) nps
	(do*
	 ((ts)
	  (vs)
	  (currs)
	  (ms)
	  (ns)
	  (hs)
	  (sim-time 0.0 (+ sim-time dt))
	  (hh-m-sim (m-infinity init-v) (update hh-m-sim (m-dot voltage hh-m-sim) dt ))
	  (hh-n-sim (n-infinity init-v) (update hh-n-sim (n-dot voltage hh-n-sim) dt ))
	  (hh-h-sim (h-infinity init-v) (update hh-h-sim (h-dot voltage hh-h-sim) dt ))
	  (voltage init-v (dvdt voltage hh-m-sim hh-n-sim hh-h-sim nps)))
	 ((> sim-time max-t) (list (nreverse ts) (nreverse vs) (nreverse currs)
				   (nreverse ms) (nreverse ns) (nreverse hs)))
	  (push sim-time ts)
	  (push voltage vs)
	  (push 0.0 currs)
	  (push hh-m-sim ms)
	  (push hh-n-sim ns)
	  (push hh-h-sim hs)
	  )))

