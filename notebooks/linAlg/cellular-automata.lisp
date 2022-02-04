(ql:quickload "trivia")
(ql:quickload "zpng")
(ql:quickload "str")

(defpackage #:cellular-automata
  (:nicknames "CA")
  (:use #:cl)
  (:import-from "TRIVIA" "MATCH")
  (:import-from "STR" "PAD")
  (:import-from "ZPNG" "PNG" "DATA-ARRAY" "WRITE-PNG"))

(in-package :ca)

(defun rule0 (ns) 
	   (match ns
	     ('(w w w) 'w)
	     ('(w w b) 'w)
	     ('(w b w) 'w)
	     ('(b w w) 'w)
	     ('(w b b) 'w)
	     ('(b w b) 'w)
	     ('(b b w) 'w)
	     ('(b b b) 'w)))

(defun rule110 (ns) 
	   (match ns
	     ('(w w w) 'w)
	     ('(w w b) 'b)
	     ('(w b w) 'b)
	     ('(b w w) 'w)
	     ('(w b b) 'b)
	     ('(b w b) 'b)
	     ('(b b w) 'b)
	     ('(b b b) 'w)))

(defun make-rule (rule-no)
    (flet ((white-or-black? (in-char)
	     (if (string= in-char #\0) 'w 'b)))
    (let ((test-char (reverse (pad 8 (write-to-string rule-no :base 2) :pad-char "0" :pad-side :left))))
      (lambda (ns)
      (match ns
	 ('(w w w) (white-or-black? (elt test-char 0)))
	 ('(w w b) (white-or-black? (elt test-char 1)))
	 ('(w b w) (white-or-black? (elt test-char 2)))
	 ('(w b b) (white-or-black? (elt test-char 3)))
	 ('(b w w) (white-or-black? (elt test-char 4)))
	 ('(b w b) (white-or-black? (elt test-char 5)))
	 ('(b b w) (white-or-black? (elt test-char 6)))
	 ('(b b b) (white-or-black? (elt test-char 7))))))))

(defparameter r110 (make-rule 110))

(defun top-row (n)
  (multiple-value-bind
	(how-many-start how-many-end)
      (if (evenp n)
	  (values (- (/ n 2) 1) (/ n 2))
	  (values (/ (- n 1) 2)  (/ (- n 1) 2)))
    (append (make-list how-many-start :initial-element 'w) (cons 'b nil) (make-list how-many-end :initial-element 'w))))

(defun pad-front (in-list)
  (cons (elt in-list (- (length in-list) 1))
	(subseq in-list 0 2)))

(defun pad-end (in-list)
  (append (subseq in-list (- (length in-list) 2) (length in-list)) (list (first in-list))))

(defun crawl-cols (which-rule in-list)
  (do*  ((next-row (list (funcall which-rule (pad-front in-list))))
	 (when-to-quit (length in-list))
	 (start 0 (+ 1 start))
	 (stop 3 (+ 1 stop))
	 (patt (funcall which-rule (subseq in-list start stop)) (funcall which-rule (subseq in-list start stop))))
	 ((= stop when-to-quit) (nreverse (append (list (funcall which-rule (pad-end in-list)) patt) next-row)))
    (push patt next-row)))

(defun build-rows (which-rule &key (ncols 200) (nrows 200))
  (let ((lol (list (top-row ncols))))
    (dotimes (rs (- nrows 1) (nreverse lol))
      (push (crawl-cols which-rule (elt lol 0)) lol)
      )))
    

(defun rule-num-to-png (rule-num file-name
			&key (xsize 200)
			  (ysize 200))
  (let* ((which-rule (make-rule rule-num))
	 (rule-dat (build-rows which-rule :ncols xsize :nrows ysize))
	 (pic (make-instance 'png :width  xsize
				  :height ysize
				  :color-type :grayscale))
	 (image-data (data-array pic)))
    (dotimes (ri (length rule-dat) (write-png pic file-name))
      (let ((cur-row (elt rule-dat ri)))
	(dotimes (ci  (length cur-row))
	  (setf (aref image-data ri ci 0) (if (equal (elt cur-row ci) 'w) 255 0)))))))
