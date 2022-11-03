#lang racket

(require math/matrix
         math/array
         racket/random
         "outer.rkt")


(define (hf-gen-size-n n)
  (for/matrix (sqrt n) (sqrt n)
  ([entry (random-sample (in-list (list 1.0 -1.0)) n)])
  entry))

(define (hf-make-n-patts-size-m n m)
  (for/list ([i (in-range n)])
    (hf-gen-size-n m)))

(define (zero-diag m)
  (let-values ([(r c) (matrix-shape m)])
    (for*/matrix r c
                 ([i (in-range r)]
                  [j (in-range c)]) 
                 (if (= i j) 0.0 (matrix-ref m i j)))))
                 

(define (hop-mk-wts patt-list)
  (let*-values ([(r c) (matrix-shape (first patt-list))]
                [(rc) (* r c)])
    (for/fold ([m (make-matrix (* r r) (* c c) 0)]
               #:result (matrix-scale (zero-diag m) (/ 1 (length patt-list))))
              ([p patt-list])
      (let ([p-col (array-reshape p (vector (* r c) 1))])
        (matrix+ m (outer-prod p-col p-col))))))
    
   IAMHERE 
    
; #+BEGIN_SRC python :session *hop* :tangle yes
;   def hopMkWts(patterns):
;       w = np.zeros(list(map((lambda x: x**2),patterns[0].shape)))
;       for p in patterns:
;           w = w + np.outer(p,p)
;       w = 1.0/len(patterns) * w
;       np.fill_diagonal(w,0)
;       return(w)
; #+END_SRC
; 
; #+RESULTS:
; 
; #+BEGIN_SRC python :session *hop* :tangle yes
;   def hopLoop(patt,wts):
;       workingpatt = deepcopy(patt)
;       testpatt    = deepcopy(patt)
;       while True:
;           rws = list(range(patt.shape[0]))
;           cls = list(range(patt.shape[1]))
;           r.shuffle(rws)
;           r.shuffle(cls)
;           linpatt = np.reshape(workingpatt,(1,workingpatt.size))
;           for rw in rws:
;               for cl in cls:
;                   workingpatt[rw][cl] = 1.0 if np.dot(linpatt,wts[rw*len(rws) + cl]) > 0 else -1.0
;           break
;                         #          if (np.all(testpatt == inpatt)): break
;       return(workingpatt)
; #+END_SRC
; 
; #+RESULTS:
; 
; ****** Function to Loop Asynchronously Through all the Elements of a
; Pattern Until Output is Stable
;        :PROPERTIES:
;        :CUSTOM_ID: function-to-loop-asynchronously-through-all-the-elements-of-a-pattern-until-output-is-stable
;        :END:
; 
; ****** A Function to Visualize the Output
;        :PROPERTIES:
;        :CUSTOM_ID: a-function-to-visualize-the-output
;        :END:
; 
; #+BEGIN_SRC python :session *hop* :tangle yes
;   def hopPlot(ins,outs): 
;       r = len(ins)
;       c = 3
;       pltcntr = 1
;       for i in range(r):
;           p.subplot(r,3,pltcntr)
;           p.imshow(ins[i])
;           p.subplot(r,3,(pltcntr+1))
;           p.imshow(outs[i])
;           p.subplot(r,3,(pltcntr+2))
;           p.imshow(ins[i]-outs[i])
;           pltcntr = pltcntr+3
;       return(p)
; #+END_SRC
; 
; #+RESULTS:
; 
; ****** Example of How to Use
;        :PROPERTIES:
;        :CUSTOM_ID: example-of-how-to-use
;        :END:
; 
; #+BEGIN_SRC python :session *hop* :tangle yes
;   myn,mysz = hopGenSzN()
;   myps = hopMkPatts(myn,mysz)
;   w = hopMkWts(myps)
;   outps = []
;   for inp in myps:
;       op = hopLoop(inp,w)
;       outps.append(op)
; #+END_SRC
; 
; #+RESULTS:
; 
; #+BEGIN_SRC python :session *hop* :tangle yes :results graphics file :file "hop.png"
;   myp = hopPlot(myps,outps)
; #+END_SRC
; 
; #+RESULTS:
; 
; file:8a1284b97d78ce1c4330d596fa0ccc6873678a1d.png]]
; 

