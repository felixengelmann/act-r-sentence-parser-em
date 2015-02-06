
;; Run if something changed in ACT-R files:
(push :actr-recompile *features*)
;; Compile for faster processing:
(push :actr-fast *features*)

;; Set Working directory (not necessarily necessary):
; (setf *default-pathname-defaults* #P"/PATH_TO_YOUR_WORKSPACE/ACTR-SentenceParser-EM/LewisVasishth2005/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run ACT-R and load the model:
(load "../actr6/load-act-r-6.lisp")
(load "sp-vbld08.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In case environment should be used:
(run-environment)
; (run-program "open" '("sp/actr6/environment/Start\ Environment\ OSX.app"))
; (start-environment)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOME EXAMPLES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Demos
;;;
(demo)
(demo '(:lf 0.8))
(demo1)
(demo2)


;;;
;;; Print information
;;;
(print-params)
(print-interface-params)
(print-runtime-vars)
(parsing-print-info)


;;;
;;; Useful functions
;;;
(rl) 				;; reload model files
(clear-sp) 	;; reload all files
(delete-output)
;; Change trace output:
(setprint full)
(setprint firing)
(setprint notrace)
(setprint off)
(setprint default)


;;;
;;; Run sentences
;;;
(ps *gg-sr*) 
(ps *gg-or* :params '(:lf 0.8)) 
;; For better eye movement presentation:
(setf *real-time* T)


;;;
;;; Set interface parameters
;;;
(setf *output-dir* "output")
(setf *real-time* T)
(setf *record-times* T)
(setf *record-visloc-activations* T)
;; Model parameters:
(setf *read-corpus* nil)
(setf *raw-freq* nil)
(setf *surprisal-on* nil)
(setf *surprisal-hl-on* nil)
(setf *fake-retrieval-on* nil)
(setf *time-penalty-factor* 0.1) ;; "Factor for penalizing use of time-out productions (p = -FACTOR*FIRING-COUNT)"


;;;
;;; Experiments
;;;
;; Run experiment:
(re 'vbld08-pirat 10) 
(re 'gg-exp1 60 :params '(:lf 0.4 :mp 2))
(re 'staub10 60)
;; Run experiment with subjects:
(res 'vbld08-pirat 10 5) 
(res 'gg-exp1 10 5 :params '(:lf 0.3 :mp 3))
(res 'gg-exp1 100 2 :ga '(0.75 1.25) :params '(:lf 0.3))


;;;
;;; Search param-space
;;;
(setf *paramspace* '(
                     (:lf .2 .4 .1)
                     (:mp 1.5 2 .5)
                     ))

(search-param-space-em vbld08-pirat 10 *paramspace*)
(search-param-space-subjects-em vbld08-pirat 5 2 *paramspace*)


;;;
;;; Run external scripts or programs
;;;
(cwd "output/")
(run-program "Rscript" '("quick-analysis.R"))
(cwd "../")
; (run-program "open" '("output/results.pdf"))
