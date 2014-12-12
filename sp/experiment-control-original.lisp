;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ACT-R Sentence Parsing Model
;;;      
;;; Copyright (C) 2006 Shravan Vasishth, Rick Lewis
;;; 
;;; Extended by Felix Engelmann 2012/2013 to work with the EMMA eye 
;;; movement model in ACT-R 6
;;;
;;; Includes the ACT-R Sentence Parsing Model processing
;;; German negative and positive polarity items as described in the
;;; Cognitive Science article Vasishth, Bruessow & Lewis (2007).
;;; 
;;; The original English model is described in the Cognitive Science
;;; article Lewis & Vasishth (2004). 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : 
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 
;;;             : * 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-experiment (name &key conditions contrasts (full-name
                                                             name) (plot-data 'yes))
  `(define-experiment-fct ',name ',full-name ',plot-data ',conditions ',contrasts)
  )

(defun define-experiment-fct (name full-name plot-data conditions contrasts)
  (setf (get '*experiments* name) (list conditions contrasts full-name plot-data))
  )



(defun run-all ()
  (with-open-file (efile "input/all-experiments"
			 :direction :input)
    (while (let ((experiment (read efile nil nil)))
             (when experiment
               (format t "

Running experiment ~A....
" experiment)
               (eval `(run-experiment ,experiment)))
             experiment)))
  )




(defmacro run-experiment (name &optional (iterations 1) &rest params)
  (when (null params)
    (setf params '(:v t)))
  
  `(let ((results nil)
	 (aggregate nil)
	 (condresults nil)
	 (conds (first (get '*experiments* ',name)))
	 (contrasts (second (get '*experiments* ',name)))
	 (cntrstresults nil)
	 (corr nil)
	 (successes 0))
     
     (dotimes (j ,iterations)
       (format t "
Iteration ~A, ~A
" j ',params)
       (let ((result (run-experiment-fct ',name ',params)))
	 (when result
	   (setf successes (+ successes 1))
	   (push result results))))
     
     (setf corr (cons 'correlation (mean (mapcar 'get-corr results))))
     
     (setf aggregate (list ',name ',params corr))
     
     (dolist (c conds)
       (let* ((cname (first c))
	      (regions (rest (rest c)))
	      (cresult (list cname)))
	 (dolist (r regions)
	   (let* ((rname (first r))
		  (humandata (second (member :data r)))
		  (modeldata (mean (mapcar
				    #'(lambda(res)
					(let* ((conds (fourth res))
					       (thiscond (cdr (assoc cname conds)))
					       (thisreg (cdr (assoc rname
								    thiscond))))
					  (car thisreg)))
				    results))))
	     (push-last (list rname modeldata humandata) cresult)))
	 (push-last cresult condresults)))
     
     (push-last condresults aggregate)
     
     
     (dolist (c contrasts)
       (let* ((name (first c))
	      (humandata (second (cdr (assoc name (fifth (first results))))))
	      (modeldata (mean (mapcar
				#'(lambda(res)
				    (let* ((cntrsts (fifth res))
					   (thiscntrst (cdr (assoc name
					                           cntrsts))))
				      (car thiscntrst)))
				results))))
	 (push-last (list name modeldata humandata) cntrstresults)))
     
     (push-last cntrstresults aggregate)
     
     
     (format t "

Aggregate (mean) result  from ~A successful runs in ~A iterations:"
	     successes ,iterations)
     
     (display-experiment-result aggregate)
     (graph-and-fit-experiment aggregate))
  )


(defun graph-and-fit-experiment(result)
  (let* ((name (string (first result)))
	 (full-name (string (third (get '*experiments* (first result)))))
	 (plot-data (string (fourth (get '*experiments* (first result)))))
	 (conditions (fourth result))
	 (pdfname (string-downcase
		   (concatenate 'string name ".pdf")))
	 (results-file (string-downcase
			(concatenate 'string "output/" name "-results"))))
    
    (with-open-file (rfile results-file
			   :direction :output
			   :if-exists :supersede)
      (format rfile "~s
~s
~s
condition, model, data
" full-name plot-data pdfname)
      
      (dolist (c conditions)
        (let ((cname (first c))
              (regions (rest c)))
          (dolist (r regions)
            (let* ((rname (first r))
                   (newcond (string-downcase
                             (concatenate 'string (string cname)
				          ":" (string rname))))
                   (modeldata (* 1000 (second r)))
                   (humandata (* 1000 (third r))))
              (format rfile "~s,~6,3F,~6,3F
" newcond modeldata humandata))))))
    )
  )



(defun display-experiment-result(result)
  (let ((name (first result))
	(params (second result))
	(corr (cdr (third result)))
	(conditions (fourth result))
	(contrasts (fifth result)))
    (format t "
_____________________________________________________________________

Simulation results for experiment ~A with parameters ~A:
R: ~6,3F  R-squared:  ~6,3F       Empirical times inside []
_____________________________________________________________________
" name params corr (* corr corr))
    (dolist (c  conditions)
      (let ((cname (first c))
	    (regions (rest c)))
	(format t "
      ~A condition
" cname)
	(dolist (r regions)
	  (let ((rname (first r))
		(modeldata (second r))
		(humandata (third r)))
	    (format t "            ~20s: ~6,3F    [~6,3F]
" rname modeldata humandata)
	    ))))
    
    (format t"
    _________________________________________________________________

"
            )
    
    (dolist (c contrasts)
      (let ((cntrname (first c))
	    (model (second c))
	    (human (third c)))
	(format t "      Contrast ~20s: ~6,3F    [~6,3F]
" cntrname model human)))
    
    (format t "_____________________________________________________________________"))
  )


(defun run-experiment-fct (name params)
  (let ((conditions (first (get '*experiments* name)))
	(contrasts (second (get '*experiments* name)))
	(exp-results nil)
	(model-times nil)
	(human-times nil)
	(corr nil)
	(contrast-results nil)
	(final-result nil))
    
    ;; Loop over conditions first....
    
    (dolist (c conditions)
      (let* ((cname (pop c))
	     (sent (eval (pop c)))
	     (regions c)
	     (model-results nil))
        
	(present-whole-sentence sent *max-time* params)
        
	;; ... then regions
        
	(dolist (r regions)
	  (let* ((rname (first r))
		 (position (second (member :position r)))
		 (human-data (second (member :data r)))
		 (model-data
		  (if (listp position)
                    (let ((s (- (first position) 1))
                          (e (second position)))
                      (mean (subseq *attach-times* s e)))
		    (nth (- position 1) *attach-times*))))
            
	    (when (null model-data)
	      (format t "
       .....FAILURE.....

"
)
	      (setf model-data -999))
            ;	      (return-from run-experiment-fct nil))
	    
	    
	    (push (list rname model-data human-data) model-results)
	    (push human-data human-times)
	    (push model-data model-times)
	    ));let reg; dolist reg
	
	(push-last (cons cname model-results) exp-results)
	));let cond; dolist cond
    
    
    (format t "

Computing correlations....

"
)
    (setf corr (cons 'correlation (correlation model-times human-times)))
    
    ;; Compute contrasts
    
    (format t "

Computing contrasts.....

"
)
    
    
    (dolist (c contrasts)
      (let* ((name (first c))
	     (region (second c))
	     (m-contrast nil)
	     (h-contrast nil)
	     (vector (third c))
	     (model-times (mapcar #'(lambda(cond)
				      (let ((times (cdr cond)))
					(second (assoc region times))))
				  exp-results))
	     (human-times (mapcar #'(lambda(cond)
				      (let ((times (cdr cond)))
					(third (assoc region times))))
				  exp-results)))
        
	(setf m-contrast (sum (mapcar '* vector model-times)))
	(setf h-contrast (sum (mapcar '* vector human-times)))
	(push-last (list name m-contrast h-contrast) contrast-results)))
    
    
    (setf final-result (list name params corr exp-results contrast-results))
    
    (setf *experiment-results*
	  (merge 'list (list final-result)
		 *experiment-results*  #'> :key 'get-corr))
    
    final-result
    ))



(defmacro search-param-space (experiment param-space)
  (let ((code `(run-experiment-fct ',experiment))
	(param-vars nil)
	(parameters '(list :v t)))
    (dolist (p (eval param-space))
      (let ((new-var (gensym))
	    (parameter (first p)))
	(push (cons parameter new-var) param-vars)
	(push-last parameter parameters)
	(push-last new-var parameters)))
    
    (push-last parameters code)
    
    (dolist (p (eval param-space))
      (let* ((new-var (cdr (assoc (first p) param-vars)))
	     (init-val (second p))
	     (final-val (third p))
	     (step-val (fourth p))
	     (do-code `(do ((,new-var ,init-val (+ ,new-var
						   ,step-val)))
			   ((> ,new-var ,final-val)))))
	(setf code (push-last code do-code))
	))
    code))


;;; HELPERS ;;;
(defun mean(l)
  (/ (sum l) (length l)))

(defun get-corr (exp-results)
  (cdr (third exp-results))
  )
  
(defun sum (nums)
  (eval (cons '+ nums))
  )