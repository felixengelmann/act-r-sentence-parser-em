;;;-*-LISP-*-
;;; ===================================================================
;;;    ACT-R Sentence Parsing Model
;;;      
;;; Copyright (C) 2006 Shravan Vasishth, Rick Lewis, Sven Bruessow
;;;
;;; This file is part of the ACT-R Sentence Parsing Model processing
;;; German negative and positive polarity items as described in the
;;; Cognitive Science article  Vasishth, Bruessow & Lewis (2007).
;;;	
;;;    The original English model is described in the Cognitive Science
;;;    article Lewis & Vasishth (2004). 
;;; 
;;;    Version 2.9
;;;    July 15 2006
;;; ===================================================================
;;;
;;; The model is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; The model is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.




;;;
;;; LEX-TO-SYN MAPPINGS
;;;

;(defvar *case-mappings* '((all . all-cases-lex)))
(defvar *case-mappings* '((nom . nom-lex)
                          (gen . gen-lex)
                          (dat . dat-lex)
                          (acc . acc-lex)
                          (all . all-cases-lex)))


;(defvar *npi-mappings* '((npi-licensor . npi-licensor-lex)
;			 (npi . npi-lex)
;			 (not-npi . not-npi-lex)))


;(defvar *polarity-item-mappings* '((negative . neg-lex)
;                                   (positive . pos-lex)
;                                   (zero . zero-lex)))

(defvar *polarity-item-mappings* '((negative . negative)
                                   (positive . positive)
                                   (zero . zero)))


(defvar *tense-mappings* '((past . past-lex)
                           (pres . pres-lex)))

(defvar *mood-mappings* '((indicative . indicative-lex)
                           (subjunctive . subjunctive-lex)))

(defvar *finite-mappings* '((finite . finite-lex) (infinite . infinite-lex)))

(defvar *number-mappings* '((singular . sing-lex) 
                            (plural . plural-lex)
                            (singular-plural . sing-plural-lex)
                            ))

(defvar *gender-mappings* '((all . all-genders-lex) (fem . fem-lex)
			    (masc . masc-lex)))

(defvar *subcat-mappings* '((intransitive . intransitive-lex)
                            (transitive-CP-finite .  transitive-CP-finite-lex)
                            (transitive-DP . transitive-DP-lex)
                            (transitive-NP . transitive-NP-lex)
                            (transitive-CP-DP-finite . transitive-CP-DP-finite-lex)
                            (gerund . gerund-lex)
                            (copula . copula-lex)
                            (past-participle-transitive . past-participle-transitive-lex)))

(defvar *lex-mappings* '((nom . nom-lex)
                          (gen . gen-lex)
                          (dat . dat-lex)
                          (acc . acc-lex)
                          (all . all-cases-lex)
                          (sing . sing-lex) 
                          (plur . plural-lex)
                          (singular-plural . sing-plural-lex)
                          (all . all-genders-lex) (fem . fem-lex) (masc . masc-lex)
                          (past . past-lex) (pres . pres-lex)
                          (finite . finite-lex) (infinite . infinite-lex)
                          (intransitive . intransitive-lex)
                          (transitive-CP-finite .  transitive-CP-finite-lex)
                          (transitive-DP . transitive-DP-lex)
                          (transitive-CP-DP-finite . transitive-CP-DP-finite-lex)
                          (gerund . gerund-lex)
                          (past-participle-transitive . past-participle-transitive-lex)
                          (negative . neg-lex) (positive . pos-lex) (zero . zero-lex)
                          (npi-licensor . npi-licensor-lex) (npi . npi-lex) (not-npi . not-npi-lex)
                          (indicative . indicative-lex)
                          (subjunctive . subjunctive-lex)
                          (NP . N)
                          (DP . DET)
                          (VP . V)
                          (PP . P)
                          (nil . none)
                          (NP . NP-lex)
                          (DP . DP-lex)
                          (VP . VP-lex)
                          (PP . PP-lex)
                          ))
  
  
  
(defun map-lex-to-syn (lex)
  ; (when *VERBOSE* (model-warning " +++ Setting next-goal to ~A. +++" goal))
  (car (rassoc lex *lex-mappings*))
)



(defmacro noun(word &key (number 'singular)
		    (case   'all)
		    (gender 'all)
		    (cat    'N)
		    (subcat 'intransitive)
		    id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(case (cdr (assoc case *case-mappings*)))
	(subcat (cdr (assoc subcat *subcat-mappings*)))
	(chunk-id id)
	(gender (cdr (assoc gender *gender-mappings*))))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   number       ,num
		   case         ,case
		   subcat       ,subcat
		   gender       ,gender))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )





(defmacro verb(word &key (number 'singular-plural)
		    (tense 'past)
		    (finite 'finite)
		    (cat    'V)
		    (subcat 'transitive-DP)
        (mood   'indicative)
		    id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(subcat (cdr (assoc subcat *subcat-mappings*)))
	(tense (cdr (assoc tense *tense-mappings*)))
	(finite (cdr (assoc finite *finite-mappings*)))
  (mood (cdr (assoc mood *mood-mappings*)))
	(chunk-id id))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   tense        ,tense
		   finite       ,finite
		   number       ,num
       mood         ,mood
		   subcat       ,subcat))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )



(defmacro det(word &key (number 'singular-plural)
                   (case   'all)
                   (gender 'all)
                   (cat    'DET)
                   polarity
                   id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(case (cdr (assoc case *case-mappings*)))
        (polarity (cdr (assoc polarity *polarity-item-mappings*)))
	(chunk-id id)
	(gender (cdr (assoc gender *gender-mappings*))))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   number       ,num
		   case         ,case
		   polarity     ,polarity
		   gender       ,gender
       subcat       transitive-NP-lex))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )



(defmacro comp(word &key (cat    'C)
		    (finite 'finite)
		    id)
  
  (let ((finite (cdr (assoc finite *finite-mappings*)))
	(chunk-id id))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   finite       ,finite
		   subcat       nil))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )



(defmacro inf(word &key (cat    'inf)
                   (finite 'infinite)
                   id)
  
  (let ((finite (cdr (assoc finite *finite-mappings*)))
	(chunk-id id))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   finite       ,finite
		   subcat       nil))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )




(defmacro prep(word &key (cat    'P)
		    (subcat 'transitive-DP)
		    id)
  
  (let ((subcat (cdr (assoc subcat *subcat-mappings*)))
	(chunk-id id))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   subcat       ,subcat))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )




(defmacro adj(word &key (number 'singular-plural)
                   (case   'all)
                   (gender 'all)
                   (cat    'adj)
                   (subcat 'intransitive)
                   id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(case (cdr (assoc case *case-mappings*)))
	(subcat (cdr (assoc subcat *subcat-mappings*)))
	(chunk-id id)
	(gender (cdr (assoc gender *gender-mappings*))))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   number       ,num
		   case         ,case
		   subcat       ,subcat
		   gender       ,gender))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )




(defmacro adv(word &key (number 'singular-plural)
                   (case   'all)
                   (gender 'all)
                   (cat    'adv)
                   (subcat 'intransitive)
                   polarity
                   id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(case (cdr (assoc case *case-mappings*)))
	(subcat (cdr (assoc subcat *case-mappings*)))
        (polarity (cdr (assoc polarity *polarity-item-mappings*)))
	(chunk-id id)
	(gender (cdr (assoc gender *gender-mappings*))))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   number       ,num
		   case         ,case
		   subcat       ,subcat
                   polarity      ,polarity
		   gender       ,gender))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )




;; VD11
(defmacro pnoun(word &key (number 'singular)
		    (case   'all)
		    (gender 'all)
		    (cat    'PN)
;		    (subcat 'intransitive)
		    id)
  
  (let ((num (cdr (assoc number *number-mappings*)))
	(case (cdr (assoc case *case-mappings*)))
;	(subcat (cdr (assoc subcat *subcat-mappings*)))
	(chunk-id id)
	(gender (cdr (assoc gender *gender-mappings*))))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   number       ,num
		   case         ,case
;		   subcat       ,subcat
		   gender       ,gender))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )



(defmacro conj(word &key (cat    'CON)
		    (subcat 'transitive-DP)
		    id)
  
  (let ((subcat (cdr (assoc subcat *subcat-mappings*)))
	(chunk-id id))
    
    `(progn
       (add-dm
	(,chunk-id isa lexical-entry
		   word         ,(string word)
		   cat          ,cat
		   subcat       ,subcat))
       
       (set-base-levels (,chunk-id 1000))
       
;       ,(create-lexical-production word)
       ))
  )




; (defun create-lexical-production(word)
;   (let*
;     ((wordstr (string-downcase word))
;      (pname (intern (concatenate 'string "lexical-retrieval-request-" wordstr))))
    
;     `(P ,pname
;       =goal>
;         isa            	comprehend-sentence
;         state		"attending"
;       =visual>
;         ISA         	text
;         value       	,wordstr
;    ==>
;       =goal>
;         state       	"lexical-retrieval"
;         cue1              ,wordstr
;         cue2              nil
;         cue3              nil
;         cue4              nil
;       -visual-location>
;       +retrieval>
;         ISA         	lexical-entry
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
;         word        	,wordstr
        
;         !eval! (word-message ,wordstr)))
;   )

