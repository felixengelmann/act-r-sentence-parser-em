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


;; vd11 test
(setf *vd11test* "the girl saw the fish that the boy liked *stop*")

;; Simple clauses and PPs

(setf *simple* "the dog bit the boy *stop*")
(setf *simple-pp* "the dog with the boy *stop*")

;;; LONG SUBJECTS

(setf *pp-simple* "the dog with the girl saw the man *stop*")
(setf *long* "the dog with the man with the boy with the man saw the girl *stop*")

;;; Deep RIGHT BRANCHING

(setf *rb1* "the boy saw the duck with the dog with the boy with the girl *STOP*")
(setf *rb2* "the boy thought that the man said that the dog bit the boy *stop*") 
(setf *rb3* "the boy thought the man said the dog bit the boy *stop*")



;;; RELATIVE CLAUSES:  subject and object relatives

(setf *sor-hinsubjrc* "the boy jisne us dog saw *stop*")

(setf *sor* "the boy that the dog bit saw the man *stop*")
(setf *ssr* "the dog that bit the boy saw the man *stop*")
(setf *ssr2* "the dog that bit the boy cried  *stop*")
(setf *sor2* "the man that the boy saw cried *stop*")


;;    Post-verbal object relative

(setf *pvdor* "the man liked the fish that the boy that the dog bit saw *stop*")


;;    Long object relatives

(setf *lsor* "the boy that the dog with the fish bit saw the man *stop*")
(setf *vlsor* "the boy that the dog with the fish with the girl bit saw the man *stop*")


;;    Classic double object relative

(setf *dor* "the writer that the editor that the dog chased scolded supervised *stop*") 



;; SENTENTIAL COMPLEMENTS

(setf *sc* "the claim that the boy bit the dog upset the girl *stop*")



;;  SC/RC mixes (sentential complements/relative clauses)

(setf *sc-rc* "the claim that the boy that the dog bit cried upset the girl *stop*")
(setf *rc-sc* "the boy that the claim that the dog cried upset saw the girl *stop*")


;; Van Dyke & Lewis 2003 constructions

(setf *vlshort* "the assistant forgot that the student was standing in the hallway *stop")

(setf *vllong* "the assistant forgot that the student who was waiting for the exam was standing in the hallway *stop*")

(setf *vlint* "the assistant forgot that the student who knew that the exam was important was standing in the hallway *stop*")

(setf *vlshorta* "the assistant forgot the student was standing in the hallway *stop")

(setf *vllonga* "the assistant forgot the student who was waiting for the exam was standing in the hallway *stop*")

(setf *vlinta* "the assistant forgot the student who knew that the exam was important was standing in the hallway *stop*")



;; Grodner & Gibson 2004 constructions

(setf *gg-or* "the reporter who the photographer sent to the editor hoped for the story *stop*")

(setf *gg-sr* "the reporter who sent the photographer to the editor hoped for the story *stop*")

(setf *gg-matrix-us* "the nurse supervised the administrator *stop")

(setf *gg-matrix-pp* "the nurse from the clinic supervised the administrator *stop")

(setf *gg-matrix-rc* "the nurse who was from the clinic supervised the administrator *stop*")

(setf *gg-embedded-us* "the administrator who the nurse supervised scolded the medic *stop*")

(setf *gg-embedded-pp* "the administrator who the nurse from the clinic supervised scolded the medic *stop*")

(setf *gg-embedded-rc* "the administrator who the nurse who was from the clinic supervised scolded the medic *stop*")



;; McElree, Foraker & Dyer (2003) constructions

(setf *mfd-no* "the editor laughed *stop*")

(setf *mfd-or* "the editor that the book amused laughed *stop*")

(setf *mfd-ppor* "the book from the press that the editor admired ripped *stop*")

(setf *mfd-orsr* "the book that the editor who quit the journal admired ripped *stop*")

(setf *mfd-oror* "the book that the editor who the receptionist married admired ripped *stop*")


;; Gibson, Grodner & Tunstall (1997) constructions (reported in
;; Gibson, 2000)

(setf *ggt-sc* "the witness thought that the evidence that was examined by the lawyer implicated his neighbor *stop*")

(setf *ggt-rc* "the witness who the evidence that was examined by the lawyer implicated admired the editor *stop*")


;;  Untensed sentential subjects vs. other embeddings

(setf *ssu* "forc the reporter who the editor hired scold the medic upset  *stop*")

(setf *sst* "that the reporter who the editor married liked the medic was  *stop*")

(setf *sc-rc2* "the claim that the reporter who the editor admired sent the medic amused *stop*")

(setf *rc-sc2* "the reporter who the claim that the editor admired the medic amused sent *stop*")

(setf *or-sr* "the medic who the dog that bit the reporter saw quit *stop*")

(setf *sc-sc* "the claim that the news that the reporter admired the medic amused the editor implicated *stop*")


;; PP recency 

(setf *recency* "the medic with the assistant with the dog with the editor with the duck with the reporter with the receptionist with the book *stop*")


;; VP-NP PP attachment ambiguity

(setf *vp-np-ambig* "the medic saw the assistant with the dog *stop*")



;; Deep clausal right-branching

(setf *deep-rb* "the boy thought that the man said that the editor thought that
the writer said that the medic thought that the assistant admired the student *stop*") 



;; Misc. 

(setf *lex1* "the boy will duck *stop*")
(setf *lex2* "the boy saw her duck *stop*")

(setf *race1* "the boy saw the duck *stop*")
(setf *race2* "the boy saw the goose *stop*")
(setf *invited* "the boy invited the man *stop*")

(setf *ps-violation-elan* "the dog saw the bit *stop*")
(setf *num-violation* "the dog bits the girl *stop*")



;;  Staub (2010): SR/OR
(setf *staub-sr* "The employees who noticed the fireman hurried to the clinic *")
(setf *staub-or* "The employees who the fireman noticed hurried to the clinic *")

;; Titus' Spanish
(setf *titusfake1* "the witness said that the employees cried from the clinic *")
(setf *titusfake2* "the witness said that the employees admired the clinic *")
(setf *titusfake3* "the witness said that the nurse thought that the employees admired the clinic *")


;;; Frazier and Rayner (1982)
(setf *fr82-a* "Since jay always jogs a mile it seems like a short distance to him *")
(setf *fr82-b* "Since jay always jogs a mile seems like a short distance to him *")

;;; Traxler (2007)
(setf *traxler07-a* "The writer of the letter that had blonde hair arrived this morning *")
(setf *traxler07-b* "The letter of the writer that had blonde hair arrived this morning *")
(setf *traxler07-c* "The sister of the writer that had blonde hair arrived this morning *")

;;; Kemper, Crow, and Kemtes (2004)
(setf *kck04-a* "The experienced soldiers warned about the dangers before the midnight raid *")
(setf *kck04-b* "The experienced soldiers warned about the dangers conducted the midnight raid *")
(setf *kck04-c* "The experienced soldiers spoke about the dangers before the midnight raid *")
(setf *kck04-d* "The experienced soldiers who were told about the dangers conducted the midnight raid *")

(setf *kck04-a* "The soldiers warned about the dangers before the midnight raid *")
(setf *kck04-b* "The soldiers warned about the dangers conducted the midnight raid *")
(setf *kck04-c* "The soldiers spoke about the dangers before the midnight raid *")
(setf *kck04-d* "The soldiers who were told about the dangers conducted the midnight raid *")


;; EXPERIMENTS

(define-experiment VL-exp4
  :full-name "Van Dyke & Lewis (2003) Exp. 4"
  :plot-data yes
  :conditions
  ((unambig-short    *vlshort* (critical-verb :position 7 :data .050))   ; 50
   (unambig-low-int  *vllong*  (critical-verb :position 13 :data .006)  ; 6
		               (region2 :position (7 12)  :data .022)) ; 22
   (unambig-high-int *vlint*   (critical-verb :position 14 :data .062) ; 62
		               (region2 :position (7 13)  :data .019)) ; 19
   (ambig-short    *vlshorta* (critical-verb :position 6 :data .048)) ; 48
   (ambig-low-int  *vllonga*  (critical-verb :position 12 :data .092)) ; 92 
   (ambig-high-int *vlinta*   (critical-verb :position 13 :data .161));  161
   )

  :contrasts ((gp-effect critical-verb (-.3333 -.3333 -.3333 .3333 .3333
  .3333))
	      (int-effect critical-verb (0 -0.5 0.5 0 -0.5 0.5))
	      (length-effect critical-verb (-0.5 0.5 0 -0.5 0.5 0))
	      (attach-length critical-verb  (-1 1 0 0 0 0))
	      (attach-interference critical-verb  (0 -1 1 0 0 0))
	      (gp-length critical-verb (1 -1 0 -1 1 0))
	      (gp-int critical-verb (0 1 -1 0 -1 1)))
)


(define-experiment GG-exp1
  :full-name "Grodner & Gibson (2004) Exp. 1"
  :plot-data yes
  :conditions
  ((subject-rel   *gg-sr* (embedded-verb :position 4  :data .344)
		  (main-verb     :position 10 :data .381))
   (object-rel    *gg-or* (embedded-verb :position 6  :data .399)
		  (main-verb     :position 10 :data .383)))

  :contrasts ((main main-verb (-1 1))
	      (embedded embedded-verb (-1 1)))
)



(define-experiment GG-OR
  :full-name "Grodner & Gibson (2004) OR"
  :plot-data yes
  :conditions
  ((object-rel   *gg-or* (the-rep  :position (1 2)  :data .336)
		          (who-phot :position (3 5) :data .349)
			  (sent-to :position (6 7) :data .397)
			  (the-ed :position (8 9) :data .386)
			  (hoped-f :position (10 11) :data .383)
			  (the-story :position (12 13) :data .360)))
)


(define-experiment GG-SR
  :full-name "Grodner & Gibson (2004) SR"
  :plot-data yes
  :conditions
  ((subject-rel   *gg-sr* (the-reporter :position (1 2)  :data .336)
		          (who-sent :position (3 4) :data .352)
			  (the-photo :position (5 6) :data .356)
			  (to-the-ed :position (7 9) :data .353)
			  (hoped-for :position (10 11) :data .395)
			  (the-story :position (12 13) :data .362)))
)




(define-experiment GG-OR-SR-regions
  :full-name "Grodner & Gibson (2004) OR and SR by region"
  :plot-data yes
  :conditions
  ((object-rel   *gg-or* (the-rep  :position (1 2)  :data .336)
		          (who-phot :position (3 5) :data .349)
			  (sent-to :position (6 7) :data .397)
			  (the-ed :position (8 9) :data .386)
			  (hoped-f :position (10 11) :data .383)
			  (the-story :position (12 13) :data .360))
 (subject-rel   *gg-sr* (the-reporter :position (1 2)  :data .336)
		          (who-sent :position (3 4) :data .352)
			  (the-photo :position (5 6) :data .356)
			  (to-the-ed :position (7 9) :data .353)
			  (hoped-for :position (10 11) :data .395)
			  (the-story :position (12 13) :data .362)))
)






(define-experiment GG-exp2
  :full-name "Grodner & Gibson (2004) Exp. 2"
  :plot-data yes
  :conditions
  ((main-bare   *gg-matrix-us* (main-verb  :position 3 :data .375))
   (main-pp     *gg-matrix-pp* (main-verb  :position 6 :data .385))
   (main-rc     *gg-matrix-rc* (main-verb  :position 8 :data .382))
   
   (embedded-bare   *gg-embedded-us* (embedded-verb  :position 6 :data .475))
   (embedded-pp     *gg-embedded-pp* (embedded-verb  :position 9 :data .515))
   (embedded-rc     *gg-embedded-rc* (embedded-verb  :position 11
   :data .568 )))
)



(define-experiment MFD-exp2
  :full-name "McElree, Foraker & Dyer (2003) Exp. 2"
  :plot-data yes
  :conditions
  ((no  *mfd-no*  (main-verb :position 3 :data 0))
   (or  *mfd-or*  (main-verb :position 7 :data .085))
   (ppor  *mfd-ppor*  (main-verb :position 10 :data .085))
   (orsr  *mfd-orsr*  (main-verb :position 11 :data .085)))
 ;  (oror  *mfd-oror*  (main-verb :position 11 :data .260)))

  :contrasts ((retrieval-estimate main-verb (-1 .33333 .33333 .3333 0)))
)


;; (define-experiment MFD-exp2
;;   :full-name "McElree, Foraker & Dyer (2003) Exp. 2"
;;   :conditions
;;   ((no  *mfd-no*  (main-verb :position 3 :data .314))
;;    (or  *mfd-or*  (main-verb :position 7 :data .441))
;;    (ppor  *mfd-ppor*  (main-verb :position 10 :data .389))
;;    (orsr  *mfd-orsr*  (main-verb :position 11 :data .427))
;;    (oror  *mfd-oror*  (main-verb :position 11 :data .574)))

;;   :contrasts ((retrieval-estimate main-verb (-1 .33333 .33333 .3333 0)))
;; )



(define-experiment SC-RC-v-RC-SC
  :full-name "SC-RC vs. RC-SC"
  :plot-data no
  :conditions
  ((sc-rc  *sc-rc2* (embedded-verb :position 9 :data 0)
	   (middle-verb  :position 10  :data 0 )
	   (main-verb  :position  13  :data 0))
   
   (rc-sc  *rc-sc2* (embedded-verb :position 9 :data 0)
	   (middle-verb  :position 12  :data 0)
	   (main-verb  :position  13  :data 0)))
)
  


(define-experiment GGT97-exp
  :full-name "Gibson, Grodner, & Tunstall (1997)"
  :plot-data yes
  :conditions
  ((small-memory-cost *ggt-sc* (verb1 :position 9  :data -.030)
		  (verb2     :position (13 15) :data -.010))

   (large-memory-cost *ggt-rc* (verb1 :position 8  :data .009)
		  (verb2     :position (12 14) :data .060))
   )
)




(define-experiment GGT02-exp
  :full-name "Grodner, Gibson & Tunstall (2002)"
  :plot-data yes
  :conditions
  ((SC *ggt-sc*
;		      (region3 :position (5 6) :data  -.0418)
;		      (region4 :position (7 8) :data -.0132)
		      (region3-4 :position (5 8) :data -.0275)
		      (region5 :position 9  :data -.0402)
		   (region6 :position (10 12) :data  -.0297)
		  (region7     :position (13 15) :data -.0188))

   (RC *ggt-rc*
;		      (region3 :position (4 5) :data  -.0161)
;		      (region4 :position (6 7) :data .0548)
		      (region3-4 :position (4 7) :data .01935)
		      (region5 :position 8  :data .0072)
		      (region6 :position (9 11 ) :data  .0187)
		  (region7    :position (12 14) :data .0792))
   )
)




(define-experiment SS
  :full-name "Untensed sentential subjects vs. SC-RC and double SR"
  :plot-data no
  :conditions
  ((ss-untensed  *ssu* (verb1 :position 7 :data 0)
	     (verb2  :position 8  :data 0 )
	     (verb3  :position  11  :data 0))

   (ss-tensed  *sst* (verb1 :position 7 :data 0)
	 (verb2  :position 8  :data 0 )
	 (verb3  :position  11  :data 0))
   
   (sc-rc *sc-rc2* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  13  :data 0))

   (or-sr *or-sr* (verb1 :position 7 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  11  :data 0)))
  :contrasts ((embedded verb1 (-1 1 0 0))
	      (middle  verb2 (-1 1 0 0))
	      (main  verb3 (-1 1 0 0)))
)


(define-experiment DCE
  :full-name "Double center-embeddings"
  :plot-data no
  :conditions
  ((ss-untensed  *ssu* (verb1 :position 7 :data 0)
	     (verb2  :position 8  :data 0 )
	     (verb3  :position  11  :data 0))

   (ss-tensed  *sst* (verb1 :position 7 :data 0)
	 (verb2  :position 8  :data 0 )
	 (verb3  :position  11  :data 0))
   
   (sc-rc *sc-rc2* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  13  :data 0))

   (or-sr *or-sr* (verb1 :position 7 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  11  :data 0))
  
   (dor *dor* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  11  :data 0)))
  )



(define-experiment SSU
  :full-name "Untensed subject sentence"
  :plot-data no
  :conditions
  ((ss-untensed  *ssu* (verb1 :position 7 :data 0)
	     (verb2  :position 8  :data 0 )
	     (verb3  :position  11  :data 0))))


(define-experiment SST
  :full-name "Tensed subject sentence"
  :plot-data no
  :conditions
   ((ss-tensed  *sst* (verb1 :position 7 :data 0)
	 (verb2  :position 8  :data 0 )
	 (verb3  :position  11  :data 0))))


(define-experiment SC-RC
  :full-name "SC-RC"
  :plot-data no
  :conditions
   ((sc-rc *sc-rc2* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  13  :data 0))))




(define-experiment RC-SC
  :full-name "RC-SC"
  :plot-data no
  :conditions
   ((rc-sc *rc-sc2* (verb1 :position 9 :data 0)
	   (verb2  :position 12  :data 0)
	   (verb3  :position  13  :data 0))))



(define-experiment OR-SR
  :full-name "Object relative/subject relative"
  :plot-data no
  :conditions
   ((or-sr *or-sr* (verb1 :position 7 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  11  :data 0))))


(define-experiment DOR
  :full-name "Double object relative"
  :plot-data no
  :conditions
   ((dor *dor* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  11  :data 0))))



(define-experiment recency
  :full-name "Consecutive PP attachments to NPs"
  :plot-data no
  :conditions
  ((recency    *recency* (pp1 :position 3 :data 0)
              	       (pp2 :position 6 :data 0)
              	       (pp3 :position 9 :data 0)
              	       (pp4 :position 12 :data 0)
              	       (pp5 :position 15 :data 0)
              	       (pp6 :position 18 :data 0)
              	       (pp7 :position 21 :data 0)))
)



(define-experiment deep-rb
  :full-name "Deep clausal right branching"
  :plot-data no
  :conditions
  ((deep-rb  *deep-rb* (verb1 :position 7 :data 0)
              	       (verb2 :position 11 :data 0)
              	       (verb3 :position 15 :data 0)
              	       (verb4 :position 19 :data 0)))
)


(define-experiment SCRC-mix
  :full-name "Mixes of SC and RCs"
  :plot-data no
  :conditions
  ((sc-rc *sc-rc2* (verb1 :position 9 :data 0)
	   (verb2  :position 10  :data 0)
	   (verb3  :position  13  :data 0))
   (rc-sc *rc-sc2* (verb1 :position 9 :data 0)
	  (verb2  :position 12  :data 0)
	  (verb3  :position  13  :data 0))
   (sc-sc *sc-sc* (verb1 :position 9 :data 0)
	  (verb2  :position 12  :data 0)
	  (verb3  :position  15  :data 0)))
  )


(define-experiment TEST
  :full-name "TESTEXP"
  :plot-data yes
  :conditions
   ((a *simple* (subj :position 2 :data 0.100)
     (verb  :position 3  :data 0.120)
     (obj  :position  4  :data 0.118))
    (b *simple* (subj :position 2 :data 0.100)
     (verb  :position 3  :data 0.120)
     (obj  :position  4  :data 0.118))))


(setf *pspace* '((:bll 0.3 0.7 .1) (:ga 0.8 1.8 .3)
		 (:lf .2 .4 .1)    (:mas 2 3.5 .5)))

(setf *pspace1* '(
     (:lf .2 .4 .1)))





(define-experiment STAUB10
  :full-name "Staub (2010) Exp."
  :conditions
  ((src   *staub-sr*    ())
   (orc   *staub-or*    ())
   )
  )
  
  
  
; '(:VISUAL-ENCODING-FACTOR 0.002 :SACCADE-PREPARATION-TIME 0.110 :LF 0.2)  
  
  

