; ;; ===================================================================
; ;; (1) ATTEND to sentence one word at a time
; ;;    First, find location of word to attend to (there is a separate
; ;;      production for the first word), and then attend to it.
; ;;    NOTE: The default value for slot goal-cat in the comprehend-sentence
; ;;      chunk-type is IP-gapped-goal (for German) or IP-goal (for English)


; (p start-reading
;    =goal>
;       ISA           comprehend-sentence
;       state         NIL
; ==>
;    !bind! =ID-IP (new-name IP)
;    =goal>
;       state          "start"
; ;      goal-cat      IP-goal
;       goal-cat      IP-gapped-goal
;    +IPb>
;       ISA                 syn-obj
;       cat                 IP
;       ID                  =ID-IP
;       waiting-for-cat     wait-for-IP
; ;      waiting-for-cat     wait-for-DP
;       waiting-for-finite  wait-for-finite
;       ;finite              finite
;       gap                 gapped
;       spec                NIL
;       comp                NIL
;       next-goal           next-*done*
; )

;;;
;;; START READING
;;;
(p start-reading
   =goal>
      ISA               comprehend-sentence
      state             NIL
   =visual-location>
      ISA               visual-location
==>
   !bind! =ID-IP (new-name IP)
   =goal>
      state             "start"
      ; goal-cat          IP-goal
      goal-cat          IP-gapped-goal
   +imaginal>
      ISA                 parsing-state
   +IPb>
      ISA                 syn-obj
      cat                 IP
      ID                  =ID-IP
      waiting-for-cat     wait-for-IP
;      waiting-for-cat     wait-for-DP
      waiting-for-finite  wait-for-finite
      ;finite              finite
      gapped              gapped
      gap                 gapped
      embedded            not-embedded
      spec                NIL
      comp                NIL
      location            =visual-location
      next-goal           next-*done*
   =visual-location>
)





; (p find-first-word
;    =goal>
;       ISA         	comprehend-sentence
;       state       	NIL
; ==>
;    !bind! =ID-IP (new-name IP)
;    =goal>
;       state       	"looking"
;    +visual-location>
;       ISA         	visual-location
;       screen-x    	lowest
;       :attended    	NIL
;    +IPb>
;       ISA                 syn-obj
;       cat                 IP
;       ID                  =ID-IP
;       waiting-for-cat     wait-for-IP
;       waiting-for-finite  wait-for-finite
;       ;finite              finite
;       gap                 gapped
;       spec                NIL
;       comp                NIL
;       next-goal           next-*done*
; ;; generic subject word
; ;      subj-word           "SUBJECT"
; )




; ;; (.6)  Find location of next word when the current word is integrated.
; ;;      More specifically, get the nearest visual location to
; ;;      the right, and direct attention to that location.

; (p find-location-of-next-word
;    =goal>
;       ISA         	comprehend-sentence
;       state       	"read"
; ;; New ACT-R 6 syntax
; ;    =visual-state>
; ;      ISA               module-state
; ;      modality          free
;    ?visual>
;       state             free
; ==>
;    +visual-location>
;       ISA         	visual-location
; ;      screen-x          greater-than-current
;     > screen-x          current
;       screen-x          lowest
; ;      :nearest           current
;    =goal>
;       state       	"looking"

;    !eval! (set-end-time)
; )



; ;; (2)(.7) ATTEND to the next word when a visual location is found.

; (p attend-to-next-word
;     =goal>
;        ISA         comprehend-sentence
;        state       "looking"
;     =visual-location>
;        ISA         visual-location
; ;; adjusted to act-r 6:
; ;    =visual-state>
; ;       ISA         module-state
; ;       modality    free
;     ?visual>
;        execution       free   ; use this for "saccadic suppression"
; ;       processor       free
; ==>
;     !eval! (set-current-ip)
;     =goal>
;        state       "attending"
;     +visual>
; ;       ISA         visual-object
;        ISA         move-attention
;        screen-pos  =visual-location
;     -IPb>
;     -CPb>
;     -DPb>
;     -NPb>
;     -VPb>
;     -VP2b>
;     -PPb>
;     -AdjPb>
;     -AdvPb>
;     -retrieval>
;    =visual-location>
; ;    !eval! (delete-bufferchunk-ancestors-retrieval)
; )
; (spp attend-to-next-word :at 0)

; (p stop-marker
;     =goal>
;        ISA         comprehend-sentence
;        state	   "attending"
;     =visual>
;        ISA         text
;        value       "*"
; ==>
;     =goal>
;        state       "stop"
;        !eval! (word-message "*")
; )
   
; (spp stop-marker :u 10)

; ;; (3)(.8) RETRIEVE lexical entry from memory

; (p lexical-retrieval-request
;     =goal>
;        ISA         comprehend-sentence
;        state	   "attending"
;     =visual>
;        ISA         text
;        value       =word
;      - value       "*"
; ==>
;     =goal>
;        state       "lexical-retrieval"
;        cue1        =word
;        cue2        nil
;        cue3        nil
;        cue4        nil
;     -visual-location>
; ;    +lex-retrieval>
;     +retrieval>
;        ISA         lexical-entry
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
;        word        =word
        
;        !eval! (word-message =word)
; )

;; ===========================================================================
;; Set WORKING MEMORY RETRIEVAL CUES
;;    Once a lexical entry has been retrieved, use features of the lexical
;;    item, plus current goal-category, to set retrieval cues for previous
;;    constituents that can be integrated with the current word.  There are
;;    different productions for different syntactic categories.  Currently
;;    the retrieval cues are syntactic features.
;;
;;    These productions create a syntactic node which is initially inactive
;;    because the features are lexical (-lex).  These nodes will not be
;;    used if they are later matched to an expectation.  But they may be
;;    used if they are attached; in that case the lexical features are
;;    converted to working memory features so they can be retrieved later
;;    in the parse. Else, they remain long-term lexical features (-lex) and
;;    thus do not contribute associative interference.


;; D E T E R M I N E R S

;; (4) When goal category is IP with a gap in SpecIP (main clauses):

(p set-retrieval-cues-IP-gapped-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-gapped-goal
;   =lex-retrieval>
   =retrieval>
      ISA 	  	lexical-entry
      cat         	DET
      word              =word
      polarity          =polarity  ;TEST! Possible, no -lex mappings anymore!
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      goal-cat          DP-goal     ;; used to be left out
      cue1              wait-for-IP ;; used to be: wait-for-IP
   +retrieval>                            
      ISA           	syn-obj
      cat               IP
      waiting-for-cat   wait-for-IP
      gap               gapped
;      polarity          =polarity ;TEST!

   !eval! (set-begin-time =word)
)


;; When goal category is IP :

(p set-retrieval-cues-IP-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      ISA 	  	lexical-entry
      cat         	DET
      word              =word
      polarity          =polarity ;TEST!
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      goal-cat          DP-goal     ;; used to be left out
      cue1              wait-for-IP ;; used to be: wait-for-IP
   +retrieval>                            
      ISA           	syn-obj
      cat               IP
      waiting-for-cat   wait-for-IP
      ;;waiting-for-cat   wait-for-VP
      gap               none
;; does not work
;      polarity          =polarity ;TEST!
   
   !eval! (set-begin-time =word)
)


;; (.9) N O U N S

(p set-retrieval-cues-goal-NP-input-N
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          NP-goal
   =retrieval>
      ISA 	  	lexical-entry
      cat         	N
      word              =word
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-NP
   +retrieval>                           
      ISA           	syn-obj
      waiting-for-cat   wait-for-NP

   !eval! (set-begin-time =word)
)


;; A D J E C T I V E S

;(p set-retrieval-cues-goal-XP-input-Adj
;   =goal>
;      ISA               comprehend-sentence
;      state             "lexical-retrieval"
;      goal-cat          XP-goal
;   =lex-retrieval>
;      ISA 	  	lexical-entry
;      word              =word
;      cat         	ADJ
;==>
;   +lex>
;      ISA               lexical-entry
;      =lex-retrieval
;   =goal>
;      state             "wm-retrieval"
;      ;cue1              wait-for-AdjP
;   +retrieval>                            
;      ISA        	  syn-obj
;      waiting-for-cat   wait-for-IP
;      waiting-for-cat2  wait-for-AdjP
;
;   !eval! (set-begin-time =word)
;)

(p set-retrieval-cues-goal-XP-input-Adj-modified
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	ADJ
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              wait-for-AdjP
   +retrieval>                            
      ISA        	  syn-obj
      cat                 VP
      gap                 gapped

   !eval! (set-begin-time =word)
)





;; V E R B S

(p set-retrieval-cues-goal-IP-gapped-input-I-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-gapped-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	I-V
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              wait-for-VP
      ;cue2              wait-for-finite
   +retrieval>                           
      ISA           	  syn-obj
      waiting-for-cat     wait-for-IP
      waiting-for-finite  wait-for-finite
      gap                 gapped

   !eval! (set-begin-time =word)
)




(p set-retrieval-cues-goal-IP-input-I-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	I-V
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
   +retrieval>                            
      ISA           	  syn-obj
      cat                 IP
      ;;waiting-for-cat     wait-for-VP
      waiting-for-cat     wait-for-IP
      gap                 none
 
   !eval! (set-begin-time =word)
)

(p set-retrieval-cues-goal-IP-input-I-V-preceding-perfect-participle
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	I-V
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
   +retrieval>                            
      ISA           	  syn-obj
      cat                 IP
;      comp                +VPb
      waiting-for-cat     wait-for-IP
      gap                 none
 
   !eval! (set-begin-time =word)
)

(p set-retrieval-cues-goal-IP-input-V-perfect-participle ;; Partizip 2
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	V
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      goal-cat          VP-goal
   +retrieval>                            
      ISA           	  syn-obj
      cat                 IP
      waiting-for-cat     wait-for-IP
      waiting-for-finite  wait-for-finite
      gap                 none
 
   !eval! (set-begin-time =word)
)



;; A D V E R B S (modifying an adjective phrase) 

;; Simple adverbs as 'immer' (neither npi, nor ppi) 

#|
(P set-retrieval-cues-goal-XP-input-Adv
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =lex-retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	ADV
      polarity         zero
==>
   +lex>
      ISA               lexical-entry
      =lex-retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              wait-for-AdjP

   +retrieval>                            
      ISA           	  syn-obj
      waiting-for-cat   wait-for-IP 
      waiting-for-cat2  wait-for-AdjP

   !eval! (set-begin-time =word)
)
|#



;; In the following, the retrieval cues for the NPI/PPI adverbs are set. 
;;
;; NPI-adverbs licensed by a negative determiner

(P set-retrieval-cues-goal-XP-input-NPI-Adv
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	ADV
      polarity          negative
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              wait-for-AdjP
   +retrieval>                            
      ISA           	syn-obj
      cat               DP 
      waiting-for-cat   wait-for-IP 
      polarity          negative
      case              nom

   !eval! (set-begin-time =word)
)


;; PPI-adverbs licensed by a positive determiner

(P set-retrieval-cues-goal-XP-input-PPI-Adv
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat         	ADV
      polarity          positive
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              wait-for-AdjP
   +retrieval>                            
      ISA           	syn-obj
      cat               DP 
      waiting-for-cat   wait-for-IP 
      polarity          positive
      case              nom

   !eval! (set-begin-time =word)
)






;; R E L A T I V E   P R O N O U N S

;; WH-words

(P set-retrieval-cues-input-wh-pronoun
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
   ;-  goal-cat          IP-goal
   ;-  goal-cat          IP-embedded-goal
   ;-  goal-cat          IP-gapped-goal
      goal-cat          =goal-cat
   =retrieval>
      ISA 	  	lexical-entry
      word              =word
      cat               wh-pronoun 
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      ;cue1              DP
   +retrieval>                            
      ISA           	syn-obj
      cat               DP

   !eval! (set-begin-time =word)
)

;(spp Set-Retrieval-Cues-Input-Wh-Pronoun :c .1)
(spp Set-Retrieval-Cues-Input-Wh-Pronoun :at .1)




;; ============================================================================
;; ATTACHMENTS
;;    Once a prior constituent has been retrieved, attachments can be
;;    made between the current word and the retrieved constituent.


;; (5) A T T A C H I N G   S U B J E C T   D P

(p attach-DP-as-subject-of-predicted-gapped-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      cat		IP
      ID                =ID-RETR
      head              NIL
      gap               gapped 
      waiting-for-cat 	wait-for-IP
;      case              =case
   =lex>
      ISA		lexical-entry
      cat		DET
      word              =word
      polarity          positive
      case		nom-lex
==>
   !bind! =ID-DP (new-name DP)
   !bind! =ID-CP (new-name CP)

   =goal>
      state             "read"
      goal-cat          NP-goal
      ;polarity          positive ;TEST!
   +DPb>
      ISA               syn-obj
      cat               DP
      ID                =ID-DP
      head              =word
      case		nom
      polarity          positive
      spec-of           =ID-RETR
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP 
      ;next-goal         next-VP-goal

;;to simplify matters the matrix CP is added and both, IP and CP bear the DP chunk in the specifier position
   +CPb>
      ISA               syn-obj
; FX added CAT:
      cat               CP
      ID                =ID-CP
      spec              =ID-DP        
      comp              =ID-RETR
      modif-of          none    
   =retrieval>
      spec		=ID-DP

  !eval! (set-end-time)
)


;; A T T A C H I N G   N E G A T E D   S U B J E C T   D P

(p attach-negated-DP-as-subject-of-predicted-gapped-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA               syn-obj
      cat               IP
      ID                =ID-IP
      head              NIL
      gap               gapped 
      waiting-for-cat   wait-for-IP
   =lex>
      ISA               lexical-entry
      cat               DET
      word              =word
      polarity          negative
      case              nom-lex
==>
   !bind! =ID-DP (new-name DP)
   !bind! =ID-CP (new-name CP)

   =goal>
      state             "read"
      goal-cat          NP-goal
      ;polarity          negative ;TEST!
   +DPb>
      ISA               syn-obj
      cat               DP
      ID                ID-DP
      head              =word
      case              nom
      polarity          negative
      spec-of           =ID-IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
      ;next-goal         next-VP-goal

;;to simplify matters the matrix CP is added and both, IP and CP bear the DP chunk in the specifier position
   +CPb>
      ISA               syn-obj
; FX added CAT:
      cat               CP
      spec              =ID-DP        
      comp              =ID-IP
      modif-of          none
   =retrieval>
      spec              =ID-DP

  !eval! (set-end-time)
)


;; A T T A C H I N G   E M B E D D E D   O B J E C T   D P

(p attach-DP-as-object-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      ;goal-cat          =goal-cat
   =retrieval>
      ISA 	  	syn-obj
      cat		IP
      head              NIL
      gap               none
      ;waiting-for-cat 	wait-for-VP
      waiting-for-cat 	wait-for-IP
   =lex>
      ISA		lexical-entry
      cat		DET
      word              =word
      polarity          positive
      case		acc-lex
==>
   !bind! =ID-DP (new-name DP)

   =goal>
      state             "read"
      goal-cat          NP-goal
   +DPb>
      ISA               syn-obj
      cat               DP
      ID                ID-DP
      head              =word
      case		acc
      polarity          positive
      ;embedded          embedded
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP 
      ;next-goal         next-VP-goal
   =retrieval>
;      ISA 	  	syn-obj
      comp		=ID-DP
      ;waiting-for-cat   wait-for-VP
      waiting-for-cat   wait-for-IP

  !eval! (set-end-time)
)



;; A T T A C H I N G    E M B E D D E D   N E G A T E D   O B J E C T   D P

(p attach-negated-DP-as-object-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      ;goal-cat          =goal-cat
   =retrieval>
      ISA 	  	syn-obj
      cat		IP
      head              NIL
      gap               none 
      ;waiting-for-cat 	wait-for-VP
      waiting-for-cat 	wait-for-IP
   =lex>
      ISA		lexical-entry
      cat		DET
      word              =word
      polarity          negative
      case		acc-lex
==>
   !bind! =ID-DP (new-name DP)

   =goal>
      state             "read"
      goal-cat          NP-goal
   +DPb>
      ISA               syn-obj
      cat               DP
      ID                =ID-DP
      head              =word
      case		acc
      polarity          negative 
      ;embedded          embedded
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP 
     ; next-goal         next-VP-goal
   =retrieval>
;      ISA 	  	syn-obj
      comp		=ID-DP
      ;waiting-for-cat   wait-for-VP
      waiting-for-cat   wait-for-IP

  !eval! (set-end-time)
)





;; (.10) A T T A C H I N G   S U B J E C T   N O U N

(p attach-initial-subject-NP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      ISA 	  	syn-obj
      cat         	DP
      ID                =ID-DP
      case        	nom
      ;next-goal         next-VP-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      ISA               lexical-entry
      cat               N
      word              =word
      case        	nom-lex
==>
   !bind! =ID-NP (new-name NP)

   =goal>
      state             "read"
;      state             "add-subjWord"
      goal-cat          IP-gapped-goal
   +NPb>
      ISA               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      case              nom
      spec-of           =IP 
      comp-of           =ID-DP 
  =retrieval>
;      ISA 	  	syn-obj
      head-word         =word
      comp              =ID-NP
      waiting-for-case	NIL
      waiting-for-cat	wait-for-IP

;   =lex>
;   =IP>                                       ;; HACK: this may not be legal
;      ISA               syn-obj
;      subj-word         =word
;      gap               gapped
; !eval! (set-chunk-slot-value syn-obj0-0 SUBJ-WORD =word)
; !eval! (set-chunk-slot-value syn-obj0-0 GAP gapped)
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'subj-word =word
     'gap 'gapped
     ))

  !eval! (set-end-time)
)


;; A T T A C H I N G   O B J E C T   N O U N

(p attach-object-NP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      ISA 	  	syn-obj
      cat         	DP
      ID                =ID-DP
      case        	acc
      ;next-goal         next-VP-goal
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP
   =lex>
      ISA               lexical-entry
      cat               N
      word              =word
      case        	acc-lex
==>
   !bind! =ID-NP (new-name NP)

   =goal>
      state             "read"
      goal-cat          IP-goal
   +NPb>
      ISA               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      case              acc
      comp-of           =ID-DP
  =retrieval>
;      ISA 	  	syn-obj
      head-word         =word
      comp              =ID-NP      
      waiting-for-case	NIL
      waiting-for-cat	wait-for-IP

  !eval! (set-end-time)
)



;; A T T A C H I N G   C P   M O D I F I E R

;; Subject relative clause as modifier of subject NP

(P attach-CP-as-SR-modifier-of-retrieved-singular-NP
   =goal>
      ISA		comprehend-sentence
      state             "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      ISA 	  	syn-obj
      cat         	DP
      ID                =ID-RETR
      case              nom
      head-word         =subj-word 
      spec-of           =IP
   =lex>
      ISA    		lexical-entry
      cat         	wh-pronoun
      word              =word
      case              nom-lex
==>
   !bind! =ID-DP (new-name DP)
   !bind! =ID-IP (new-name IP)
   !bind! =ID-CP (new-name CP)

   =goal>
      state             "read"
      goal-cat          IP-goal
   +DPb>
      ISA		syn-obj
      cat		DP
      ID                =ID-DP
      head-word         =word     
      case		nom
      coindexed-with	=ID-RETR
      subj-predicate    =IP
   +IPb>
      ISA		syn-obj
      cat		IP
      ID                =ID-IP
      waiting-for-cat	wait-for-IP       
      ;waiting-for-cat	wait-for-VP       
      waiting-for-finite  wait-for-finite
      spec              =ID-DP
      comp              =IP
      ;finite            finite
      ;embedded          embedded
      gap               none 
      subj-word         =subj-word
      subj-predicate    =IP
;; does this work?
      next-goal         =goal-cat

;   !eval! (set-next-goal-category *IP* =goal-cat)

   +CPb>
      ISA               syn-obj
      cat               CP
      ID                =ID-CP
      modif-of		=ID-RETR
      spec              =ID-DP
      comp              =ID-IP
;; does this work?
      next-goal         =goal-cat

;  !eval! (set-next-goal-category *CP* =goal-cat)

=retrieval>
;      ISA          syn-obj
      modifier     =ID-CP

  !eval! (attach-message =subj-word "modifier" =word)  
;  !eval! (set-begin-time =word)
; !eval! (add-reference (wme-references (get-wme =IP)))
; !eval! (boost-message =IP)

  !eval! (set-end-time)
)






;; A T T A C H I N G   A U X   V E R B   O F   G A P P E D   I P   (IN MAIN CLAUSE)

(p attach-aux-verb-gap
   =goal>
      	ISA               	comprehend-sentence
        state                   "wm-retrieval"
        goal-cat                IP-gapped-goal 
   =retrieval>
      	ISA 		  	syn-obj
	waiting-for-cat         wait-for-IP 
	waiting-for-finite      wait-for-finite
        gap                     gapped       
	subj-word               =subj-word
   =lex>
        ISA                 lexical-entry
	word                =word
	cat       	    I-V
==>
   !bind! =ID-VP (new-name VP)

   =goal>
        state               "read"
	goal-cat            XP-goal  
   +VPb>
      ISA                   syn-obj
      cat                   VP
      ID                    =ID-VP
      head                  =word
      subj-word             =subj-word
      gap                   gapped       
   =retrieval>
;      ISA                  syn-obj
	head               =word
	gap                gapped
        comp               =ID-VP
	;;waiting-for-cat    wait-for-VP
	waiting-for-cat    wait-for-IP
        ;waiting-for-cat2   wait-for-AdjP
	;waiting-for-cat3   wait-for-PP
	;;waiting-for-finite wait-for-finite

 !eval! (attach-message =word "subject" =subj-word)

  !eval! (set-end-time)
)



;; A T T A C H I N G   A U X   V E R B   (IN EMBEDDED CLAUSE)

(p attach-aux-verb-of-verb-in-pqp-no-gap  ;; attaches finite form of sein/haben for verbs in pqp, participle preceding
   =goal>
      	ISA                comprehend-sentence
        state              "wm-retrieval"
	goal-cat           VP-goal
   =retrieval>
      	ISA 		   syn-obj
      	cat         	   IP
	waiting-for-cat    wait-for-IP
        waiting-for-finite wait-for-finite
	next-goal          =next-goal
	gap                none
	subj-word          =subj-word
   =lex>
        ISA               lexical-entry
	word              =word
	cat               I-V
==>

   =goal>
        state            "read"
        goal-cat         IP-gapped-goal 

=retrieval>
;      ISA                syn-obj
;; where does that VP come from??
      comp               +VPb
      waiting-for-cat    wait-for-IP
      waiting-for-finite nil
      next-goal          =next-goal

 !eval! (attach-message =word "subject" =subj-word)

  !eval! (set-end-time)
)


(p attach-aux-verb-no-gap 
   =goal>
      	ISA                comprehend-sentence
        state              "wm-retrieval"
	goal-cat           IP-goal
   =retrieval>
      	ISA 		   syn-obj
      	cat         	   IP
        head-word          nil
	waiting-for-cat    wait-for-IP
        waiting-for-finite wait-for-finite
	next-goal          =next-goal
	gap                none
	subj-word          =subj-word
   =lex>
        ISA               lexical-entry
	word              =word
	cat               I-V
==>
   !bind! =ID-VP (new-name VP)

   =goal>
        state            "read"
        goal-cat         IP-gapped-goal 
   +VPb>
      ISA               syn-obj
      cat               VP
      ID                =ID-VP
      head              =word
;; where does that DP come from??
      comp              +DPb                    ;; accusative object
      subj-word         =subj-word
      gap               none

   =retrieval>
;      ISA                syn-obj
      head               =word
      comp               =ID-VP
      waiting-for-cat    wait-for-IP
      waiting-for-finite nil
      next-goal          =next-goal
      subj-word          =subj-word

 !eval! (attach-message =word "subject" =subj-word)

  !eval! (set-end-time)
)

;; A T T A C H I N G    V E R B   (IN EMBEDDED CLAUSE, PERFECT)

(p attach-perfect-participle-of-verb-in-pqp-no-gap
   =goal>
      	ISA                comprehend-sentence
        state              "wm-retrieval"
	goal-cat           VP-goal 
   =retrieval>
      	ISA 		   syn-obj
      	cat         	   IP
	waiting-for-cat    wait-for-IP
        waiting-for-finite wait-for-finite
	next-goal          =next-goal
	gap                none
	subj-word          =subj-word
   =lex>
        ISA               lexical-entry
	word              =word
	cat               V
==>
   !bind! =ID-VP (new-name VP)

   =goal>
        state            "read"
+VPb>
      ISA               syn-obj
      cat               VP
      ID                =ID-VP
      head              =word
;; where does the DP come from??
      comp              +DPb                    ;; accusative object
      subj-word         =subj-word

=retrieval>
;      ISA                syn-obj
      head-word          =word
      comp               =ID-VP
      waiting-for-cat    wait-for-IP
      waiting-for-finite wait-for-finite 
      next-goal          =next-goal
      subj-word          =subj-word

 !eval! (attach-message =word "subject" =subj-word)

  !eval! (set-end-time)
)





;; A T T A C H I N G   P R E D I C A T E   A D J E C T I V E

#|
(p attach-predicate-adjective
   =goal>
       ISA                comprehend-sentence
       state              "wm-retrieval"
       ;cue1               wait-for-AdjP
   =retrieval>
       ISA                syn-obj
      waiting-for-cat   wait-for-IP
       waiting-for-cat2   wait-for-AdjP
   =lex>
       ISA                lexical-entry
       cat                ADJ
       word               =word
 ==>
   =goal>
       state              "read"
   +AdjPb>
       ISA                syn-obj
       cat                AdjP
       head               =word
   =retrieval>
      ISA 	  	syn-obj
      waiting-for-cat   wait-for-IP
      waiting-for-cat2  wait-for-Adj
 )
|#

(p attach-modified-predicate-adjective
   =goal>
       ISA                comprehend-sentence
       state              "wm-retrieval"
       ;cue1               wait-for-AdjP
   =retrieval>
       ISA                syn-obj
       cat                VP
       ;waiting-for-cat2   wait-for-AdjP
   =lex>
       ISA                lexical-entry
       cat                ADJ
       word               =word
 ==>
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
       state              "read"
   +AdjPb>
       ISA                syn-obj
       cat                AdjP
       ID                 =ID-AdjP
       head               =word
;; where does that AdvP come from
       spec               +AdvPb
   =retrieval>
;      ISA 	  	syn-obj
      comp              =ID-AdjP

  !eval! (set-end-time)
)





;; A T T A C H I N G   A D V E R B

(P attach-adv-as-modifier-of-predicted-predicate-AdjP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      ;waiting-for-cat   wait-for-VP
      waiting-for-cat2	wait-for-AdjP
   =lex>
      ISA		lexical-entry
      cat		ADV
      word              =word
      polarity         zero
==>
   !bind! =ID-AdvP (new-name AdvP)
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
      goal-cat           XP-goal
   +AdvPb>
      ISA               syn-obj
      cat               AdvP
      ID                =ID-AdvP
      polarity          zero
      head              =word
      modif-of		=ID-AdjP ;does not resolve
   +AdjPb>
      ISA               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      waiting-for-cat   wait-for-AdjP
      modifier          =ID-AdvP
   =retrieval>
      waiting-for-cat   wait-for-IP
      waiting-for-cat2	wait-for-AdjP

  !eval! (set-end-time)
)



;; A T T A C H I N G   N P I   A D V E R B

(P attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPnom
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      cat               DP
      case              nom
   =lex>
      ISA		lexical-entry
      cat		ADV
      word              =word
      polarity          negative
==>
   !bind! =ID-AdvP (new-name AdvP)
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
      goal-cat           XP-goal
   +AdvPb>
      ISA               syn-obj
      cat               AdvP
      ID                =ID-AdvP
      polarity          negative
      head              =word
      modif-of		=ID-AdjP
   +AdjPb>
      ISA               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      waiting-for-cat   wait-for-AdjP
      modifier          =ID-AdvP
   =retrieval>
      waiting-for-cat   wait-for-IP
      waiting-for-cat2	wait-for-AdjP

  !eval! (set-end-time)
)


(P attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPacc
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      cat               DP
      case              acc
   =lex>
      ISA		lexical-entry
      cat		ADV
      word              =word
      polarity          negative
==>
   !bind! =ID-AdvP (new-name AdvP)
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
      goal-cat           XP-goal
   +AdvPb>
      ISA               syn-obj
      cat               AdvP
      ID                =ID-AdvP
      polarity          negative
      head              =word
      modif-of		=ID-AdjP
   +AdjPb>
      ISA               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      waiting-for-cat   wait-for-AdjP
      modifier          =ID-AdvP
   =retrieval>
      waiting-for-cat   wait-for-IP
      waiting-for-cat2	wait-for-AdjP

  !eval! (set-end-time)
)


;; A T T A C H I N G    P P I   A D V E R B

(P attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPnom
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      cat               DP
      case              nom
   =lex>
      ISA		lexical-entry
      cat		ADV
      word              =word
      polarity          positive
==>
   !bind! =ID-AdvP (new-name AdvP)
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
      goal-cat           XP-goal
   +AdvPb>
      ISA               syn-obj
      cat               AdvP
      ID                =ID-AdvP
      polarity          positive
      head              =word
      modif-of		=ID-AdjP
   +AdjPb>
      ISA               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      waiting-for-cat   wait-for-AdjP
      modifier          =ID-AdvP
   =retrieval>
      waiting-for-cat   wait-for-IP
      waiting-for-cat2	wait-for-AdjP

  !eval! (set-end-time)
)

(P attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPacc
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      ISA 	  	syn-obj
      cat               DP
      case              acc
   =lex>
      ISA		lexical-entry
      cat		ADV
      word              =word
      polarity          positive
==>
   !bind! =ID-AdvP (new-name AdvP)
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
      goal-cat           XP-goal
   +AdvPb>
      ISA               syn-obj
      cat               AdvP
      ID                =ID-AdvP
      polarity          positive
      head              =word
      modif-of		=ID-AdjP
   +AdjPb>
      ISA               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      waiting-for-cat   wait-for-AdjP
      modifier          =ID-AdvP
   =retrieval>
      waiting-for-cat   wait-for-IP
      waiting-for-cat2	wait-for-AdjP

  !eval! (set-end-time)
)