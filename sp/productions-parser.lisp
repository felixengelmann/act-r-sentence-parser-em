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
;;; Filename    : productions.lisp
;;; Version     : 3.0
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;   - redo center-embedding runs
;;   - Monte Carlo simulations of quantatitive fit data sets
;;   - check lexical bias for categorial ambiguity ("duck")
;;   - review Gordon data
;;   - Hindi simulations
;;   - German simulations?
;;   - Latex figures
;;   - Latex tables
;;   - rework VD&L examples with new lexical access model
;;   - switch to multiple goal slots on goal?

;;   - [DONE] Return to split functional projections
;;   - [DONE] Propagate gap/slash category
;;   - [DONE] make sure "goal-cat" does not unfairly cue retrieval
;;   - [DONE] make sure "position" does not unfairly cue retrieval
;;   - [DONE] get SSR and SOR working, compare to each other and to LONG
;;   - [DONE] compare to DOR and DSR
;;   - [DONE] get verb SC working (RB's)
;;   - [DONE] get noun SC working
;;   - [DONE] make sure the next-goal slot does not cause interference
;;   - [DONE] SC/RCs
;;   - [DONE] restore serial position cues
;;   - [DONE] Van Dyke/Lewis unambiguous short/long/interfering
;;   - [DONE] Van Dyke/Lewis ambiguous short/long/interfering
;;   - [DONE] multiple retreival buffers with ordered access
;;   - [DONE] speed-up lexical access
;;   - [DONE] fit Grodner & Gibson structures
;;   - [DONE] check Gibson & Grodner paper for others? 
;;   - [no] make subcats create explicit expectations that get retrieved(?)
;;   - [DONE] McElree et al (2003) constructions
;;   - [DONE] review Konieczny data
;;   - [DONE] uniformly reintroduce retrievals to simulate semantic
;;     processing
;;   - [DONE] fix subject/object gap constructions
;;   - [DONE] see if dummy semantic interpretation works for VD&L
;;   - [DONE] remove spurious retrievals
;;   - [DONE] 3-NP recency preference
;;   - [DONE] decide on which examples to fit quantitatively
;;   - [DONE] elminate position coding; use "gap" and/or embedded features 
;;   - [DONE] add noise to test double center-embeddings
;;   - [DONE] Monte Carlo simulation support
;;   - [DONE] R graphs/fits
;;   - [DONE] review Vosse & Kempen Cognition article
;;   - [DONE] consider removing all "active" features
;;   - [DONE] make sure wait-for features are reset uniformly
;;   - [DONE] GGT experiment (from Gibson 2000 DLT chapter)
;;   - [DONE] collect qualitative contrast examples
;;      - [DONE] SC/RC vs. RC/SC
;;             - [DONE] SS and SS with relative
;;             - [DONE] For vs that-clause
;;             - [later] difficult garden path?
;;             - [DONE] review ACT-R workshop talk, Hopkins, CUNY talks for other
;;               examples 
;;             - [DONE] check Gibson DLT chapter for others
;;   - [DONE] Monte Carlo simulations of center-embeddings
;;   - [later--not this paper] Nakayama/Lewis Japanese data?
;;   - [TRIED--see abandoned-productions.actr] remove all "non-legal" chunk
;;     mods/matches, including retrieval of empty category, and IP
;;     retrieval at head subject noun 
;;   - [NO..not needed] VP-internal retrievals? (do retrieval for
;;          spec-of-IP for subject of verb?) 
;;   - [NO..no different than vp-internal] do filler retrieval for spec of
;;     verb in subject relative? 
;;   - [LATER MAYBE] do filler retrieval in parallel?
;;   - [DONE] remove as much "illegal" chunk modifications as possible
;;; 
;;;
;;; ----- History -----
;;; 2012/2013 Felix
;;;             : * productions adapted to new environment and parsing module.
;;;             : * basic attention-related productions now in 
;;;             :   productions-basic.lisp
;;;             : * Modifying current IP chunk now directly through a function
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
      goal-cat          IP-goal
      ; goal-cat          IP-gapped-goal
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
      ; gapped              gapped
      embedded            not-embedded
      spec                NIL
      comp                NIL
      location            =visual-location
      next-goal           next-*done*
   =visual-location>
)



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


;; DETERMINERS
;;    When goal category is IP

(P set-retrieval-cues-IP-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-IP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-IP

   !eval! (push-clause)
   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-IP-embedded-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-embedded-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-IP
      cue2              embedded
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-IP
      embedded          embedded

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



(P set-retrieval-cues-IP-gap-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-gapped-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-IP
      cue2              embedded
      cue3              gapped
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-IP
      embedded          embedded
      gapped            gapped

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)





;;    When goal category is DP

(P set-retrieval-cues-goal-DP-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          DP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-DP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-DP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-DP-input-DET2
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          CP-DP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-DP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-DP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



;;    This production is the first step in a reanalysing from a subject
;;    relative to object relative

(P set-retrieval-cues-goal-VP-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-VP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-embedded-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-embedded-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-VP
      embedded          embedded

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-gapped-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-gapped-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
      cue3              gapped
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-VP
      embedded          embedded
      gapped            gapped

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


;; NOUNS

(P set-retrieval-cues-goal-NP-input-N
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            N
      number           sing-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-NP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-NP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


;; VERBS

(P set-retrieval-cues-goal-VP-input-V-finite
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      number           sing-plural-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
;      cue2              nil
      cue2              wait-for-finite
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite
   ;; FE: gap none added, otherwise wrong embedded ip retrieved
      gap               none        

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-input-I
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            I-V
      number           sing-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
;      cue2              nil
      cue2              wait-for-finite
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite
   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-embedded-input-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-embedded-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      number           sing-plural-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
      cue3              wait-for-finite
;      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      embedded            embedded
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)

(P set-retrieval-cues-goal-VP-gapped-input-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-gapped-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      number           sing-plural-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
      cue3              gapped
;      cue4              nil
      cue4              wait-for-finite
   +retrieval>
      ISA              syn-obj
      embedded            embedded
      gapped              gapped
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-embedded-input-I
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-embedded-goal   ;; ???
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            I-V          ;; ???
      number           sing-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
      cue3              wait-for-finite
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite
      embedded            embedded
   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)

(P set-retrieval-cues-goal-VP-gapped-input-I
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-gapped-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            I-V
      number           sing-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              embedded
;      cue3              nil
      cue3              wait-for-finite
      cue4              gapped
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite
      embedded            embedded
      gapped              gapped
   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-VP-input-V-infinite
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          VP-embedded-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      finite            infinite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              wait-for-infinite
      cue3              embedded
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-infinite
      embedded            embedded

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



(P set-retrieval-cues-goal-XP-input-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      finite            infinite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-XP-input-finite-V
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            V
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-VP
;      cue2              nil
      cue2              wait-for-finite
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat     wait-for-VP
      waiting-for-finite  wait-for-finite

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)




;; This is the first step toward the subject/object reanalysis

(P set-retrieval-cues-no-goal-input-I
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
   -  goal-cat          VP-goal
   -  goal-cat          VP-embedded-goal
   -  goal-cat          VP-gapped-goal
   -  goal-cat          XP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            I-V
      number           sing-lex
      finite            finite-lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      attend-to         "left"
      cue1              wait-for-IP
;      cue2              nil
      cue2              wait-for-finite
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat2    wait-for-IP
      waiting-for-finite  wait-for-finite

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



;; ADJECTIVE

(P set-retrieval-cues-goal-XP-input-Adj
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            ADJ
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-AdjP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA              syn-obj
      waiting-for-cat2    wait-for-AdjP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



(P set-retrieval-cues-input-PREP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
    - goal-cat          PP-goal
    - goal-cat          XP-goal
      goal-cat          =goal-cat
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            P
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              NP
      cue2              VP
      cue3              nil
      cue4              nil
   +retrieval>
      ISA               syn-obj
      cat               NP-VP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



(P set-retrieval-cues-goal-XP-input-PREP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          XP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            P
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-PP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat3  wait-for-PP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-PP-agent-phrase-input-PREP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          PP-goal
   =retrieval>
      isa      lexical-entry
      word              "by"
      cat            P
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-PP
      cue2              wait-for-agent-by-phrase
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-PP

   !eval! (set-begin-time 'by)
   !eval! (increase-ref-count-parseloc)
)



;; COMPLEMENTIZER

(P set-retrieval-cues-input-COMP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
   -  goal-cat          CP-goal
   -  goal-cat          IP-goal
   -  goal-cat          CP-DP-goal
      goal-cat          =goal-cat
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            C
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              DP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      cat               DP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-CP-input-COMP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          CP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            C
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-CP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-CP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


(P set-retrieval-cues-goal-CP-DP-input-COMP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          CP-DP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            C
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-CP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-CP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)



;; Below production is first step toward sentential subject

(P set-retrieval-cues-IP-goal-input-COMP
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            C
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-IP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      waiting-for-cat   wait-for-IP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)




;; WH-words

(P set-retrieval-cues-input-wh-pronoun
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
   -  goal-cat          IP-goal
   -  goal-cat          IP-embedded-goal
   -  goal-cat          IP-gapped-goal
      goal-cat          =goal-cat
   =retrieval>
      isa      lexical-entry
      word              =word
      cat            wh-pronoun
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              DP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            syn-obj
      cat               DP

   !eval! (set-begin-time =word)
   !eval! (increase-ref-count-parseloc)
)


;(spp Set-Retrieval-Cues-Input-Wh-Pronoun :c .1)
(spp Set-Retrieval-Cues-Input-Wh-Pronoun :at .1)














;; ============================================================================
;; ATTACHMENTS
;;    Once a prior constituent has been retrieved, attachments can be
;;    made between the current word and the retrieved constituent.


;; Determiner + retrieved predicted IP triggers attachment of DP


(P attach-DP-as-subject-of-predicted-embedded-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          IP-embedded-goal
   =retrieval>
      isa      syn-obj
      cat      IP
      ID                =ID-RETR
      head              nil
      waiting-for-cat   wait-for-IP
   =lex>
      isa      lexical-entry
      cat      DET
      word              =word
      case     all-cases-lex
      gender      all-genders-lex
      number      sing-plural-lex
==>
   !bind! =ID-DP (new-name DP)
   =goal>
      state             "read"
      goal-cat          NP-goal
   +DPb>
      isa               syn-obj
      cat               DP
      ID                =ID-DP
      head              =word
      case     nom
      spec-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
      next-goal         next-VP-embedded-goal
   =retrieval>
      number      sing-plural
      spec     =ID-DP
      waiting-for-cat   wait-for-VP
   ; -retrieval>

   !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; (P attach-DP-as-subject-of-predicted-IP-gapped
;;    =goal>
;;       ISA               comprehend-sentence
;;       state             "wm-retrieval"
;;       goal-cat          VP-gapped-goal
;;    =retrieval>
;;       isa      syn-obj
;;       cat      IP
;;       ID             =ID-RETR
;;       head              nil
;;       waiting-for-cat   wait-for-VP
;;       spec              =empty-op
;;    =lex>
;;       isa      lexical-entry
;;       cat      DET
;;       word              =word
;;       case     all-cases-lex
;;       gender      all-genders-lex
;;       number      sing-plural-lex
;; ==>
;;    =goal>
;;       state             "read"
;;       goal-cat          NP-goal
;;    =empty-op>                               ; may not be legal
;;       isa               syn-obj
;;       filler            yes-filler          ; reinstate filler as active   
;;    +DPb>
;;       isa               syn-obj
;;       cat               DP
;;       case     nom
;;       head              =word
;;       spec-of     =ID-RETR
;;       gender      all-genders
;;       number      sing-plural
;;       waiting-for-case  wait-for-nom
;;       waiting-for-cat   wait-for-NP
;;       next-goal         next-VP-gapped-goal
;;    =retrieval>
;;       number      sing-plural
;;       spec     +DPb
;;       gap               open
;;       waiting-for-cat   wait-for-VP
;;    -retrieval>
;;   !eval! (increase-ref-count-parseloc)
;; )



(P attach-DP-as-subject-of-predicted-IP-gapped
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          VP-gapped-goal
   =imaginal>
        ISA             parsing-state
        filler-pos      =fillerpos
   =retrieval>
      isa               syn-obj
      cat               IP
      head              nil
      subj-word         =subj-word
      waiting-for-cat   wait-for-VP
      spec              =empty-op
      gap               spec
   =lex>
      isa               lexical-entry
      cat               DET
      word              =word
      case              all-cases-lex
      gender            all-genders-lex
      number            sing-plural-lex
==>
   !bind! =ID-DP (new-name DP)
   =goal>
      state             "wm-retrieval"
      cue1              =empty-op
      ; attend-to         =fillerpos
      attend-to         "left"
   =imaginal>
      ; word              =subj-word
   +DPb>
      isa               syn-obj
      cat               DP
      ID                =ID-DP
      case              nom
      head              =word
      spec-of           =retrieval
      gender            all-genders
      number            sing-plural
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
      next-goal         next-VP-gapped-goal
   =retrieval>
      number            sing-plural
      spec              =ID-DP
      gap               open
   ; +retrieval>          =empty-op   ;;; FE: problem?
   +retrieval>
      ISA               syn-obj
      CAT               DP ;
      ID                =empty-op
      filler            DONE ;

   !eval! (increase-ref-count-parseloc)
)



(P revise-subject-relative
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          VP-gapped-goal
   =retrieval>
      isa               syn-obj
      filler            done
==>
   =goal>
      state             "read"
      goal-cat          NP-goal
      attend-to         "next-word"
   =retrieval>
      filler            yes-filler          ;reinstate filler as active   
  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-DP-as-subject-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      cat      IP
      ID                =ID-RETR
      head              nil
      waiting-for-cat   wait-for-IP
   =lex>
      isa      lexical-entry
      cat      DET
      word              =word
      case     all-cases-lex
      gender      all-genders-lex
      number      sing-plural-lex
==>
   !bind! =ID-DP (new-name DP)
   =goal>
      state             "read"
      goal-cat          NP-goal
   +DPb>
      isa               syn-obj
      cat               DP
      ID                =ID-DP
      head              =word
      case     nom
      spec-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
      next-goal         next-VP-goal
   =retrieval>
      number      sing-plural
      spec     =ID-DP
      waiting-for-cat   wait-for-VP
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Determiner + retrieved synobj looking for an accusative DP triggers the
;; attachment of DP.

(P attach-DP-as-object
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal         =next-goal
      head              =head-word
   =lex>
      isa      lexical-entry
      cat      DET
      word              =word
      case     all-cases-lex
      gender      all-genders-lex
      number      sing-plural-lex
==>
   !bind! =ID-DP (new-name DP)

   =goal>
      state             "read"
      goal-cat          NP-goal
   +DPb>
      isa               syn-obj
      cat               DP
      ID                =ID-DP
      head              =word
      case     acc
      comp-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP
      next-goal         =next-goal
      head-word         =head-word
   =retrieval>
      waiting-for-case  nil
      waiting-for-cat   nil
      comp              =ID-DP
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-AdjP-as-object
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
;      goal-cat          XP-goal
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      waiting-for-cat2  wait-for-AdjP
      next-goal         =next-goal
      head              =head-word
   =lex>
      isa      lexical-entry
      cat      Adj
      word              =word
;      case    all-cases-lex
;      gender     all-genders-lex
;      number     sing-plural-lex
==>
   !bind! =ID-AdjP (new-name AdjP)

   =goal>
      state             "read"
       goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)

   +AdjPb>
      isa               syn-obj
      cat               AdjP
      ID                =ID-AdjP
      head              =word
;      case    acc
      comp-of     =ID-RETR
   =retrieval>
      waiting-for-case  nil
      waiting-for-cat   nil
      waiting-for-cat2  nil
      waiting-for-cat3  nil
      waiting-for-finite nil              ;;; PROBLEM HERE???
      comp              =ID-AdjP
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



;; Attaching subject nouns

(P attach-subject-NP-singular-any-case
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            intransitive-lex
==>
   !bind! =ID-NP (new-name NP)

   =goal>
      state             "read"
      goal-cat          VP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))
  
  !eval! (set-end-time)
  !eval! (increase-ref-count-parseloc)
)


(P attach-subject-NP-singular-any-case-masc
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           masc-lex
      subcat            intransitive-lex
==>
   !bind! =ID-NP (new-name NP)

   =goal>
      state             "read"
      goal-cat          VP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            masc
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            masc
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-singular-any-case-fem
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           fem-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            fem
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            fem
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-sentential-comp-singular-any-case
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            transitive-CP-finite-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          CP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            transitive-CP-finite
      waiting-for-cat   wait-for-CP
      next-goal         next-VP-goal
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



;; embedded versions of above

(P attach-subject-NP-singular-any-case-embedded
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-embedded-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-embedded-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-singular-any-case-masc-embedded
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-embedded-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           masc-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-embedded-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            masc
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            masc
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-subject-NP-singular-any-case-fem-embedded
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-embedded-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           fem-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-embedded-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            fem
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            fem
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-sentential-comp-singular-any-case-embedded
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-embedded-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            transitive-CP-finite-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          CP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            transitive-CP-finite
      waiting-for-cat   wait-for-CP
      next-goal         next-VP-embedded-goal
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; gapped versions of above

(P attach-subject-NP-singular-any-case-gapped
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-gapped-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-gapped-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-singular-any-case-masc-gapped
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-gapped-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           masc-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-gapped-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            masc
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            masc
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-subject-NP-singular-any-case-fem-gapped
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           all-genders
      next-goal         next-VP-gapped-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           fem-lex
      subcat            intransitive-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          VP-gapped-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            fem
      subcat            intransitive
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      gender            fem
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; HACK: this may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word
;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-subject-NP-sentential-comp-singular-any-case-gapped
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           nom
      number      sing-plural
      gender           =gender
      next-goal         next-VP-gapped-goal
      spec-of           =IP
      waiting-for-case  wait-for-nom
      waiting-for-cat   wait-for-NP
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            transitive-CP-finite-lex
==>
  !bind! =ID-NP (new-name NP)
   =goal>
      state             "read"
      goal-cat          CP-goal
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              nom
      gender            =gender
      subcat            transitive-CP-finite
      waiting-for-cat   wait-for-CP
      next-goal         next-VP-gapped-goal
      spec-of           =IP
   =retrieval>
      head-word         =word
      comp              =ID-NP
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
;   =IP>                                       ;; This may not be legal
;      isa               syn-obj
;      number            sing
;      subj-word         =word

;;; We directly use ACT-R functions here to modify the current IP chunk
;;; in memory
   !eval! (mod-current-ip (list 
     'number 'sing 
     'subj-word =word
     ))

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Attach object nouns

(P attach-object-NP-fem-singular-any-case
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           acc
      number      sing-plural
      gender           all-genders
      next-goal         =next-goal
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP
      head-word         =head-word
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case              all-cases-lex
      number            sing-lex
      gender           fem-lex
      subcat            intransitive-lex
==>
   =goal>
      state             "read"
      goal-cat          =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)

  !bind! =ID-NP (new-name NP)
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number            sing
      case              acc
      gender            fem
      subcat            intransitive
   =retrieval>
      comp              =ID-NP
      number            sing
      gender            fem
      head-word         =word
      waiting-for-case  nil
      waiting-for-cat   nil


   !eval! (attach-message =head-word "object" =word)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-object-NP-masc-singular-any-case
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           acc
      number      sing-plural
      gender           all-genders
      next-goal         =next-goal
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP
      head-word         =head-word
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           masc-lex
      subcat            intransitive-lex
==>
   =goal>
      state             "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-NP (new-name NP)
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number            sing
      case              acc
      gender            masc
      subcat            intransitive
   =retrieval>
      comp              =ID-NP
      head-word         =word
      number            sing
      gender            masc
      waiting-for-case  nil
      waiting-for-cat   nil

   !eval! (attach-message =head-word "object" =word)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-object-NP-singular-any-case-any-gender
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          NP-goal
   =retrieval>
      isa      syn-obj
      cat            DP
      case           acc
      number      sing-plural
      gender           all-genders
      next-goal         =next-goal
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-NP
      head-word         =head-word
   =lex>
      isa               lexical-entry
      cat               N
      word              =word
      case           all-cases-lex
      number      sing-lex
      gender           all-genders-lex
      subcat            intransitive-lex
==>
   =goal>
      state             "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-NP (new-name NP)
   +NPb>
      isa               syn-obj
      cat               NP
      ID                =ID-NP
      head              =word
      number      sing
      case              acc
      gender            all-genders
      subcat            intransitive
   =retrieval>
      comp              =ID-NP
      head-word         =word
      number            sing
      waiting-for-case  nil
      waiting-for-cat   nil
   !eval! (attach-message =head-word "object" =word)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Match expectation for an VP, checking that number matches.

(P attach-VP-transitive-DP-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-DP-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                 DP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     none
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
   
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-infinitive-transitive-DP-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-infinite
         number         =number
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-DP-lex
   finite                  infinite-lex
==>
   =goal>
        state                   "read"
   goal-cat                 DP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     none
   =retrieval>
   head        =word
   comp                    =ID-VP
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
   
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-VP-transitive-DP-subj-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     spec
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-DP-lex
   number         sing-plural-lex
   tense       past-lex
==>
   !bind! =current-clause (current-clause)
   =goal>
        state                   "read"
   goal-cat                 DP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     spec
   subj-predicate          =subj-pred
   in-clause               =current-clause
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
   
 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-VP-transitive-DP-object-gap
   =goal>
      ISA                  comprehend-sentence
      state                   "wm-retrieval"
   =retrieval>
      isa               syn-obj
      head              nil
      cat               IP
      waiting-for-cat         wait-for-VP
      waiting-for-finite      wait-for-finite
      number         =number
     next-goal               =next-goal
     gap                     open
     subj-word               =subj-word
     subj-predicate          =subj-pred
   =lex>
      isa                     lexical-entry
      word                    =word
      cat            V
      subcat                  transitive-DP-lex
      number         sing-plural-lex
      tense       past-lex
==>
   =goal>
      state                   "wm-retrieval"
      goal-cat                DP-goal
      cue1                    nil
;      cue1                    DP
      cue2                    yes-filler
      cue3                    nil
      cue4                    nil

  !bind! =ID-VP (new-name VP)
   +VPb>
      isa                     syn-obj
      cat                     VP
      ID                      =ID-VP
      head                    =word
      number                  =number
      subcat                  transitive
      waiting-for-case        wait-for-acc
      waiting-for-cat         wait-for-DP
      tense                   past
      next-goal               =next-goal
      gap                     open
      subj-predicate          =subj-pred
   =retrieval>
      head                    =word
      comp                    =ID-VP
      tense                   past
   +retrieval>
      isa                     syn-obj
      filler                  yes-filler
      cat                     DP
 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

   !eval! (increase-ref-count-parseloc)
)





(P attach-VP-transitive-CP-no-gap
   =goal>
      ISA                  comprehend-sentence
      state                   "wm-retrieval"
   =retrieval>
      isa         syn-obj
      head        nil
      cat               IP
      waiting-for-cat         wait-for-VP
      waiting-for-finite      wait-for-finite
      number         =number
      next-goal               =next-goal
      gap                     none
      subj-word               =subj-word
   =lex>
      isa                     lexical-entry
      word                    =word
      cat            V
      subcat                  transitive-CP-finite-lex
      number         sing-plural-lex
      tense       past-lex
==>
   =goal>
      state                   "read"
      goal-cat                 CP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive-CP-finite
        waiting-for-cat         wait-for-CP
   tense                   past
   next-goal               =next-goal
   gap                     none
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-transitive-CP-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     =gap
     -  gap                    none
   subj-word               =subj-word
   subj-predicate          =subj-pred

   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-CP-finite-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                 CP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive-CP-finite
        waiting-for-cat         wait-for-CP
   tense                   past
   next-goal               =next-goal
   gap                     =gap
   subj-predicate       =subj-pred
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-transitive-CP-DP-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-CP-DP-finite-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                CP-DP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive-DP-finite
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     none

  !bind! =ID-VP2 (new-name VP2)
   +VP2b>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP2
   head                    =word
        number                  =number
        subcat                  transitive-CP-finite
;  waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-CP
        waiting-for-cat2        wait-for-IP
   waiting-for-finite      wait-for-finite
   tense                   past
   next-goal               =next-goal
   gap                     none
   comp-of                 =retrieval
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-transitive-CP-DP-subj-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     spec
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-CP-DP-finite-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                CP-DP-goal

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive-DP-finite
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     spec
   subj-predicate          =subj-pred

  !bind! =ID-VP2 (new-name VP2)
   +VP2b>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP2
   head                    =word
        number                  =number
        subcat                  transitive-CP-finite
        waiting-for-cat         wait-for-CP
        waiting-for-cat2        wait-for-IP
   waiting-for-finite      wait-for-finite
   tense                   past
   next-goal               =next-goal
   gap                     spec
   comp-of                 =retrieval
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-transitive-CP-DP-object-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     open
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  transitive-CP-DP-finite-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                CP-DP-goal
        cue1                    DP
        cue2                    yes-filler
   cue3                    nil
        cue4                    nil

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  transitive-DP-finite
   waiting-for-case        wait-for-acc
        waiting-for-cat         wait-for-DP
   tense                   past
   next-goal               =next-goal
   gap                     open
   subj-predicate          =subj-pred

!bind! =ID-VP2 (new-name VP2)
   +VP2b>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP2
   head                    =word
        number                  =number
        subcat                  transitive-CP-finite
        waiting-for-cat         wait-for-CP
        waiting-for-cat2        wait-for-IP
   waiting-for-finite      wait-for-finite
   tense                   past
   next-goal               =next-goal
   gap                     open
   comp-of                 =retrieval
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   +retrieval>
   isa         syn-obj
   filler         yes-filler
   cat                     DP

 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-intransitive-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  intransitive-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  intransitive
   tense                   past
   gap                     none
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-intransitive-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     =gap
      - gap                     none
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  intransitive-lex
   number         sing-plural-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
        cat                     VP
        ID                      =ID-VP
   head                    =word
        number                  =number
        subcat                  intransitive
   tense                   past
   gap                     =gap
   subj-predicate          =subj-pred
   =retrieval>
   head        =word
   comp                    =ID-VP
   tense       past
   waiting-for-cat         nil
   waiting-for-finite      nil
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-gerund-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         cat               IP
   subcat                  aux
   waiting-for-cat         wait-for-VP
   next-goal               =next-goal
   gap                     =gap
      - gap                     none
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  gerund-lex
==>
   =goal>
        state                   "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
   cat                     VP
   ID                      =ID-VP
   head        =word
        subcat                  gerund
   gap                     open
   =retrieval>
   comp                    =ID-VP
   waiting-for-finite      nil
   waiting-for-cat         nil
   waiting-for-cat2        nil
   waiting-for-cat3        nil
   ; -retrieval>

   !eval! (attach-message =word "subject" =subj-word)  
;   !eval! (add-reference (wme-references (get-wme =subj-pred)))
;   !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-gerund-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         cat               IP
   subcat                  aux
   waiting-for-cat         wait-for-VP
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  gerund-lex
==>
   =goal>
        state                   "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

  !bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
   cat                     VP
   ID                      =ID-VP
   head        =word
        subcat                  gerund
   =retrieval>
   comp                    =ID-VP
   waiting-for-finite      nil
   waiting-for-cat         nil
   waiting-for-cat2        nil
   waiting-for-cat3        nil
   ; -retrieval>

   !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-VP-participle
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         cat               IP
   subcat                  aux
   waiting-for-cat         wait-for-VP
   next-goal               =next-goal
;        gap                     none
   subj-word               =subj-word
   spec                    =subject
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            V
   subcat                  past-participle-transitive-lex
==>
   =goal>
;        state                   "wm-retrieval"
        state                   "read"
   goal-cat                PP-goal
   cue1                    =retrieval
        cue2                    DP
        cue3                    nil
        cue4                    nil
   
;      goal-cat        =goal-cat
;   !bind! =goal-cat (map-goal-category =next-goal)      

!bind! =ID-VP (new-name VP)
   +VPb>
        isa                     syn-obj
   cat                     VP
   ID                      =ID-VP
   head        =word
        subcat                  past-participle-transitive
;  gap                     open
   next-goal               =next-goal
   waiting-for-cat         wait-for-PP
   waiting-for-cat2        wait-for-agent-by-phrase
   =retrieval>
   comp                    =ID-VP
   waiting-for-finite      nil
   waiting-for-cat         nil
   waiting-for-cat2        nil
   waiting-for-cat3        nil
   ; +retrieval>
   ;      =subject
   !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-aux-verb-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
        gap                     =gap
-  gap                     none
   subj-word               =subj-word
   subj-predicate          =subj-pred
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            I-V
   number         sing-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                 XP-goal    
   =retrieval>
   head        =word
   tense       past
   number                  sing
    subcat                  aux
   gap                     =gap
   waiting-for-cat         wait-for-VP
   waiting-for-cat2        wait-for-AdjP
   waiting-for-cat3        wait-for-PP
   waiting-for-finite      wait-for-finite
   next-goal               =next-goal
   subj-word               =subj-word
   subj-predicate          =subj-pred
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-aux-verb-no-gap
   =goal>
         ISA                  comprehend-sentence
        state                   "wm-retrieval"
;  goal-cat                 VP-goal
   =retrieval>
         isa         syn-obj
         head        nil
         cat               IP
   waiting-for-cat         wait-for-VP
   waiting-for-finite      wait-for-finite
         number         =number
   next-goal               =next-goal
   gap                     none
   subj-word               =subj-word
   =lex>
        isa                     lexical-entry
   word                    =word
   cat            I-V
   number         sing-lex
   tense       past-lex
==>
   =goal>
        state                   "read"
   goal-cat                 XP-goal    
   =retrieval>
   tense       past
   number                  sing
   subcat                  aux
   head                    =word
   gap                     none
   waiting-for-cat         wait-for-VP
   waiting-for-cat2        wait-for-AdjP
   waiting-for-cat3        wait-for-PP
   waiting-for-finite      wait-for-finite
   next-goal               =next-goal
   subj-word               =subj-word
   ; -retrieval>
 !eval! (attach-message =word "subject" =subj-word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Attach PP as modifier of DP/NP.  This production also sets the PP
;; "looking" for an accusative DP to be its object. 

(P attach-PP-as-modifier-of-retrieved-noun
   =goal>
      ISA               comprehend-sentence
      state         "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      isa      syn-obj
      cat            NP
      ID                =ID-RETR
      head              =head-word
      spec-of           nil
   =lex>
      isa            lexical-entry
      cat            P
      subcat            transitive-DP-lex
      word              =word
 ==>
   =goal>
      state         "read"
      goal-cat          DP-goal

  !bind! =ID-PP (new-name PP)
   +PPb>
      isa               syn-obj
      head              =word
      cat               PP
      ID                =ID-PP
      modif-of    =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal        =goal-PP
   !bind! =goal-PP (map-next-goal-category =goal-cat)

   =retrieval>
      modifier    =ID-PP
   ; -retrieval>

 !eval! (attach-message =head-word "modifier" =word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-PP-as-modifier-of-retrieved-noun-subj
   =goal>
      ISA               comprehend-sentence
      state         "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      isa      syn-obj
      cat            NP
      ID                =ID-RETR
      head              =head-word
      spec-of           =IP
   =lex>
      isa            lexical-entry
      cat            P
      subcat            transitive-DP-lex
      word              =word
 ==>
   =goal>
      state         "read"
      goal-cat          DP-goal

  !bind! =ID-PP (new-name PP)
   +PPb>
      isa               syn-obj
      head              =word
      cat               PP
      ID                =ID-PP
      modif-of    =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal        =goal-PP
   !bind! =goal-PP (map-next-goal-category =goal-cat)

   =retrieval>
      modifier    =ID-PP
   ; -retrieval>

 !eval! (attach-message =head-word "modifier" =word)  
; !eval! (add-reference (wme-references (get-wme =IP)))
; !eval! (boost-message =IP)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-PP-as-modifier-of-retrieved-VP
   =goal>
      ISA               comprehend-sentence
      state         "wm-retrieval"
      goal-cat          =goal-cat
-     goal-cat          PP-goal
   =retrieval>
      isa      syn-obj
      cat            VP
      ID                =ID-RETR
      head              =head-word
   =lex>
      isa            lexical-entry
      cat            P
      subcat            transitive-DP-lex
      word              =word
 ==>
   =goal>
      state         "read"
      goal-cat          DP-goal
      
  !bind! =ID-PP (new-name PP)
   +PPb>
      isa               syn-obj
      head              =word
      cat               PP
      ID                =ID-PP
      modif-of    =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal        =goal-PP
   !bind! =goal-PP (map-next-goal-category =goal-cat)

   =retrieval>
      modifier    =ID-PP
   ; -retrieval>

 !eval! (attach-message =head-word "modifier" =word)  

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



(P attach-PP-as-object
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      waiting-for-cat3  wait-for-PP
      next-goal         =next-goal
      head              =head-word
      subj-predicate    =subj-pred
   =lex>
      isa      lexical-entry
      cat      P
      subcat            transitive-DP-lex
      word              =word
==>
   =goal>
      state             "read"
      goal-cat          DP-goal

!bind! =ID-PP (new-name PP)
   +PPb>
      isa               syn-obj
      cat               PP
      ID                =ID-PP
      head              =word
      comp-of     =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal         =next-goal
   =retrieval>
      waiting-for-case  nil
      waiting-for-cat   nil
      waiting-for-cat2  nil
      waiting-for-cat3  nil
      comp              =ID-PP
   ; -retrieval>

 !eval! (attach-message =head-word "object" =word)
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-agent-PP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      waiting-for-cat   wait-for-PP
      next-goal         =next-goal
      head              =head-word
      subcat            past-participle-transitive      
    =lex>
      isa      lexical-entry
      cat      P
      subcat            transitive-DP-lex
      word              =word
==>
   =goal>
      state             "read"
      goal-cat          DP-goal

!bind! =ID-PP (new-name PP)
   +PPb>
      isa               syn-obj
      cat               PP
      ID                =ID-PP
      head              =word
      comp-of     =ID-RETR
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal         =next-goal
   =retrieval>
      waiting-for-case  nil
      waiting-for-cat   nil
      waiting-for-cat2  nil
      waiting-for-cat3  nil
      modifier          =ID-PP
   ; -retrieval>

 !eval! (attach-message =head-word "agent-by-phrase" =word)
; !eval! (add-reference (wme-references (get-wme =subj-pred)))
; !eval! (boost-message =subj-pred)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Attach CP as relative clause modifying a DP.

(P attach-CP-as-modifier-of-retrieved-singular-DP
   =goal>
      ISA      comprehend-sentence
;      state             "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      isa      syn-obj
      cat            DP
      ID                =ID-RETR
      number            sing
      head-word         =subj-word
      spec-of           =IP
   =lex>
      isa         lexical-entry
      cat            C
      finite            finite-lex
      word              =word
==>
   =goal>
      state             "read"
      goal-cat          VP-gapped-goal

  !bind! =ID-DP (new-name DP)
   +DPb>
      isa      syn-obj
      cat      DP
      ID    =ID-DP
      filler            done                ;yes-filler
      case     all-cases
      coindexed-with =retrieval
      head-word         =subj-word
      subj-predicate    =IP

  !bind! =ID-IP (new-name IP)
   +IPb>
      isa      syn-obj
      cat      IP
      ID    =ID-IP
      waiting-for-cat   wait-for-VP
      waiting-for-finite  wait-for-finite
      spec              =ID-DP
      finite            finite
      number            sing
      gap               spec
      embedded          embedded
      gapped            gapped
      subj-word         =subj-word
      subj-predicate   =IP

      next-goal        =goal-IP
   !bind! =goal-IP (map-next-goal-category =goal-cat)

  !bind! =ID-CP (new-name CP)
   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      head              =word
      modif-of    =ID-RETR
      spec              =ID-DP
      comp              =ID-IP
     next-goal        =goal-CP
   !bind! =goal-CP (map-next-goal-category =goal-cat)

   =retrieval>
      modifier          =ID-CP
   ; -retrieval>

 !eval! (attach-message =subj-word "modifier" =word)  

; !eval! (add-reference (wme-references (get-wme =IP)))
; !eval! (boost-message =IP)


  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



;; Eliminate the creation of the IP at the that-complement?
;; (unless all other complements also create nodes?)

(P attach-CP-as-modifier-of-retrieved-singular-DP-wh-pronoun
   =goal>
      ISA      comprehend-sentence
;      state             "wm-retrieval"
      goal-cat          =goal-cat
   =imaginal>
      ISA               parsing-state
   =retrieval>
      isa      syn-obj
      cat            DP
      ID                =ID-RETR
      number            sing
      head-word         =subj-word
      spec-of           =ID-IP1
   =lex>
      isa         lexical-entry
      cat            wh-pronoun
      word              =word
      case              all-cases-lex
==>
   !bind! =ID-IP (new-name IP)
   !bind! =ID-DP (new-name DP)
   !bind! =ID-CP (new-name CP)
   !bind! =currentpos *current-index*

   =goal>
      state             "read"
      goal-cat          VP-gapped-goal
   =imaginal>
      filler-pos        =currentpos
   +DPb>
      isa      syn-obj
      cat      DP
      ID                =ID-DP
      head              =word
      filler            done             ;yes-filler
      case     all-cases
      coindexed-with =ID-RETR
      head-word         =subj-word
      subj-predicate    =ID-IP1
   +IPb>
      isa      syn-obj
      cat      IP
      ID                =ID-IP
      waiting-for-cat   wait-for-VP       
      waiting-for-finite  wait-for-finite
      spec              =ID-DP
      finite            finite
      embedded          embedded
      gapped            gapped
      number            sing
      gap               spec
      subj-word         =subj-word
      subj-predicate    =ID-IP1
      next-goal         =goal-ip

   !bind! =goal-ip (map-next-goal-category =goal-cat)

   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      modif-of    =ID-RETR
      spec              =ID-DP
      comp              =ID-IP
     next-goal        =goal-CP
   !bind! =goal-CP (map-next-goal-category =goal-cat)


   =retrieval>
      modifier          =ID-CP
   ; -retrieval>

 !eval! (attach-message =subj-word "modifier" =word)  

   !eval! (set-begin-time =word)
; !eval! (add-reference (wme-references (get-wme =IP1)))
; !eval! (boost-message =IP1)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




;; Attach CP as complement

(P attach-CP-as-complement
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      ID      =ID-RETR
      waiting-for-cat   wait-for-CP
      subcat            transitive-CP-finite
      next-goal         =next-goal
      spec-of           nil
   =lex>
      isa         lexical-entry
      cat            C
      finite            finite-lex
      word              =word
==>
   !bind! =ID-IP (new-name IP)
   !bind! =ID-CP (new-name CP)

   =goal>
      state             "read"
      goal-cat          IP-embedded-goal
   +IPb>
      isa      syn-obj
      cat      IP
      ID    =ID-IP
      waiting-for-cat   wait-for-IP
      waiting-for-finite  wait-for-finite
      next-goal         =next-goal
      finite            finite
      gender      all-genders
      number      sing-plural
      embedded          embedded
   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      head              =word
      comp              =ID-IP
      comp-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      finite            finite
      next-goal         =next-goal
   =retrieval>
      waiting-for-cat   nil
      waiting-for-cat2  nil
      waiting-for-finite nil
      comp              =ID-CP
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-CP-as-complement-subj
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      waiting-for-cat   wait-for-CP
      subcat            transitive-CP-finite
      next-goal         =next-goal
      spec-of           =ID-IP1
   =lex>
      isa         lexical-entry
      cat            C
      finite            finite-lex
      word              =word
==>
   !bind! =ID-IP (new-name IP)
   !bind! =ID-CP (new-name CP)
   
   =goal>
      state             "read"
      goal-cat          IP-embedded-goal
   +IPb>
      isa      syn-obj
      cat      IP
      ID    =ID-IP
      waiting-for-cat   wait-for-IP
      waiting-for-finite  wait-for-finite
      next-goal         =next-goal
      finite            finite
      gender      all-genders
      number      sing-plural
      embedded          embedded
   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      head              =word
      comp              =ID-IP
      comp-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      finite            finite
;      waiting-for-finite wait-for-finite         ;; THIS CAUSED INTERFERENCE
      next-goal         =next-goal
   =retrieval>
      waiting-for-cat   nil
      waiting-for-cat2  nil
      waiting-for-finite nil
      comp              =ID-CP
   ; -retrieval>
; !eval! (attach-message =head-word "modifier" =word)  
; !eval! (add-reference (wme-references (get-wme =IP)))
; !eval! (boost-message =ID-IP1)


  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



;; Sentential subject

(P attach-infinite-CP-as-subject-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      cat      IP
      ID                =ID-RETR
      head              nil
      waiting-for-cat   wait-for-IP
   =lex>
      isa      lexical-entry
      cat      C
      word              =word
      finite            infinite-lex
==>
   !bind! =ID-IP (new-name IP)
   !bind! =ID-CP (new-name CP)
   =goal>
      state             "read"
      goal-cat          IP-embedded-goal
   +IPb>
      isa      syn-obj
      cat      IP
      ID    =ID-IP
      waiting-for-cat   wait-for-IP
      waiting-for-finite  wait-for-infinite
      next-goal         next-VP-goal
      finite            infinite
      gender      all-genders
      number      sing-plural
      embedded          embedded
   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      head              =word
      comp              =ID-IP
      comp-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      finite            infinite
      next-goal         next-VP-goal
   =retrieval>
      spec     =ID-CP
      waiting-for-cat   wait-for-VP
      number            sing
      subj-word         =word
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-finite-CP-as-subject-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      syn-obj
      cat      IP
      ID                =ID-RETR
      head              nil
      waiting-for-cat   wait-for-IP
   =lex>
      isa      lexical-entry
      cat      C
      word              =word
      finite            finite-lex
==>
   !bind! =ID-IP (new-name IP)
   !bind! =ID-CP (new-name CP)
   =goal>
      state             "read"
      goal-cat          IP-embedded-goal
   +IPb>
      isa      syn-obj
      cat      IP
      ID    =ID-IP
      waiting-for-cat   wait-for-IP
      waiting-for-finite  wait-for-finite
      next-goal         next-VP-goal
      finite            finite
      gender      all-genders
      number      sing-plural
      embedded          embedded
   +CPb>
      isa               syn-obj
      cat               CP
      ID                =ID-CP
      head              =word
      comp              =ID-IP
      comp-of     =ID-RETR
      gender      all-genders
      number      sing-plural
      finite            finite
      next-goal         next-VP-goal
   =retrieval>
      spec     =ID-CP
      waiting-for-cat   wait-for-VP
      number            sing
      subj-word         =word
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)


;; repair reduced relative subject/object ambiguity 
;; Add extra retrieval for current IP/DP/VP?
;; ? change IP directly (vp in comp) and retrieve DP as new subj
(P attach-IP-as-complement-repair
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      isa      syn-obj
      cat               VP
      ID                =ID-VP
      waiting-for-cat2  wait-for-IP
      subcat            transitive-CP-finite
      comp-of           =IP
   =lex>
      isa         lexical-entry
      cat            I-V
      number      sing-lex
      tense    past-lex
      word              =word
   ; =IP>
   ;    isa               syn-obj
   ;    comp              =VP
   ; =VP>
   ;    isa               syn-obj
   ;    comp              =DP
   ; =DP> 
   ;    isa                syn-obj
   ;    head-word          =subj-word
==>
   !bind! =ID-IP2 (new-name IP)
   !bind! =ID-VP2 (new-name VP)
   =goal>
      ; state             "read"
      ; goal-cat          XP-goal
      attend-to         "left"
   +VPb>
      isa               syn-obj
      cat               VP2
      ID                =ID-VP
   +IPb>
      isa      syn-obj
      cat      IP
      ID                =ID-IP2
      head              =word
      waiting-for-cat         wait-for-VP
      waiting-for-cat2        wait-for-AdjP
      waiting-for-cat3        wait-for-DP
      finite            finite
      gender      all-genders
      number      sing
      ; spec              =ID-DP
      subcat            aux
      embedded          embedded
      ; subj-predicate    +IPb
      ; subj-word         =subj-word

      next-goal        =goal-IP
      !bind! =goal-IP (map-next-goal-category =goal-cat)

   =retrieval>
      waiting-for-cat   nil
      comp              =ID-IP2
   ; =IP>
   ;    comp              =retrieval
   =VP2b>                =retrieval
   !eval! (mod-chunk-fct =IP (list 
     'comp =retrieval
     ))
   +retrieval>
      ISA               syn-obj
      cat               DP
  ; !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)

(P revise-DP-from-obj-to-subj-of-compl-clause
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          =goal-cat
   =IPb> ISA            syn-obj
   =VPb> ISA            syn-obj
   =VP2b> ISA           syn-obj
   =retrieval>
      ISA               syn-obj
      cat               DP
      head-word         =word
==>
   =goal>
      state             "read"
      attend-to         "next-word"
      goal-cat          XP-goal
   =IPb>
      spec              =retrieval
      comp              =VPb
      subj-word         =word
   =VP2b>
      comp              =IPb
   !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)


#|
(P attach-IP-as-complement-repair
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          =goal-cat
   =retrieval>
      isa      syn-obj
      cat               VP
      waiting-for-cat2  wait-for-IP
      subcat            transitive-CP-finite
      comp-of           =IP
   =lex>
      isa         lexical-entry
      cat            I-V
      number      sing-lex
      tense    past-lex
      word              =word
   =IP>
      isa               syn-obj
      comp              =VP
   =VP>
      isa               syn-obj
      comp              =DP
   =DP> 
      isa                syn-obj
      head-word          =subj-word
==>
   =goal>
      state             "read"
      goal-cat          XP-goal
   +VPb>
      isa               syn-obj
      cat               VP
   +IPb>
      isa      syn-obj
      cat      IP
      head              =word
      waiting-for-cat         wait-for-VP
      waiting-for-cat2        wait-for-AdjP
      waiting-for-cat3        wait-for-DP
      finite            finite
      gender      all-genders
      number      sing
      spec              =DP
      subcat            aux
      embedded          embedded
      subj-predicate    +IPb
      subj-word         =subj-word

      next-goal        =goal-IP
   !bind! =goal-IP (map-next-goal-category =goal-cat)

   =retrieval>
      waiting-for-cat   nil
      comp              +IPb
   =IP>
      comp              =retrieval
   ; -retrieval>

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)

|#

;; Fill an object gap.

(P attach-filler-in-object-gap
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =VPb>
      isa               syn-obj
      gap               open
      subcat            transitive
      waiting-for-case  wait-for-acc
      waiting-for-cat   wait-for-DP
      next-goal         =next-goal
      head              =v-word
   =retrieval>
      isa      syn-obj
      ID                =ID-RETR
      filler      yes-filler
      case     all-cases
      head-word         =filler-word
      subj-predicate    =subj-pred
==>
   =goal>
      state             "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)      

   =VPb>
      comp     =ID-RETR
      waiting-for-case  nil
      waiting-for-cat   nil
      gap      comp
   =retrieval>
      case     acc
      filler            done
   ; -retrieval>

   !eval! (mod-current-ip (list 
;        'subj-word =word
        'gap 'done
        'waiting-for-cat NIL
        'waiting-for-finite NIL
        ))

 !eval! (attach-message =v-word "object (gap)" =filler-word)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)




(P attach-empty-category-in-object-passive
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =VPb>
      isa               syn-obj
;      gap               open
      subcat            past-participle-transitive
      next-goal         =next-goal
      head              =v-word
   =retrieval>
      isa      syn-obj
      cat               DP
      head-word         =obj-word
==>
   !bind! =ID-DP (new-name DP)
   =goal>
      state             "read"
      goal-cat        =goal-cat
   !bind! =goal-cat (map-goal-category =next-goal)
   +DPb>
      isa      syn-obj
      cat      DP
      ID    =ID-DP
      head-word         =obj-word
      case     acc
      coindexed-with =retrieval
      filler            done
   =VPb>
      comp     =ID-DP
      waiting-for-case  nil
      waiting-for-cat   nil
      gap      comp
   ; -retrieval>

 !eval! (attach-message =v-word "object (passive)" =obj-word)

  !eval! (set-end-time)
   !eval! (increase-ref-count-parseloc)
)



