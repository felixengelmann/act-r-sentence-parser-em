
;;;
;;; SENTENCE:   "the dog bit the boy *stop*"
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START READING
;;    NOTE: The default value for slot goal-cat in the comprehend-sentence
;;      chunk-type is IP-gapped-goal (for German) or IP-goal (for English)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p start-reading
   =goal>
      ISA               	comprehend-sentence
      state             	NIL
   =visual-location>
      ISA               	visual-location
==>
   !bind! =ID-IP (new-name IP)
   =goal>
      state             	"start"
      goal-cat          	IP-goal
      ; goal-cat          IP-gapped-goal
   +imaginal>
      ISA                 parsing-state
   +IPb>
      ISA                 syn-obj
      cat                 IP
      ID                  =ID-IP
      waiting-for-cat     wait-for-IP
      ; waiting-for-cat     wait-for-DP
      waiting-for-finite  wait-for-finite
      ; finite              finite
      ; gapped              gapped
      embedded            not-embedded
      spec                NIL
      comp                NIL
      location            =visual-location
      next-goal           next-*done*
   =visual-location>
)


(P set-retrieval-cues-IP-goal-input-DET
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          IP-goal
   =retrieval>
      isa      					lexical-entry
      word              =word
      cat            		DET
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      cue1              wait-for-IP
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>
      ISA            		syn-obj
      waiting-for-cat   wait-for-IP

   !eval! (push-clause)
   !eval! (set-begin-time =word)
)




(P attach-DP-as-subject-of-predicted-IP
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
   =retrieval>
      isa      					syn-obj
      cat      					IP
      ID                =ID-RETR
      head              nil
      waiting-for-cat   wait-for-IP
   =lex>
      isa      					lexical-entry
      cat      					DET
      word              =word
      case     					all-cases-lex
      gender      			all-genders-lex
      number      			sing-plural-lex
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
)


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
)


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
)


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
)



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
)


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
)

