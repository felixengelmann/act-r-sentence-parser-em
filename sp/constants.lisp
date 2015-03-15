(add-dm
  (ip-goal isa chunk)
  (ip-gapped-goal isa chunk))


;;; C H U N K   T Y P E S

(chunk-type parsing-state
      ; (word nil)
      (att-obj nil)
      ; (parse-loc nil)
      ; current-IP
      (attached-pos ())
      (ip-pos nil)
      (vp-pos nil)
      (pred-pos nil)
      (filler-pos nil)
      (clause-id-stack nil)
      ; (unattached NIL)
      ; last-loc
      
      ; (retr-obj NIL)
      )

(chunk-type comprehend-sentence
      state
      em-state
      (attend-to "next-word")
      word
      target
      unattached
      ; (goal-cat  IP-gapped-goal)     ;; German
      (goal-cat  IP-goal)    ;; English
      ; goal-subcat
      polarity
      time-out
      skip
      (regression nil)
      last-loc
      loc
      (filler-pos nil)
      ; last-parse-loc
      ; (retrieval "free")
      cue1
      cue2
      cue3
      cue4
      )


(chunk-type lexical-entry
            word
            cat
      (subcat intransitive-lex)
            
            number
            case
            person
            gender
            polarity
            tense
            voice
            finite
            mood
            ;     (waiting-for-case   none)
            ;     (waiting-for-cat    none)
            ;     (waiting-for-finite none)
            screen-pos
            )



(chunk-type syn-obj
    cat
    subcat
    ID
    next-goal
    head
    head-word   ; purely for trace information
    subj-word   ; purely for trace information
    unattached
    (in-clause C0)
    (location nil)
    ; comp-word
    subj-predicate  
    (waiting-for-finite none)
    (waiting-for-case   none)
    (waiting-for-num    none)
    (waiting-for-cat    none)
    (waiting-for-cat2   none)
    (waiting-for-cat3   none)
    case
    gender
    polarity
    number
    finite
    tense
    (mood  nil)
    (embedded   not-embedded)
    gapped
    (spec        nil)
    (adj-modifier nil)
    (adjunct     nil)
    (comp        nil)
    (comp2       nil)
    (modifier    nil)
    ;;(argument    none)
    conjunct-head
    (conjunct       nil)
    (conjunct-of    nil)
    (conjunct-word  nil)
    (comp-of     none)           
    (modif-of    none)
    ;;(argument-of none)
    (adjunct-of  none) 
    (spec-of     nil)
    (cp-spec-of  none)
    coindexed-with
    attached
    (gap        none)
    filler
)




;;; C O N S T A N T S

(add-dm
    (top-goal isa comprehend-sentence state nil em-state nil)
)

(add-dm
  (all-cases isa chunk)
  (none  isa chunk)
;  (ip-goal isa chunk)
;  (ip-gapped-goal isa chunk)
  (embedded isa chunk)
  (not-embedded isa chunk)

  (nom  isa chunk)
  (acc  isa chunk)
  (gen  isa chunk)
  (dat  isa chunk)
  (oblique isa chunk)

  (wait-for-acc isa chunk)
  (wait-for-nom isa chunk)
  (wait-for-dat isa chunk)
  (wait-for-gen isa chunk)
  (wait-for-oblique isa chunk)
  (wait-for-all-cases isa chunk)
  (discharged-case isa chunk)

  (N isa chunk)
  (V isa chunk)
  (I isa chunk)
  (P isa chunk)
  (C isa chunk)
  (DET isa chunk)
  (ADV isa chunk)
  (ADJ isa chunk)

  (DP isa chunk)
  (IP isa chunk)
  (PP isa chunk)
  (NP isa chunk)
  (VP isa chunk)
  (NP-VP isa chunk)

  (wait-for-DP isa chunk)
  (wait-for-IP isa chunk)
  (wait-for-PP isa chunk)
  (wait-for-NP isa chunk)
  (wait-for-VP isa chunk)

  (discharged-cat isa chunk)

  (wait-for-finite isa chunk)
  (discharged-finite isa chunk)

  (tensed  isa chunk)
  (past    isa chunk)
  (future    isa chunk)
  (past-participle isa chunk)
  (passive isa chunk)
  (indicative isa chunk)
  (subjunctive isa chunk)
  (all-moods isa chunk)

  (1stPers isa chunk)
  (2ndPers isa chunk)
  (3rdPers isa chunk)

  (sing isa chunk)
  (plural isa chunk)
  (sing-plural isa chunk)
  (wait-for-sing isa chunk)
  (wait-for-plural isa chunk)
  (wait-for-sing-plural isa chunk)

  (fem isa chunk)
  (masc isa chunk)
  (fem-masc isa chunk)
  (all-genders isa chunk))


(set-similarities
   (wait-for-DP wait-for-IP -0.8)
 
   (all-cases nom -0.2)
   (all-cases acc -0.2)
   (all-cases dat -0.2)
   (all-cases oblique -0.2)

   (wait-for-all-cases wait-for-nom -0.2)
   (wait-for-all-cases wait-for-acc -0.2)
   (wait-for-all-cases wait-for-dat -0.2)
   (wait-for-all-cases wait-for-oblique -0.8)

   (none nom -0.2)
   (none acc -0.2)
   (none dat -0.2)
   (none gen -0.2)
   (none oblique -0.2)

   (none wait-for-nom -0.8)
   (none wait-for-acc -0.8)
   (none wait-for-dat -0.8)
   (none wait-for-gen -0.8)
   (none wait-for-oblique -0.8)
   (none wait-for-all-cases -0.8)

   (none wait-for-finite -0.8)

   (sing-plural sing -0.2)
   (sing sing-plural -0.2)
   (plural sing-plural -0.2)

   (wait-for-sing-plural wait-for-sing -0.2)
   (wait-for-sing-plural wait-for-plural -0.2)

   (fem-masc fem -0.2)
   (fem-masc masc -0.2)
   (all-genders fem -0.2)
   (all-genders masc -0.2)

   (tensed past -0.2)
   (past tensed -0.2)

   (NP NP-VP 0)
   (VP NP-VP 0)

   (none N -0.99)
   (none V -0.99)
   (none Adj -0.99)
   (none P -0.99)
   (none C -0.99)
   (none Adv -0.99))


(goal-focus top-goal)
