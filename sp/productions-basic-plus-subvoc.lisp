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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTEND to sentence one word at a time
;;    First, find location of word to attend to (there is a separate
;;      production for the first word), and then attend to it.
;;    NOTE: The default value for slot goal-cat in the comprehend-sentence
;;      chunk-type is IP-gapped-goal (for German) or IP-goal (for English)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p find-first-word
   =goal>
      ISA               comprehend-sentence
      ; state             NIL
      state             "start"
==>
   =goal>
      em-state          "looking"
      state             "read"
      ; regression        nil
      attend-to         "next-word"
      time-out          nil
      ; unexpected        nil
   +visual-location>
      ISA               visual-location
      screen-x          lowest
      :attended         NIL
   +imaginal>
      ISA               parsing-state
      attached-pos      ()
      ; time-out          NIL
      ; state             "parse"
)
(spp find-first-word :at 0.05)



;;  (1) Find location of next word when the current word is integrated.
;;      More specifically, get the nearest visual location to
;;      the right, and direct attention to that location.

(p find-next-word-/-A
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      em-state          "free"
      time-out          nil
   ?visual>
      processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
==>
   +visual-location>
      ISA               visual-location
    > screen-x          current
      screen-x          lowest
   =goal>
      em-state          "looking"
      last-loc          =visual-location
      regression        nil
)
(spp find-next-word-/-A :at 0.05)
; (spp find-next-word-/-A :u 20)



;;; RANDOM REGRESSION
(p find-previous-location
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "free"
      attend-to         "left"
      time-out          nil
      skip              nil
   =imaginal>
      ISA          parsing-state
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
==>
   !bind! =eye-loc (first (current-eye-loc))
   ; !bind! =target-loc (get-target-loc =eye-loc =target-pos)
   =goal>
      em-state          "looking"
      attend-to         nil
      last-loc          =visual-location
      ; time-out       t
    =imaginal>
       ; att-obj        =visual
       ; last-obj     =visual
    +visual-location>
         ISA         visual-location
       < screen-x    =eye-loc
       ; < screen-x    =eye-loc ;; target before current fixation
         ; screen-x    highest
        :attended   T
   ; !eval! (report-regression =visual-location =target-pos =target-loc)
   !eval! (trialmessage "regression" "left")
)
(spp find-previous-location :at 0)
(spp find-previous-location :u 10)


;; REREADING REGRESSION
(p find-sentence-beginning
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "free"
      attend-to         "start"
      time-out          nil
      skip              nil
   =imaginal>
      ISA               parsing-state
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
==>
   !bind! =eye-loc (first (current-eye-loc))
   ; !bind! =target-loc (get-target-loc =eye-loc =target-pos)
   =goal>
      em-state          "looking"
      attend-to         "next-word"
      last-loc          =visual-location
      ; time-out       t
    =imaginal>
       ; att-obj        =visual
       ; last-obj     =visual
    +visual-location>
         ISA         visual-location
       < screen-x    =eye-loc
       ; < screen-x    =eye-loc ;; target before current fixation
         screen-x    lowest
        ; :attended   nil
   ; !eval! (report-regression =visual-location =target-pos =target-loc)
   !eval! (trialmessage "regression" "reread-old")
)
(spp find-sentence-beginning :at 0.05)
; (spp find-sentence-beginning :u 10)


;;; TARGETED REGRESSION
(p find-target-position
   =goal>
      ISA               comprehend-sentence
    - state             "lexical-retrieval"
      ; em-state          "free"
    - attend-to         "next-word"
    - attend-to         "left"
    - attend-to         "word"
    - attend-to         "start"
    - attend-to         "location"
    - attend-to         nil
      attend-to         =target-pos
      time-out          nil
      skip              nil
   =imaginal>
      ISA          parsing-state
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
==>
   !bind! =eye-loc (first (current-eye-loc))
   !bind! =target-loc (get-target-loc =eye-loc =target-pos)
   =goal>
      em-state          "looking"
      attend-to         nil
      last-loc          =visual-location
    =imaginal>
    +visual-location>
         ISA         visual-location
       <= screen-x    =target-loc
       ; < screen-x    =eye-loc ;; target before current fixation
         screen-x    highest
        ; :attended   nil
   !eval! (report-regression =visual-location =target-pos =target-loc)
)
(spp find-target-position :at 0)
(spp find-target-position :u 10)



(p find-target-location-attended
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "free"
      attend-to         "location"
      time-out          nil
      skip              nil
      regression        NIL
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
   ?visual-location>
      attended          t
==>
   =goal>
      em-state          "looking"
      attend-to         "next-word"
      regression        t
   ; !eval! (report-regression =visual-location =target-pos =target-loc)
   !eval! (trialmessage "regression" "attended-loc")
)
(spp find-target-location-attended :at 0)
(spp find-target-location-attended :u 10)


(p find-target-location-unattended
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "free"
      attend-to         "location"
      time-out          nil
      skip              nil
      regression        NIL
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
   ?visual-location>
      attended          nil
==>
  !bind! =eye-loc (first (current-eye-loc))
   =goal>
      em-state          "looking"
      attend-to         "next-word"
      regression        t
      last-loc          =visual-location
    +visual-location>
         ISA         visual-location
         < screen-x  =eye-loc
         screen-x    lowest
    !eval! (trialmessage "regression" "reread")
   ; !eval! (report-regression =visual-location =target-pos =target-loc)
)
(spp find-target-location-unattended :at 0.01)
; (spp find-target-location-unattended :u -0.15)
(spp find-target-location-unattended :u 0.25)
;; with 0 at sometimes timeout starts while regression in preparation


(p do-not-find-target-location-unattended
   =goal>
      ISA               comprehend-sentence
      attend-to         "location"
      last-loc          =loc
      time-out          nil
      skip              nil
      regression        NIL
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   =visual-location>
      ISA               visual-location
   ?visual-location>
      attended          nil
==>
  !bind! =eye-loc (first (current-eye-loc))
   =goal>
      attend-to         "next-word"
      ; em-state          "free"
   =visual-location>    =loc
)
(spp do-not-find-target-location-unattended :at 0)
(spp do-not-find-target-location-unattended :u 0)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (2) ATTEND to the next word when a visual location is found.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p shift-attention-/-L1
    =goal>
       ISA            comprehend-sentence
       em-state       "looking"
     - state          "stop"
       ; attend-to      "next-word"
    =imaginal>
       ISA            parsing-state
    =visual-location>
       ISA         visual-location
    ; ?visual-location>
    ;    attended    nil
    ?visual>
       execution      free         ;; use this for "saccadic suppression"
;       processor       free
==>
    !bind! =skip  (check-parsed =visual-location)
    ; !bind! =new-state  (parse-or-wait =visual-location)

    =goal>
       ; state          =new-state
       em-state       "attending"
       skip           =skip
       ; last-loc       =visual-location
    =imaginal>
    ;    state          =new-state
    +visual>
       ISA            move-attention
       screen-pos     =visual-location
;   -visual-location>
    =visual-location>
)
(spp shift-attention-/-L1 :at 0)
; (spp shift-attention-/-L1 :u 10)



; (p attend-left
;     =goal>
;        ISA            comprehend-sentence
;        em-state       "looking"
;        ; em-state       "regression"
;        attend-to      "left"
;        ; regression     t
;        ; time-out       T
;      - state          "stop"
;     =imaginal>
;        ISA            parsing-state
;        ; last-retr-loc  =loc
;     =visual-location>
;        ISA            visual-location
; ;    ?visual-location>
; ;       attended    nil
;     ?visual>
;        execution       free   ; use this for "saccadic suppression"
; ;       processor       free
; ==>
;     ; !bind! =new-state  (skip-or-parse =visual-location)
;     ; !bind! =new-state  (parse-or-wait =visual-location)

;     =goal>
;        ; state          =new-state
;        ; em-state       "attending-left"
;        em-state       "free"
;     =imaginal>
;     ;    state          =new-state
;     +visual>
;        ISA         move-attention
;        screen-pos  =visual-location
; ;   -visual-location>
;     =visual-location>    ;=loc
;     ; +visual-location>
;     ;   ISA               visual-location
;     ; > screen-x          700
;     ;   screen-x          lowest

; ;   !eval! (check-if-attached =visual-location)
; )
; (spp attend-left :at 0)
; ; (spp attend-left :u 10)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (3) RETRIEVE lexical entry from memory
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p subvocalize
    =goal>
       ISA          comprehend-sentence
       state        "wm-retrieval"
       em-state     "attending"
       time-out     nil
       attend-to    "next-word"
       ; regression   nil
       skip         nil
       ; unexpected   nil
    =imaginal>
       ISA          parsing-state
    =visual>
       ISA         text
       value       =word
     - value       "*"
    =visual-location>
       ISA         visual-location
    ?vocal>
       preparation free
==>
    !bind! =ip-chunkname (current-ip)
    =goal>
       em-state    "free"
    =imaginal>
       att-obj     =visual
       last-parse-loc  =visual-location
    ; -visual-location>
    =visual-location>
    +vocal>
       ISA         subvocalize
       string      =word
)


(p attend-subvocalized-word
    =goal>
       ISA          comprehend-sentence
       state        "read"
       ; em-state     "attending"
       time-out     nil
       attend-to    "next-word"
       ; regression   nil
       skip         nil
       ; unexpected   nil
    =aural-location>
       ISA          audio-event
==>
    =goal>
       ; state        "listening"
       ; em-state    "free"
    +aural>
       isa          sound
       event        =aural-location
    -aural-location>
)


(p lexical-retrieval-request-of-subvocalization
    =goal>
       ISA          comprehend-sentence
       state        "read"
       ; em-state     "attending"
       time-out     nil
       attend-to    "next-word"
       ; regression   nil
       skip         nil
       ; unexpected   nil
    =imaginal>
       ISA          parsing-state
       att-obj      =vis
    ?aural-location>
       buffer      empty
    =aural>
       isa          sound
       kind         word
       content      =word
     - content      "*"
    ; ?retrieval>
    ;     state       free
==>
    !bind! =ip-chunkname (current-ip)

    =goal>
       state       "lexical-retrieval"
       ; em-state    "free"
       unattached  nil
       cue1        =word
       cue2        nil
       cue3        nil
       cue4        nil
    =imaginal>
       current-ip  =ip-chunkname
       ; att-obj     =visual
       ; last-parse-loc  =visual-location
    -aural>
    ; -visual-location>
    ; =visual-location>
    +retrieval>
       ISA         lexical-entry
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word

    ; !eval! (record-end-time)
    !eval! (set-current-ip)  ;; eventually get rid of
    !eval! (word-message =word)
    !eval! (set-current-wordnr =vis)
    -IPb>
    -CPb>
    -DPb>
    -DP2b>
    -NPb>
    -VPb>
    -VP2b>
    -PPb>
    -AdjPb>
    -AdvPb>
)


(p lexical-retrieval-request-/-L2
    =goal>
       ISA          comprehend-sentence
       state        "read"
       em-state     "attending"
       time-out     nil
       attend-to    "next-word"
       ; regression   nil
       skip         nil
       ; unexpected   nil
    =imaginal>
       ISA          parsing-state
    =visual>
       ISA         text
       value       =word
     - value       "*"
    =visual-location>
       ISA         visual-location
    ?aural-location>
       buffer      empty
    ?vocal>
       state       free
    - last-command subvocalize
    ; ?retrieval>
    ;     state       free
==>
    !bind! =ip-chunkname (current-ip)

    =goal>
       state       "lexical-retrieval"
       em-state    "free"
       unattached  nil
       cue1        =word
       cue2        nil
       cue3        nil
       cue4        nil
    =imaginal>
       current-ip  =ip-chunkname
       att-obj     =visual
       last-parse-loc  =visual-location
    ; -visual-location>
    =visual-location>
    +retrieval>
       ISA         lexical-entry
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word
       word        =word

    ; !eval! (record-end-time)
    !eval! (set-current-ip)  ;; eventually get rid of
    !eval! (word-message =word)
    !eval! (set-current-wordnr =visual)
    -IPb>
    -CPb>
    -DPb>
    -DP2b>
    -NPb>
    -VPb>
    -VP2b>
    -PPb>
    -AdjPb>
    -AdvPb>
)
(spp lexical-retrieval-request-/-L2 :at 0.05)



(p skip-integration
    =goal>
       ISA          comprehend-sentence
       state        "read"
       em-state     "attending"
       time-out     nil
       skip         t
    =imaginal>
        ISA         parsing-state
    ;     state       "skip"
    =visual>
       ISA         text
;       SCREEN-POS  =screen-pos
       value       =word
     - value       "*"
    =visual-location>
       ISA         visual-location
    ?retrieval>
        state       free
==>
    =goal>
       state       "read"
       em-state    "free"
       cue1        nil
       cue2        nil
       cue3        nil
       cue4        nil
    =imaginal>
       att-obj     =visual
    ; -visual-location>
    =visual-location>

    !eval! (skip-message =word)
)
(spp skip-integration :at 0.05)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TIME OUT PRODUCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p start-time-out
    =goal>
       ISA            comprehend-sentence
     ; - state          "read"            ;
       state          "wm-retrieval..."
       em-state       "attending"
       time-out       nil
       attend-to      "next-word"
       regression     nil
       ; last-loc       =loc            ;; use this for regression to previous attention location
   =imaginal>
       ISA            parsing-state
       last-parse-loc  =parse-loc    ;; use this for direct regression to problematic word
   ?visual>
       processor      free              ;; no current encoding
       execution      free              ;; no current saccade execution
       ; preparation       free           ;; no current saccade preparation
    =visual>
       ISA            text
    =visual-location>
       ISA            visual-location
    ?vocal>
       state          busy
    ; ?retrieval>
    ;    state        busy
 ==>
    !bind! =eye-loc (first (current-eye-loc))
    =goal>
       em-state       "looking"
       ; em-state       "regression"
       last-loc       =visual-location
       time-out       t
       ; regression     t
    =imaginal>
    +visual-location>
        ISA           visual-location
      ; < screen-x      current       ;; target before current attended loc (last attended loc)
      < screen-x      =eye-loc       ;; target before current fixation
        screen-x      highest       ;; target nearest to the left
          ; :attended       t             ;; target one of last two attended (with :visual-num-finsts 3)

   !eval! (start-time-out =parse-loc)
   !eval! (trialmessage "timeout" =eye-loc)
)
(spp start-time-out :at 0)


(P exit-time-out
   =goal>
      ISA             comprehend-sentence
      time-out        t
      em-state        "attending"
      state           "read"
      ; attend-to       "next-word"
      ; regression      nil
      ; last-loc        =aloc
      ; last-parse-loc  =ploc
      ; last-parse-loc       =parse-loc
   ; =imaginal>
   ;     ISA            parsing-state
    ?retrieval>
       state        free
 ==>
   =goal>
      time-out        nil
      ; em-state        "looking"
      em-state        "free"
   ; =imaginal>
   ; =visual-location> =ploc  ;; VERSION 1: back to last lex-retrieved word (n+1)
;   +visual-location>      ;; VERSION 2: back to normal reading
;      ISA               visual-location
;    > screen-x          current
;      screen-x          lowest
   
   !eval! (exit-time-out)
)
(spp exit-time-out :at 0.00)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REGRESSIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (p targeted-regression
;    =goal>
;        ISA          comprehend-sentence
;        ; state        "read"
;        ; em-state     "attending"
;        time-out     nil
;        skip         nil
;      - attend-to    "next-word"
;      - attend-to    "left"
;      - attend-to    "word"
;        attend-to    =target-pos
;     =imaginal>
;        ISA          parsing-state
;     ; =visual>
;     ;    ISA         text
;     ;    value       =word
;     ;  - value       "*"
;     =visual-location>
;        ISA         visual-location
;     ; ?retrieval>
;         ; state       free
;     ?visual>
;        ; processor         free    ;; no current encoding
;        execution         free    ;; no current saccade execution
;        ;preparation       free    ;; no current saccade preparation
;  ==>
;   !bind! =eye-loc (first (current-eye-loc))
;   !bind! =target-loc (get-target-loc =eye-loc =target-pos)
;     =goal>
;        em-state       "looking"
;        ; unexpected     nil
;        attend-to     "left"
;        last-loc       =visual-location
;        ; time-out       t
;     =imaginal>
;        ; att-obj        =visual
;        ; last-obj     =visual
;     +visual-location>
;          ISA         visual-location
;        <= screen-x    =target-loc
;        ; < screen-x    =eye-loc ;; target before current fixation
;          screen-x    highest
;         ; :attended   nil

;    !eval! (report-regression =visual-location =target-pos =target-loc)
; )
; (spp targeted-regression :at 0)
; (spp targeted-regression :u 10)



; (p fail-at-unexpected-word
;    =goal>
;        ISA          comprehend-sentence
;        ; state        "read"
;        em-state     "attending"
;        time-out     nil
;        skip         nil
;        regression   nil
;        unexpected   t
;     =imaginal>
;        ISA          parsing-state
;     ; =visual>
;     ;    ISA         text
;     ;    value       =word
;     ;  - value       "*"
;     =visual-location>
;        ISA         visual-location
;     ?retrieval>
;         state       free
;     ?visual>
;        ; processor         free    ;; no current encoding
;        execution         free    ;; no current saccade execution
;        ;preparation       free    ;; no current saccade preparation
;  ==>
;   !bind! =eye-loc (first (current-eye-loc))
;     =goal>
;        em-state       "looking"
;        unexpected     nil
;        ; regression     t
;        last-loc       =visual-location
;        ; time-out       t
;     =imaginal>
;        ; att-obj        =visual
;        ; regression   t
;        ; unexpected   nil
;        ; word         =word
;        ; last-obj     =visual
;     +visual-location>
;          ISA         visual-location
;        < screen-x    current ;; target before current attended loc (last attended loc)
;        < screen-x    =eye-loc ;; target before current fixation
;          ; screen-x    highest  ; always target word n
;         ; :attended   nil

;    !eval! (start-fail =visual-location)
; )
; (spp fail-at-unexpected-word :at 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OTHER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p stop-reading
    =goal>
       ISA         comprehend-sentence
       em-state    "attending"
       state       "read"
    =visual>
       ISA         text
       value       "*"
==>
    =goal>
       state       "stop"
       em-state    "stop"
       cue1         nil
       cue2         nil
       cue3         nil
       cue4         nil
       ; !eval! (word-message "*")
    -IPb>
    -CPb>
    -DPb>
    -NPb>
    -VPb>
    -VP2b>
    -PPb>
    -AdjPb>
    -AdvPb>
    -retrieval>

    ; !eval! (record-end-time)
    !output! "SENTENCE PARSED SUCCESSFULLY"
)
(spp stop-reading :at 0.05)
(spp stop-reading :u 10)



(p find-loc-failure
    =goal>
       ISA          comprehend-sentence
      ; - em-state     "free"
        ; last-loc    =last-loc
   ?visual-location>
       error            t
   ?visual>
       processor       free
       execution       free
 ==>
    ; =visual-location>  =last-loc
    +visual-location>
         ISA         visual-location
       ; < screen-x    current
         screen-x    lowest
   =goal>
       em-state       "looking"
)
