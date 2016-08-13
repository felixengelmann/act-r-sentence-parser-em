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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p find-first-word
   =goal>
      ISA               comprehend-sentence
      state             "start"
   ; =imaginal>
   ;    ISA               parsing-state
   =visual-location>
      ISA               visual-location
==>
   =goal>
      em-state          "looking"
      state             "read"
      attend-to         "next-word"
      loc               =visual-location
      last-loc          =visual-location
      time-out          nil
   ; +visual-location>
   ;    ISA               visual-location
   ;    screen-x          lowest
   ;    ; :attended         NIL
   =visual-location>
   ; =imaginal>
   -visual>
)
(spp find-first-word :at 0.05)


;;;
;;;  (1) Find location of next word when the current word is integrated.
;;;      More specifically, get the nearest visual location to
;;;      the right, and direct attention to that location.
;;;
(p find-next-word
   =goal>
      ISA               comprehend-sentence
    - state             "lexical-retrieval"
      em-state          "free"
      attend-to         "next-word"
      time-out          nil
   ?visual>
      ; buffer            empty
      processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   ?visual-location>
      buffer            empty
==>
   +visual-location>
      ISA               visual-location
    > screen-x          current
      screen-x          lowest
   =goal>
      em-state          "looking"
      regression        nil
   -visual>
)
(spp find-next-word :at 0.05)
; (spp find-next-word :u 20)



;;; 
;;;  (2) ATTEND to the next word when a visual location is found.
;;;
(p shift-attention
    =goal>
       ISA            comprehend-sentence
     - state          "stop"
       em-state       "looking"
     ; - attend-to      "location"
       regression       nil
    =visual-location>
       ISA            visual-location
    ?visual>
       buffer         empty
       execution      free         ;; use this for "saccadic suppression"
;       processor       free
==>
    !bind! =skip  (check-parsed =visual-location)

    =goal>
       em-state       "attending"
       skip           =skip
    +visual>
       ISA            move-attention
       screen-pos     =visual-location
    ; =visual-location>
    ;; SWIFT:
    !eval! (request-swift-state)
)
(spp shift-attention :at 0)
; (spp shift-attention :u 10)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (3) RETRIEVE lexical entry from memory
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p lexical-retrieval-request
    =goal>
       ISA          comprehend-sentence
       state        "read"
       em-state     "attending"
       attend-to    "next-word"
       loc          =last-loc
       time-out     nil
       skip         nil
       ; regression   nil
    ; =imaginal>
       ; ISA          parsing-state
    =visual>
       ISA         text
       value       =word
     - value       "*"
       screen-pos  =visloc
    ; =visual-location>
    ;    ISA         visual-location
    ;; SWIFT:
    !eval! (recognition-complete =visloc)
==>
    ; !bind! =ip-chunkname (current-ip)

    =goal>
       state       "lexical-retrieval"
       em-state    "free"
       attend-to   "next-word"
       last-loc    =last-loc
       loc         =visloc
       word        =word
       unattached  nil
       cue1        =word
       cue2        nil
       cue3        nil
       cue4        nil
    ;; will go into parsing module handled automatically by (set-begin-time)
    ; =imaginal>
       ; word        =word
       ; att-obj     =visual
       ; parse-loc   =visloc
       ; current-ip  =ip-chunkname
    =visual>

    ; +retrieval>
    ;    ISA         lexical-entry
    +lexical>
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

    !eval! (parsing-set-current-ip)
    !eval! (word-message =word)
    !eval! (register-new-visloc =visloc)
    !eval! (increase-ref-count =visloc)
    ; !eval! (set-current-wordnr =visual)
    -structural>
    -structural2>
    -grammatical>
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
(spp lexical-retrieval-request :at 0.05)


;; SWIFT:
(p encode-word
    =goal>
       ISA          comprehend-sentence
       state        "read"
       em-state     "attending"
       attend-to    "next-word"
       loc          =last-loc
       time-out     nil
       skip         nil
    =visual>
       ISA         text
       value       =word
     - value       "*"
       screen-pos  =visloc
    !eval! (not (recognition-complete =visloc))
==>
    =goal>
    =visual>
    !eval! (request-swift-state)
)
(spp encode-word :at 0.10)


(p skip-integration
    =goal>
       ISA          comprehend-sentence
       state        "read"
       em-state     "attending"
       time-out     nil
       skip         t
    ; =imaginal>
    ;     ISA         parsing-state
    ;     state       "skip"
    =visual>
       ISA         text
;       SCREEN-POS  =screen-pos
       value       =word
     - value       "*"
    ?grammatical>
        state       free
==>
    =goal>
       state       "read"
       em-state    "free"
       attend-to   "next-word"
       target      nil
       cue1        nil
       cue2        nil
       cue3        nil
       cue4        nil
    ; =imaginal>
    ;    att-obj     =visual
    =visual>

    !eval! (parsing-skip-message =word)
)
(spp skip-integration :at 0.05)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REGRESSIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TARGETED REGRESSIONS
(p plan-targeted-regression
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "regression"
      attend-to         "location"
      target            =loc
      ; time-out          nil
      ; skip              nil
      regression        nil
   ?visual>
      execution         free    ; no current saccade execution
   ?visual-location>
      buffer            empty
==>
   =goal>
      em-state          "regression"
      ; attend-to         "next-word"
      regression        t
      time-out          nil
      skip              nil
   =visual-location>    =loc
)
(spp plan-targeted-regression :at 0.01)
; (spp plan-targeted-regression :u 0.25)
(spp-fct (list 'plan-targeted-regression ':u (car (no-output (sgp :regr-util)))))


(p no-regression
   =goal>
      ISA               comprehend-sentence
      attend-to         "location"
      target            =loc
      regression        nil
   ?visual>
      execution         free    ; no current saccade execution
   ?visual-location>
      buffer            empty
==>
   =goal>
      attend-to         "next-word"
      target            nil
)
(spp no-regression :at 0)
(spp no-regression :u 0)



(p find-target-location-attended
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      em-state          "regression"
      attend-to         "location"
      time-out          nil
      skip              nil
      regression        t
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
   =visual-location>
      
   !eval! (trialmessage "regression" "attended-loc")
)
(spp find-target-location-attended :at 0.0)
; (spp find-target-location-attended :u 10)


(p reread-sentence
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      em-state          "regression"
      attend-to         "location"
      time-out          nil
      skip              nil
      regression        t
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
   +visual-location>
      ISA               visual-location
      < screen-x        =eye-loc
      screen-x          lowest
    !eval! (trialmessage "regression" "reread")
    ;; SWIFT:
    !eval! (reset-all-processing-states)
)
(spp reread-sentence :at 0.05)
;; with 0 at sometimes timeout starts while regression in preparation
; (spp reread-sentence :u 0.25)




;;; RANDOM REGRESSION
(p find-previous-location
   =goal>
      ISA               comprehend-sentence
    ; - state             "lexical-retrieval"
      ; em-state          "free"
      last-loc          =last-loc
      attend-to         "left"
      time-out          nil
      skip              nil
   ; =imaginal>
   ;    ISA          parsing-state
   ?visual>
      ; processor         free    ; no current encoding
      execution         free    ; no current saccade execution
   ; =visual-location>
   ;    ISA               visual-location
==>
   !bind! =eye-loc (first (current-eye-loc))
   !bind! =last-x (chunk-slot-value-fct =last-loc 'screen-x)
   ; !bind! =target-loc (get-target-loc =eye-loc =target-pos)
   =goal>
      em-state          "looking"
      attend-to         nil
      ; last-loc          =visual-location
      ; time-out       t
    ; =imaginal>
       ; att-obj        =visual
       ; last-obj     =visual
    +visual-location>
         ISA         visual-location
       < screen-x    =eye-loc   ; target before current fixation
      <= screen-x    =last-x
         screen-x    highest
        ; :attended   T
    -visual>
   ; !eval! (report-regression =visual-location =target-pos =target-loc)
   !eval! (trialmessage "regression" "left")
   !eval! (increase-ref-count =last-loc)
   ;; SWIFT:
   !eval! (reset-word-processing-state (- (parsing-get-index) 1))
)
(spp find-previous-location :at 0)
(spp find-previous-location :u 10)



;;; INDEX-TARGETED REGRESSION
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
   ; =imaginal>
   ;    ISA          parsing-state
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
    ; =imaginal>
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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TIME OUT PRODUCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p start-time-out
    =goal>
       ISA            comprehend-sentence
     ; - state          "read"            ;
       state          "wm-retrieval"
       em-state       "attending"
       attend-to      "next-word"
       time-out       nil
       regression     nil
       ; last-loc       =loc            ;; use this for regression to previous attention location
   ?visual>
       ; processor      free              ;; no current encoding
       execution      free              ;; no current saccade execution
       ; preparation       free           ;; no current saccade preparation
    =visual>
       ISA            text
       screen-pos     =visual-location
    ; =visual-location>
    ;    ISA            visual-location
    ; ?retrieval>
    ;    state        busy
 ==>
    !bind! =eye-loc (first (current-eye-loc))
    !bind! =parse-loc (parsing-get-loc)
    =goal>
       em-state       "looking"
       ; em-state       "regression"
       last-loc       =visual-location
       time-out       t
       ; regression     t
    +visual-location>
        ISA           visual-location
      < screen-x      current       ;; target before current attended loc (last attended loc)
      < screen-x      =eye-loc       ;; target before current fixation
        screen-x      highest       ;; target nearest to the left
          ; :attended       t             ;; target one of last two attended (with :visual-num-finsts 3)

   !eval! (start-time-out =parse-loc)
   !eval! (trialmessage "timeout" =eye-loc)
   ;; SWIFT:
   !eval! (reset-word-processing-state (parsing-get-index))
)
(spp start-time-out :at 0)
;; set to 0 when using fixation-relative regressions


; ;; Exit time-out VERSION I: free state
; (P exit-time-out
;    =goal>
;       ISA             comprehend-sentence
;       time-out        t
;       em-state        "attending"
;       state           "read"
;       ; attend-to       "next-word"
;       ; regression      nil
;       last-loc        =last-loc
;       ; last-parse-loc  =ploc
;       ; last-parse-loc       =parse-loc
;    ; =imaginal>
;    ;     ISA            parsing-state
;     ; ?visual>
;     ;    processor      free
;     ?grammatical>
;        state        free
;  ==>
;    =goal>
;       time-out        nil
;       em-state        "free"
;   -visual-location>
;   -visual>   
;    !eval! (exit-time-out)
; )
; (spp exit-time-out :at 0.00)



; ; Exit time-out VERSION II: back to last lex-retrieved word (n+1)
; (P exit-time-out
;    =goal>
;       ISA             comprehend-sentence
;       time-out        t
;       em-state        "attending"
;       state           "read"
;       last-loc        =last-loc
;     ?grammatical>
;        state        free
;  ==>
;    =goal>
;       time-out        nil
;       em-state        "looking"
;    =visual-location> =last-loc
;   -visual>
;    !eval! (exit-time-out)
; )
; (spp exit-time-out :at 0.00)



; Exit time-out VERSION III: looking for next word
(P exit-time-out
   =goal>
      ISA             comprehend-sentence
      time-out        t
      em-state        "attending"
      state           "read"
      last-loc        =last-loc
    ?grammatical>
       state        free
 ==>
   =goal>
      time-out        nil
      em-state        "looking"
  +visual-location>
     ISA               visual-location
  ;  > screen-x          =last-loc
   > screen-x          current
     screen-x          lowest
  -visual>
   !eval! (exit-time-out)
)
(spp exit-time-out :at 0.00)




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
    -grammatical>

    !output! "SENTENCE PARSED SUCCESSFULLY"
    ; !stop!
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
   -visual>
)
