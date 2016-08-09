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

(clear-all)


(defparameter *record-visloc-activations* nil)

(define-model parser

;;; S E T   G E N E R A L   P A R A M E T E R S
(sgp
  ;;; --------------------------------
  ;;; PRINTING
  ;;; --------------------------------
    :CBCT                        NIL   ; default: NIL        : Whether or not to show an event in the trace when a buffer copies a chunk
    :CMDT                        T     ; default: T          : Commands trace controls output of commands
    :MODEL-WARNINGS              nil     ; default: T          : Whether to output model warnings
    :TRACE-DETAIL                low  ; default: MEDIUM     : Determines which events show in the trace
    :TRACE-FILTER                nil   ; default: NIL        : Function to limit output shown in the trace
    ; :TRACE-FILTER                production-firing-only   ; default: NIL        : Function to limit output shown in the trace
    :V                           T     ; default: T          : Verbose controls model output
    :BUFFER-TRACE                nil     ; default: NIL        : Display the trace as a buffer summary instead of as an event list.
    :TRACED-BUFFERS              (goal production retrieval visual grammatical lexical ipb vpb npb imaginal)     ; default: T          : The list of buffers to be traced (all buffers if set to t).
    :GRAPHIC-COLUMN-WIDTHS       (50 200 100 50 100 60 60 60 60 50)      ; default: NIL        : The pixel width of the columns drawn for the buffers using the graphic tracing tool.    
    ; :act t        ; activation trace
    ; :pct t        ; production compilation trace
    ; :BOLD-INC 0.2
    ; :BOLD-SETTLE 1


  ;;; GLOBAL
    :dat 0.01          ; default action time (time in s for a production to fire) (default: 0.05; Salvucci2001: 0.01)
    :esc t             ; enable subsymbolic computation
    :ga 1              ; goal activation
    :er t              ; Enable Randomness (to break “ties” during conflict resolution and retrievals)
    :randomize-time t  ; Used mainly by the perceptual and motor modules to add noise to the action times
    :ncnar t         ; Normalize chunk names. Switch off for better performance.
    :dcnn nil            ; Dynamic chunk name normalizing during run time. Switch off for better performance.
    :short-copy-names t
    ; :use-tree T         ; Controls whether a decision tree is created to use during production matching. This should result in a faster model run time.

  ;;; VISUAL + EMMA-P
    :VISUAL-ENCODING-FACTOR    0.002
    :VISUAL-ENCODING-EXPONENT  0.4
    :SACCADE-PREPARATION-TIME  0.110
    :FIXED-PREP-TIME           T
    :EYE-SACCADE-RATE          0.002
    :SACCADE-BASE-TIME         0.020
    :vis-obj-freq              0.01
    :show-focus t                    ; show attention focus
    ; :visual-attention-latency .085  ; time of visual attention shift in seconds
    ; :visual-finst-span 3.0          ; how long a finst marker will remain on a location (in seconds) (default: 3)
    ; :visual-num-finsts 3             ;; how many finsts are available to the vision module (default: 4)
    :visual-onset-span .02           ;; specifies how long an item recently added to the visicon will 
                                     ;;   be marked as new and how long a scene change notice will be 
                                     ;;   available (in seconds) (default: 0.5)

  ;;; PARSING
    :SURPRISAL-FACTOR          0.005
    :SURPRISAL-HL-FACTOR       2

  ;;; RETRIEVAL LATENCY FACTOR F
  ;;; (0.14 for for LV2005, VL2006) (0.46 for VBRD2008) (0.26 NPI ACTR6 replication)
     ; :lf .46 ; (VBRT2008)
     ; :lf .45 ; (was used in Raluca's model)
    ; :lf .26 ; (NPI ACTR6 replication)
    :lf .2
    ; :lf .14 ; (LV2005, VL2006)

  ;;; RETRIEVAL THRESHOLD T  (-1.5 for for LV2005, VL2006, VBRD2008)
    ; :rt -.5        ; retrieval threshold T
    ; :rt -1.0
    :rt -1.5

  ;;; ACTIVATION
   ;; Decay parameter (0.5 for LV2005, VL2006, VBRD2008; 2 was used in Raluca's  model)   
    :bll 0.5        ; base-level learning decay parameter (d)
    :ol T         ; use full learning rule, not approximation
    ; :egs 3         ; expected gain s. The s parameter for the noise added to utility values.

   ;; Activation noise settings
   ;; [epsilon] (0 and 0.15 for for LV2005, VL2006) (0.15, 0.30, 0.45 for VBRD2008)
    ; :ans nil
    :ans .15 

   ;; Partial Matching
    ; :mp  3         ; match scale (mismatch penalty parameter)
    :mp  1.5
    :md -.6         ; maximum difference penalty  (-0.6 for for LV2005, VL2006, VBRD2008)

   ;; Maximum associated strengh S (1.5 for LV2005, VL2006, VBRD2008) 
    ; :mas 3         ; maximum associative strength S (3 was used in Sternberg model)
    :mas 1.5
    ; :mas 0


;;; --------------------------------
;;; PRODUCTION-HISTORY module
;;; --------------------------------
    :DRAW-BLANK-COLUMNS          T ; default: T          : Whether or not to draw the columns which have no matched productions.
    :P-HISTORY-GRAPH-X           40  ; default: 40         : Horizontal pixels between production boxes.
    :P-HISTORY-GRAPH-Y           90  ; default: 90         : Vertical pixels between production boxes.
    :SAVE-P-HISTORY              T   ; default: NIL        : Whether or not to record the utility and whynot history of all conflict-resolution events.

    :SAVE-BUFFER-HISTORY         T ; default: NIL        : Whether or not to record the history of buffer changes.

) 


;;; Set-visloc-default is used to set the values of the properties which are used when testing the items in 
;;;  the visicon to determine if a visual-location chunk should be stuffed into the visual-location buffer for 
;;;  the current model.
; (set-visloc-default isa visual-location screen-x lowest :attended new)


(if *params* (sgp-fct *params*))
(setf *actr-enabled-p* t)

)

(reset)


