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

(defvar *current-ip* '() "holds stack of current ips")
; (defvar *copied-chunks* '() "holds names of chunks copied to retrieval buffer")
(defvar *goal-cat-mappings*)
(defvar *wait-for-cat-mappings*)
(defvar *cat-goal-cat-mappings*)
(defvar *transitive-mappings*)


;; The new WM buffers for creating constituents.
;; still needed???
(setf *PP* nil)
(setf *IP* nil)
(setf *CP* nil)
(setf *DP* nil)
(setf *DP2* nil)
(setf *NP* nil)
(setf *VP* nil)  
(setf *VP2* nil)  
(setf *AdjP* nil)  
(setf *lex* nil)




(setf *sentences* nil)
(setf *current-ip* '())
(setf *copied-chunks* '())



;;;
;;; GOAL-CATEGORY MAPPINGS
;;;
(setf *goal-cat-mappings* '(
          (next-*done* . *done*)
			    (next-vp-goal . vp-goal)
			    (next-np-goal . np-goal)
          (next-dp-goal . dp-goal)
			    (next-vp-embedded-goal . vp-embedded-goal)
			    (next-vp-gapped-goal . vp-gapped-goal)
			    ;; added, DISCUSS and let CONFIRM!!!
                            (next-ip-goal . ip-goal)
			    (next-ip-embedded-goal . ip-embedded-goal)
			    (next-ip-gapped-goal . ip-gapped-goal)
))

(setf *cat-goal-cat-mappings* '(
          (next-*done* . *done*)
          (vp . vp-goal)
          (np . np-goal)
          (dp . dp-goal)
))

(setf *wait-for-cat-mappings* '(
          (vp . wait-for-vp)
          (np . wait-for-np)
          (dp . wait-for-dp)
          (ip . wait-for-ip)
          (v . wait-for-v)
          (n . wait-for-n)
          (det . wait-for-det)
))

(setf *transitive-mappings* '(
          (transitive-CP-finite-lex .  CP)
          (transitive-DP-lex . DP)
          (transitive-NP-lex . NP)
          (past-participle-transitive-lex . I-V)
))



(defun map-goal-category (next-goal)
  (when *VERBOSE* (model-warning " +++ Setting goal to ~A. +++" next-goal))
  (cdr (assoc next-goal *goal-cat-mappings*))
)

(defun map-next-goal-category (goal)
  (when *VERBOSE* (model-warning " +++ Setting next-goal to ~A. +++" goal))
  (car (rassoc goal *goal-cat-mappings*))
)

(defun map-cat-to-wait-for-cat (cat)
  (cdr (assoc cat *wait-for-cat-mappings*))
)

(defun map-lex-cat-to-wait-for-cat (cat-lex)
  (map-cat-to-wait-for-cat (car (rassoc cat-lex *lex-mappings*)))
)

(defun map-goal-cat-to-cat (goal-cat)
  (car (rassoc goal-cat *cat-goal-cat-mappings*))
)

(defun map-cat-to-goal-cat (cat)
  (cdr (assoc cat *cat-goal-cat-mappings*))
)

(defun map-lex-cat-to-goal-cat (cat-lex)
  (cdr (assoc (map-lex-to-syn cat-lex) *cat-goal-cat-mappings*))
)

(defun map-transitive-cat (cat)
  (cdr (assoc cat *transitive-mappings*))
)



;;;
;;; KEEPING TRACK OF C-COMMANDERS
;;;
;; NOTE: Move to parsing module?
(defun push-clause nil
  (let* ((imchunk (buffer-read 'imaginal))
         (stack (chunk-slot-value-fct imchunk 'clause-id-stack))
         (id (new-name c)))
    (push id stack)
    (mod-chunk-fct imchunk (list 'clause-id-stack stack))
    id))

(defun pop-clause nil
  (let* ((imchunk (buffer-read 'imaginal))
         (stack (chunk-slot-value-fct imchunk 'clause-id-stack)))
    (pop stack)
    (mod-chunk-fct imchunk (list 'clause-id-stack stack))
    (car stack)
    ))

(defun current-clause nil
  (let* ((imchunk (buffer-read 'imaginal))
         (stack (chunk-slot-value-fct imchunk 'clause-id-stack))
         (id (car stack)))
    id))



;;;
;;; CURRENT IP MAINTENANCE FUNCTIONS
;;;
(defun current-ip-old nil
  (let* ((ipchunk (buffer-read 'IPb))
         (imchunk (buffer-read 'imaginal)))
    (if ipchunk (progn (model-warning " +++ Setting current IP chunk +++")
                       ipchunk)
        (chunk-slot-value-fct imchunk 'current-ip))
    ))

(defun set-current-ip-old nil
  (let* ((ipchunk (buffer-read 'IPb)))
    (if ipchunk (progn (model-warning " +++ Setting current IP chunk *2* +++")
                       (setf *current-ip* ipchunk)))
    ))


(defun set-current-ip nil
  (let* ((ipchunk (buffer-read 'IPb)))
    (if ipchunk (progn (model-warning " +++ Setting current IP chunk +++")
                       (push ipchunk *current-ip*)))
    ))


(defun parsing-set-current-ip (ipchunk)
  (if ipchunk (progn (model-warning " +++ Setting current IP chunk +++")
                     (push ipchunk *current-ip*)))
  )


(defun current-ip nil
  (let* ((ipchunk (buffer-read 'IPb)))
    (if ipchunk 
        ipchunk
        (car *current-ip*))
    ))

(defun mod-current-ip (modlist)
  (let* ((current-ip (car *current-ip*)))
    (mod-chunk-fct current-ip modlist)
    (model-warning " +++ Modifying IP +++")
    ))

(defun read-current-ip-slot (slot)
  (let* ((current-ip (car *current-ip*)))
    (chunk-slot-value-fct current-ip slot)
   ))

(defun pop-current-ip nil
  (pop *current-ip*)
  )


(defun pretty-ip nil
  (let* ((current-ip (car *current-ip*))
         (dpc (chunk-slot-value-fct current-ip 'spec))
         (vpc (chunk-slot-value-fct current-ip 'comp))
         (dp (create-chunk-alias-fct dpc 'SUBJECT-DP))
         (vp (create-chunk-alias-fct vpc 'VP1))
         (dp2c (chunk-slot-value-fct vpc 'comp))
         (dp2 (create-chunk-alias-fct dp2c 'OBJECT-DP)))
    (mod-chunk-fct current-ip (list
                                'spec dp
                                'comp vp))
    (mod-chunk-fct vpc (list
                                'comp dp2))
    (model-warning " +++ Modifying IP +++")
    ))



; (defun read-slot (buffer slot)
;   (chunk-slot-value-fct buffer slot)
;   )

; (defun mod-bufferchunk (chunk modlist)
;   (mod-chunk-fct chunk modlist)
;   (model-warning " +++ Modifying buffer +++")
;   )



(defun mod-current-ip-old2 (modlist)
  (let* ((imchunk (buffer-read 'imaginal))
         (current-ip (chunk-slot-value-fct imchunk 'current-ip)))
    (mod-chunk-fct current-ip modlist)
    (model-warning " +++ Modifying IP +++")
    ))

(defun read-current-ip-slot-old (slot)
  (let* ((imchunk (buffer-read 'imaginal))
         (current-ip (chunk-slot-value-fct imchunk 'current-ip)))
    (chunk-slot-value-fct current-ip slot)
   ))

; OLD
(defun mod-current-ip-OLD (modlist)
  (mod-chunk-fct *current-ip* modlist)
  (model-warning " +++ Modifying IP +++")
  )








;;;
;;; PARSING STATE MAINTENANCE
;;;

; ;; temporary:
; (defun parsing-get-loc nil
;   (let* ((imchunk (buffer-read 'imaginal))
;          (loc (chunk-slot-value-fct imchunk 'parse-loc)))
;     loc
;     ))

;;; obsolete
; (defun set-current-wordnr (text-obj)
;   (let* ((loc (obj->location text-obj))
;          (index (location->index loc *sentence-plist*)))
;     (setf *current-index* index)
;     ))


; (defun set-current-wordnr (text-obj)
;   (let* ((loc (obj->location text-obj))
;          (index (location->index loc *sentence-plist*)))
;     (setf *current-index* index)
;     ; (if (mymember index *attached-positions*)
;     ;     (mod-focus state "read")
;         ; )
;         ))

;; TODO: Move to parsing module
;; use (set-begin-time word visloc)
(defun set-begin-time (word)
  (let* (
         ; (time (- (mp-time) (car (no-output (sgp :dat)))))
         (imchunk (buffer-read 'imaginal))
         ; (vischunk (buffer-read 'visual))
         ; (parse-loc (chunk-slot-value-fct (buffer-read 'visual) 'screen-pos))
         (parse-loc (chunk-slot-value-fct imchunk 'parse-loc))
         (index (visloc->index parse-loc)))
    ; (setf *begin-time* time)  ; includes firing time of set-cues production
    ; (setf *word* word)
    ; (setf *current-index* index)
    (parsing-set-begin-time word index parse-loc)
    ; (schedule-event-relative 0 'word-message :module 'parsing :params (list word) :priority :max)
    ; (schedule-event-relative 0 'word-message :params (list word) :module 'parsing :output 'low :details (format nil "BEGIN INTEGRATION OF ~s" word) :priority :max)
    ; (command-output 
    ;   "*** ~6,3F   PARSING                INTEGRATING: ~s" 
    ;   (mp-time) word)
    ))


; (defun set-begin-time (word)
;   (let* ((imchunk (buffer-read 'imaginal))
;          (vischunk (chunk-slot-value-fct imchunk 'att-obj))
;          (visloc (chunk-slot-value-fct vischunk 'screen-pos)))
;     ; (setf *begin-time* (mp-time))
;     (setf *begin-time* (- (mp-time) (car (no-output (sgp :dat)))))  ; includes firing time of set-cues production
;     (setf *word* word)
;     (set-current-wordnr vischunk)
;     (mod-chunk-fct imchunk (list 'parse-loc visloc))
;     ))


;; TODO: Move to parsing module
(defun set-end-time ()
  ; (setf *end-time* (mp-time))
  ; (setf *attach-time* (round (* 1000 (- (mp-time) *begin-time*))))
  ; )
  ; (let ((attach-time (round (* 1000 (- (mp-time) *begin-time*)))))
;     (when *VERBOSE*
;       (command-output 
; "---------------------------------------------------------------------------
; +++  Total attachment time for ~A: ~D  +++
; ---------------------------------------------------------------------------" 
;      *word* attach-time))
    ; (push-last attach-time *attach-times*)
    ; (push-last (list *current-index* *word* attach-time)  *attached-items*)
    ; (push-last *current-index* *attached-positions*)
    ; (update-attached-pos *current-index*)
    (parsing-set-end-time)
    ; (mod-chunk-fct imchunk (list 'attached-pos (push-last *current-index* poslist)))
    )
  ; )


(defun set-end-time-abort ()
;   (setf *end-time* (mp-time))
;   ; (setf *attach-time* (round (* 1000 (- (mp-time) *begin-time*))))
;   ; )
;   (let ((attach-time (round (* 1000 (- (mp-time) *begin-time*)))))
;     (when *VERBOSE*
;       (command-output 
; "---------------------------------------------------------------------------
; +++  Attachment ABORTED for ~A: ~D  +++
; ---------------------------------------------------------------------------" 
;      *word* attach-time))
;     (push-last attach-time *attach-times*)
    (parsing-set-end-time-abort)
    ; (push-last (list *current-index* *word* attach-time)  *attached-items*)
    ; (push-last *current-index* *attached-positions*)
    ; (update-attached-pos *current-index*)
    ; (mod-chunk-fct imchunk (list 'attached-pos (push-last *current-index* poslist)))
    ; )
  )


; (defun record-end-time ()
;   ; (setf *end-time* (mp-time))
;   ; (let ((attach-time (round (* 1000 (- (mp-time) *begin-time*)))))
;   (unless (eq *current-index* nil)  
;     (let ((attach-time *attach-time*))
;       (when *VERBOSE*
;         (command-output 
;   "     ===========================================
;           TOTAL attachment time for ~A: ~6,3F
;        ===========================================" 
;        *word* attach-time))
;       (push-last attach-time *attach-times*)
;       (push-last (list *current-index* *word* attach-time) *attached-items*)
;       (push-last *current-index* *attached-positions*)
;       (update-attached-pos *current-index*)
;       ;(mod-chunk-fct imchunk (list 'attached-pos (push-last *current-index* poslist)))
;   )))

(defun update-attached-pos (index)
  (let* ((imchunk (buffer-read 'imaginal))
         (poslist (chunk-slot-value-fct imchunk 'attached-pos)))
    (mod-chunk-fct imchunk (list 'attached-pos (push-last index poslist)))
    ))


(defun check-parsed (visloc)
  (let* ((loc (visloc->location visloc))
         (index (location->index loc *sentence-plist*))
         ; (goalchunk (buffer-read 'goal))
         (imchunk (buffer-read 'imaginal))
         (poslist (chunk-slot-value-fct imchunk 'attached-pos))
         ; (timeout (chunk-slot-value-fct goalchunk 'time-out))
         )
    ; (unless timeout
      (if (or (mymember index poslist) (eq (parsing-get-index) index))
          t
          nil);)
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FAKE RETRIEVAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start-fake-retrieval (obj)
  (let* ((lf (get-par :lf))
  ;(retr (retrieval obj))
  ;(integr (* *integration-factor* (retrieval obj) (surprisal-hl obj)))   ;; retrieval modulated by surprisal
        (integr (+ (* lf (retrieval obj)) (surprisal-hl obj)))   ;; retrieval modulated by surprisal
        ; (pos (location->index (obj->location obj) *sentence-plist*))
        (word (word-name (obj->wordinfo obj *sentence-plist*))))
    ; (if (mymember (+ 1 pos) *attached-items*) (setf integr 0.001))  ; check if word is already integrated
    (if (< integr 0.001) (setf integr 0.001))
    (command-output "   > Retrieval Time (~S): ~6,3F" word integr)
    (mod-focus retrieval "busy")
    (schedule-event-relative integr 'finish-fake-retrieval :priority :max :params nil :output 'medium :module 'parsing 
                           :details (concatenate 'string "Retrieval-finished " (write-to-string obj))
                           )))


(defun finish-fake-retrieval ()
  (mod-focus retrieval "free")
  ; (set-end-time)
)


(defun start-fake-lex-retrieval (word)
  (let* ((lf (get-par :lf))
         (encdur (third (car (last *encoded-items*))))
         (lex-retr (* lf (/ encdur 1000 2))))
    (if (< lex-retr 0.001) (setf lex-retr 0.001))
    (command-output "   > Lexical Retrieval / L2 (~S): ~6,3F" word lex-retr)
    ; (mod-chunk-fct (buffer-read 'lex) (list 'word nil))
    (schedule-event-relative lex-retr 'finish-fake-lexical-retrieval :priority :max :params (list word) :output 'medium :module 'parsing 
                           :details (concatenate 'string "Lexical-retrieval-finished " (write-to-string word))
                           )))

(defun finish-fake-lexical-retrieval (word)
  (mod-chunk-fct (buffer-read 'lex) (list 'word word))
)





;;;
;;; MESSAGES
;;;
(defun trialmessage (var val)
  (push-last (list *experiment* *simulation* *item* (1+ (parsing-get-index)) (parsing-get-word) var val) *trialmessages*)
  (command-output 
    "*** ~6,3F   *** TRIALMESSAGE ***   ~s at ~s: ~s" 
    (mp-time) var (parsing-get-word) val)
)


(defun word-message (word)
  (when *VERBOSE* 
    (command-output 
      "*** ~6,3F   Word ~s ready for integration" 
      (mp-time) word))
  )


; (defun skip-message (word)
;   (when *VERBOSE* 
;     (command-output 
;       "+++  Word ~s already attached - Skipping integration!  +++" 
;       word))
;   )



(defun attach-message (head relation dependent)
  (when *VERBOSE* (command-output 
"+++  Relation: ~s is ~s of ~s  +++" 
     dependent relation head))
  )

(defun boost-message (chunk)
  (when *VERBOSE* (command-output 
"     ------------------------------------------
!!!!!!!!!!!!!!!!  BOOSTING ACTIVATION OF ~s.
     ------------------------------------------" 
     chunk))
  )


