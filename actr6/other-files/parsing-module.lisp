;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Felix Engelmann
;;; Copyright   : (c) 2010-14 Felix Engelmann
;;; Availability: 
;;; Address     : Department Linguistik
;;;             : University of Potsdam
;;;             : 14467 Potsdam
;;;             : felix.engelmann@uni-potsdam.de
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : parsing-module.lisp
;;; Version     : 1.2
;;; 
;;; Description : A module for simulating sentence parsing in parallel to other cognition.
;;; 
;;; Bugs        : 
;;;
;;; Todo        : [X] Check if hooks are applied:
;;;             :     - :chunk-add-hook merge-copied-syn-obj
;;;             :     - (add-post-event-hook 'detect-set-buffer-chunk)
;;;             : [X] Make hooks part of the module
;;;             : [X] Add possibility to mark busy lexical and grammatical
;;;             :     buffers independently.
;;;             : [ ] Add a clear-all-buffers command.
;;;             : [X] Make *copied-chunks* an inaccessible parameter
;;;             : [ ] Add parsing-related structures and functions:
;;;             :     [ ] fake-parsing 
;;;             :     [ ] record and modify current IP
;;;             :     [ ] map categories
;;;             :     [X] begin and end time of attachment
;;;             :     [X] update attached positions
;;;             :     [X] check if parsed 
;;;             :     [X] messages for integrated word, skip, attach, (boost)
;;;             :     [X] vars that hold *current-index*, *current-ip*, *current-word*,
;;;             :         *begin-time*, *end-time*, *attach-times*, *attached-positions*,
;;;             :         *attached-items* (with functions like (parsing-get-index))
;;;             :     [X] function for printing parser variables (print-parsing-state)
;;;             :     [ ] Put prefix "parsing" before each function name
;;;             :     [ ] current-clause maintenance
;;;
;;; ----- History -----
;;; 2010.10.22 Felix:
;;;             : * started file
;;; 2014.04.10 Felix:
;;;             : * New buffers GRAMMATICAL and LEXICAL that can issue retrieval requests.
;;;             :   Both have their own parameters :gram-lf, :gram-rt, :lex-lf, :lex-rt
;;;             : * Additional buffer CONTEXTUAL, that can be used to, e.g., store the
;;;             :   contents of the lexical buffer
;;;             : * Buffers STRUCTURAL, STRUCTURAL2, STRUCTURAL3 to create syntactic nodes
;;;             : * Parameter to switch off chunk duplication for grammatical buffer
;;; 2014.07.18 Felix:
;;;             : * Uploaded to GitHub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The module defines two new retrieval buffers that can be used in parallel:
;;;   - GRAMMATICAL
;;;   - LEXICAL
;;;
;;; For the grammatical buffer, chunk duplication in DM can be switched off with 
;;; (sgp :gram-force-merge T).
;;;
;;; Other buffers are:
;;;   - CONTEXTUAL (can be used to, e.g., store the contents of the lexical buffer)
;;;   - STRUCTURAL, STRUCTURAL2, STRUCTURAL3 (act like goal buffer, to create syntactic nodes)
;;;
;;;
;;; The module uses the following parameters:
;;;   :SURPRISAL-FACTOR (used in EMMA or elsewhere to scale surprisal values if used)
;;;   :SURPRISAL-HL-FACTOR (used in EMMA or elsewhere to scale surprisal values if used)
;;;   :gram-lf (:lf for grammatical buffer)
;;;   :gram-rt (:rt factor for grammatical buffer)
;;;   :lex-lf  (:lf for lexical buffer)
;;;   :lex-rt  (:rt for lexical buffer)
;;;   :gram-force-merge (If T, then chunks retrieved and manipulated in the grammatical 
;;;                      buffer will always be merged with their originals in DM by 
;;;                      updating their slots, so no chunks will be duplicated in DM.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")



;;;
;;; MODULE STRUCTURE
;;;
(defstruct parsing-module model-name 
  busy failed error jammed lex-busy lex-error lex-jammed lex-failed 
  parser-busy 
  glf grt llf lrt srpr srprhl 
  begin-time current-word current-index parse-loc ip-stack clause-id-stack
  durations attached-positions attached-items unattached-positions
  force-merge hook-id1 hook-id2 copied-chunks
  regr-util att-util att-util2 sp-time
  )



;;;
;;; MODULE INTERNAL FUNCTIONS
;;;

(defun create-parsing-module (model-name)
  (make-parsing-module :model-name model-name))

(defun delete-parsing-module (instance)
  (delete-event-hook (parsing-module-hook-id1 instance))
  (delete-event-hook (parsing-module-hook-id2 instance)))

(defun parsing-primary-reset (instance)
  ;; create the chunk-types used by the module?
  ;;
  
  ;; clear its internal flags
  (setf (parsing-module-busy instance) nil)
  (setf (parsing-module-error instance) nil)
  (setf (parsing-module-failed instance) nil)
  (setf (parsing-module-jammed instance) nil)
  (setf (parsing-module-lex-busy instance) nil)
  (setf (parsing-module-lex-jammed instance) nil)
  (setf (parsing-module-lex-error instance) nil)
  (setf (parsing-module-lex-failed instance) nil)
  )


(defun parsing-secondary-reset (instance)
  ; (declare (ignore instance))
  ; (delete-event-hook (parsing-module-hook-id1 instance))
  ; (delete-event-hook (parsing-module-hook-id2 instance))
  (setf (parsing-module-durations instance) nil)
  (setf (parsing-module-attached-positions instance) nil)
  (setf (parsing-module-unattached-positions instance) nil)
  (setf (parsing-module-attached-items instance) nil)
  (setf (parsing-module-begin-time instance) nil)
  (setf (parsing-module-current-word instance) nil)
  (setf (parsing-module-current-index instance) -1)
  (setf (parsing-module-parse-loc instance) nil)
  (setf (parsing-module-ip-stack instance) nil)
  (setf (parsing-module-clause-id-stack instance) nil)
  (setf (parsing-module-copied-chunks instance) '())
  
  (suppress-warnings
    (let ((id1 (add-post-event-hook 'detect-set-buffer-chunk))
          (id2 (add-post-event-hook 'detect-set-buffer-chunk-retrieval))) ;; for backward comp.
      (if id1 (setf (parsing-module-hook-id1 instance) id1))
      (if id2 (setf (parsing-module-hook-id2 instance) id2))
      ))
  (if (not (member 'merge-copied-syn-obj (no-output (car (sgp :chunk-add-hook)))))
      (no-output (sgp :chunk-add-hook merge-copied-syn-obj)))
; (sgp :do-not-harvest grammatical)
  )


(defun parsing-query (instance buffer query value)
  (cond ((eq buffer 'grammatical)
    ;; grammatical takes the state queries which check the module's flags
    (cond ((eq query 'state)
           (case value
             (busy (parsing-module-busy instance))
             (free (not (parsing-module-busy instance)))
             (error (parsing-module-error instance))
             (failed (parsing-module-failed instance))))
          ;; The detect-jam query is t if the value specified matches the
          ;; internal state of the flag
          ((eq query 'detect-jam)
           (eq value (parsing-module-jammed instance)))
          ;; Otherwise print the warning and return nil
          (t
           (model-warning "Invalid query specified in the grammatical buffer query"))))
        ((eq buffer 'lexical)
         ;; lexical takes has its own flags
         (cond ((eq query 'state)
                (case value
                      (busy (parsing-module-lex-busy instance))
                      (free (not (parsing-module-lex-busy instance)))
                      (error (parsing-module-lex-error instance))
                      (failed (parsing-module-lex-failed instance))))
               ;; The detect-jam query is t if the value specified matches the
               ;; internal state of the flag
               ((eq query 'detect-jam)
                (eq value (parsing-module-lex-jammed instance)))
               ;; Otherwise print the warning and return nil
               (t
                 (model-warning "Invalid query specified in the lexical buffer query"))))
        (t          
          ;; buffers other tha grammatical only takes the required state queries
          (if (eq query 'state)
              (case value
                    (busy nil)
                    (free t)
                    (error nil))
              (model-warning "Invalid query specified in the ~S buffer query" buffer)))
        ))



(defun parsing-query-status ()
  ;; Space the output out like the default queries
  ;; and just print the internal flag's value.
  (command-output "  detect-jam            : ~S"
                  ;; Get the value by querying the module instead 
                  ;; of getting the module's instance and pulling 
                  ;; out the value directly.
                  (query-buffer 'grammatical '((detect-jam . t)))))


(defun parsing-module-params (parsing param)
  (if (consp param)
    (case (car param)
      (:SURPRISAL-FACTOR
        (setf (parsing-module-srpr parsing) (cdr param)))
      (:SURPRISAL-HL-FACTOR
        (setf (parsing-module-srprhl parsing) (cdr param)))
      (:gram-lf
        (setf (parsing-module-glf parsing) (cdr param)))
      (:gram-rt
        (setf (parsing-module-grt parsing) (cdr param)))
      (:lex-lf
        (setf (parsing-module-llf parsing) (cdr param)))
      (:lex-rt
        (setf (parsing-module-lrt parsing) (cdr param)))
      (:gram-force-merge
        (setf (parsing-module-force-merge parsing) (cdr param)))
      (:att-util
        (setf (parsing-module-att-util parsing) (cdr param)))
      (:att-util2
        (setf (parsing-module-att-util2 parsing) (cdr param)))
      (:regr-util
        (setf (parsing-module-regr-util parsing) (cdr param)))
      (:sp-time
        (setf (parsing-module-sp-time parsing) (cdr param)))
    )
    (case param
      (:SURPRISAL-FACTOR
       (parsing-module-srpr parsing))
      (:SURPRISAL-HL-FACTOR
       (parsing-module-srprhl parsing))
      (:gram-lf 
        (parsing-module-glf parsing))
      (:gram-rt 
        (parsing-module-grt parsing))
      (:lex-lf 
        (parsing-module-llf parsing))
      (:lex-rt 
        (parsing-module-lrt parsing))
      (:gram-force-merge
        (parsing-module-force-merge parsing))
      (:att-util
        (parsing-module-att-util parsing))
      (:att-util2
        (parsing-module-att-util2 parsing))
      (:regr-util
        (parsing-module-regr-util parsing))
      (:sp-time
        (parsing-module-sp-time parsing))
      )))




;;;
;;; MODULE REQUESTS
;;;

(defun parsing-requests (instance buffer chunk-spec)
  ; (let ((le (car (no-output (sgp :le)))))
    ; (model-output "Request to the ~s buffer made with a request of type ~S" buffer
                ; (chunk-spec-chunk-type chunk-spec))
    (cond ((eq buffer 'grammatical)
           (handle-grammatical-request instance chunk-spec))
          ((eq buffer 'lexical)
           (handle-lexical-request instance chunk-spec))
          (t
           (handle-other-parsing-request chunk-spec buffer)))
    )


(defun handle-lexical-request (instance chunk-spec)
  (let* ((spec (flatten (list 'isa (chunk-spec-chunk-type chunk-spec) (mapcar #'(lambda (x) (cdr x)) (chunk-spec-slot-spec chunk-spec))))) 
         (results (no-output (simulate-retrieval-request-fct spec)))
         (le (car (no-output (sgp :le)))))
    
    ;; do some dummy act-r-random calls to advance the seed somewhat
    ;; because simulate-retrieval-request restores the random seed to the same 
    ;; state it had prior to the call
    (dotimes (i 10) (act-r-random 1.0))
    
    (if (parsing-module-lex-busy instance)
      (progn  ;; if so issue a warning and set the jammed flag
        (model-warning "Parsing module's lexical buffer can only process one request at a time.")
        (setf (parsing-module-lex-jammed instance) t))
      (if (and (car results) (>= (chunk-activation (car results)) (parsing-module-lrt instance)))
          ;; if chunk retrieved
          (let* ((bestchunk (car results))
                 (activation (chunk-activation bestchunk))
                 (latency (+ 0.001 (parsing-compute-activation-latency (parsing-module-llf instance) le activation)))
                 ; (latency 0.001)
                 )
            ; set the busy flag for the module
            (setf (parsing-module-lex-busy instance) t)
            ; clear the error flag 
            (setf (parsing-module-lex-error instance) nil)
            (schedule-set-buffer-chunk 'lexical bestchunk latency 
                                       :module 'parsing 
                                       :priority :max)
            (schedule-event-relative latency 'finish-lexical-request :module 'parsing :destination 'parsing :priority :max)
            )
          ;; if no chunk retrieved
          (parsing-lexical-retrieval-failure instance)
          ))))



(defun handle-grammatical-request (instance chunk-spec)
  (let* ((spec (flatten (list 'isa (chunk-spec-chunk-type chunk-spec) (mapcar #'(lambda (x) (cdr x)) (chunk-spec-slot-spec chunk-spec))))) 
         (results (no-output (simulate-retrieval-request-fct spec)))
         (le (car (no-output (sgp :le)))))
    
    ;; do some dummy act-r-random calls to advance the seed somewhat
    ;; because simulate-retrieval-request restores the random seed to the same 
    ;; state it had prior to the call
    (dotimes (i 10) (act-r-random 1.0))
    
    (if (parsing-module-busy instance)
      (progn  ;; if so issue a warning and set the jammed flag
        (model-warning "Parsing module's grammatical buffer can only process one request at a time.")
        (setf (parsing-module-jammed instance) t))
      (if (and (car results) (>= (chunk-activation (car results)) (parsing-module-grt instance)))
          ;; if chunk retrieved
          (let* ((bestchunk (car results))
                 (activation (chunk-activation bestchunk))
                 (latency (parsing-compute-activation-latency (parsing-module-glf instance) le activation)))
            ; set the busy flag for the module
            (setf (parsing-module-busy instance) t)
            ; clear the error flag 
            (setf (parsing-module-error instance) nil)
            ; schedule the buffer setting
            (schedule-set-buffer-chunk 'grammatical bestchunk latency 
                                       :module 'parsing 
                                       :priority :max)
            (schedule-event-relative latency 'finish-grammatical-request :module 'parsing :destination 'parsing :priority :max)
            )
          ;; if no chunk retrieved
          (parsing-retrieval-failure instance)
        ))))



(defun handle-other-parsing-request (chunk-spec buffer)
  (let ((chunk-def (chunk-spec-to-chunk-def chunk-spec)))
    (if chunk-def
        ;; schedule the buffer setting for the newly created chunk
        (schedule-set-buffer-chunk buffer (car (define-chunks-fct (list chunk-def))) 0
                                   :module 'parsing)
      (model-warning "Invalid chunk-spec provided for a ~S buffer request." buffer))))


(defun parsing-compute-activation-latency (lf le activation)
  (* lf
     (exp-coerced (* -1 le activation))))
  
(defun parsing-retrieval-failure (instance)
  (model-warning "No chunk retrieved with grammatical buffer in parsing module!") 
  ;; Clear the busy flag and set the failure flag.
  (setf (parsing-module-busy instance) nil)
  (setf (parsing-module-failed instance) t))

(defun parsing-lexical-retrieval-failure (instance)
  (model-warning "No chunk retrieved with lexical buffer in parsing module!") 
  ;; Clear the busy flag and set the failure flag.
  (setf (parsing-module-lex-busy instance) nil)
  (setf (parsing-module-lex-failed instance) t))

(defun finish-grammatical-request (instance)
  ;; Just clear the busy and jammed flags
  (setf (parsing-module-busy instance) nil)
  (setf (parsing-module-jammed instance) nil))

(defun finish-lexical-request (instance)
  ;; Just clear the busy and jammed flags
  (setf (parsing-module-lex-busy instance) nil)
  (setf (parsing-module-lex-jammed instance) nil))





;;;
;;; DM HOOKS TO AVOID CHUNK DUPLICATION 
;;;

;;; RECORD RETRIEVED CHUNKS HOOK
;;;  Whenever a chunk is retrieved, original and copy are recorded
;;;  in order to merge them after the chunk is released from the buffer

(defun detect-set-buffer-chunk-retrieval (event)
  (when (and (parsing-module-force-merge (get-module parsing)) (eq (evt-action event) 'SET-BUFFER-CHUNK) (eq (first (evt-params event)) 'RETRIEVAL))
    (let ((copy (buffer-read (first (evt-params event)))))
      (record-copied-chunk copy))
    ))


(defun detect-set-buffer-chunk (event)
  (when (and (parsing-module-force-merge (get-module parsing)) (eq (evt-action event) 'SET-BUFFER-CHUNK) (eq (first (evt-params event)) 'GRAMMATICAL))
    (let ((copy (buffer-read (first (evt-params event)))))
      (record-copied-chunk copy))
    ))

(defun record-copied-chunk (copy)
  (when (eq (chunk-chunk-type-fct copy) 'SYN-OBJ)
    (let ((original (chunk-copied-from-fct copy)))
      (when original
        (if (chunk-p-fct original)
          (progn 
            (when (no-output (car (sgp :v))) (model-warning " +++ Chunk ~s was copied from ~s. +++" copy original))
            (setf (parsing-module-copied-chunks (get-module parsing)) (acons copy original (parsing-module-copied-chunks (get-module parsing)))))))
      )))


;;; The chunk-add-hook:
;;; Checks whether syn-obj chunk was copied and merges it with original.
(defun merge-copied-syn-obj (copy)
  (when (and (parsing-module-force-merge (get-module parsing)) (eq (chunk-chunk-type-fct copy) 'SYN-OBJ))
    (let ((original (cdr (assoc copy (parsing-module-copied-chunks (get-module parsing))))))
      (when original
        (if (chunk-p-fct original)
          (progn 
            (when (no-output (car (sgp :v))) (model-warning " +++ Forcing merge unequal copy ~s with source chunk ~s. +++" copy original))
            (force-merge-chunks original copy))))
      )))

(defun force-merge-chunks (original copy)
  (let* ((ctype (chunk-chunk-type-fct copy))
         (slots (chunk-type-slot-names-fct ctype)))
    (loop for slot in slots do
      (set-chunk-slot-value-fct original slot (chunk-slot-value-fct copy slot)))
    (if (and (equal-chunks-fct original copy) (merge-chunks-fct original copy))
      nil ;(when *VERBOSE* (model-warning " +++ force-merge-chunks success +++~%"))
      (print-warning " !!! ERROR: force-merge-chunks failed! !!!"))
    ))





;;;
;;; PARSING STATE MAINTENANCE
;;;

(defun parsing-begin (&optional (word nil) (index nil) (location nil) )
  (let ((time (- (mp-time) (car (no-output (sgp :dat))))))
    (schedule-event-relative 0 'begin-attachment :params (list time word index location)
                             :module 'parsing :destination 'parsing 
                             :output 'low :details (format nil "Begin-attachment ~s at ~6,3F" word time) 
                             :priority :max)
    ))


(defun begin-attachment (instance time word index location)
  (setf (parsing-module-begin-time instance) time)  ; includes firing time of set-cues production
  (setf (parsing-module-current-word instance) word)
  (setf (parsing-module-current-index instance) index)
  (setf (parsing-module-parse-loc instance) location)
  (setf (parsing-module-parser-busy instance) t)
  ; (setf (parsing-module-begin-time (get-module parsing)) (- (mp-time) (car (no-output (sgp :dat)))))  ; includes firing time of set-cues production
  ; (setf (parsing-module-current-word (get-module parsing)) word)
  ; (setf (parsing-module-current-index (get-module parsing)) index)
  ; (setf (parsing-module-parse-loc (get-module parsing)) loc)
  ; (setf (parsing-module-parser-busy (get-module parsing)) t)
  )


(defun parsing-complete ()
  (let ((attach-time (round (* 1000 (- (mp-time) (parsing-module-begin-time (get-module parsing))))))
        (word (parsing-module-current-word (get-module parsing)))
        (index (parsing-module-current-index (get-module parsing))))
    (schedule-event-relative 0 'attachment-completed :params (list attach-time word index)
                             :module 'parsing :destination 'parsing 
                             :output 'low :details (format nil "Attachment-complete ~s (~D ms)" word attach-time) 
                             :priority :max)
    ))



(defun attachment-completed (instance attach-time word index)
  (priority-info-message (format nil "Total attachment time for ~A: ~D" word attach-time))
  (push-last attach-time (parsing-module-durations instance))
  (push-last (list index word attach-time) (parsing-module-attached-items instance))
  (push-last index (parsing-module-attached-positions instance))
  (setf (parsing-module-parser-busy instance) nil)
    ;; TODO: release all syn-obj chunks
    ;; TODO: set current IP
  )


(defun parsing-abort ()
  (let ((attach-time (round (* 1000 (- (mp-time) (parsing-module-begin-time (get-module parsing))))))
        (word (parsing-module-current-word (get-module parsing)))
        (index (parsing-module-current-index (get-module parsing))))
    (schedule-event-relative 0 'attachment-aborted :params (list attach-time word index)
                             :module 'parsing :destination 'parsing 
                             :output 'low :details (format nil "Attachment-aborted ~s" word) 
                             :priority :max)
    ))


(defun attachment-aborted (instance attach-time word index)
  (priority-info-message (format nil "Attachment ABORTED for ~A after ~D ms  +++" word attach-time))
  (push-last attach-time (parsing-module-durations instance))
  (push-last index (parsing-module-unattached-positions instance))
  (setf (parsing-module-parser-busy instance) nil)
  )



;;;
;;; CURRENT IP MAINTENANCE FUNCTIONS
;;;

;; TODO: replace 'IPb by 'structural
(defun parsing-get-ip-from-buffer nil
  ; (let ((strchunk (buffer-read 'structural)))
  (let ((strchunk (buffer-read 'IPb)))
    (when strchunk
      (let ((cat (chunk-slot-value-fct strchunk 'cat)))
        (if (eq cat 'IP)
            strchunk
            nil)
        ))
    ))

(defun parsing-set-current-ip nil
  (let ((strchunk (parsing-get-ip-from-buffer)))
    (when strchunk
      (model-warning " +++ Setting current IP chunk +++")
      (push strchunk (parsing-module-ip-stack (get-module parsing)))
      )
    ))

(defun parsing-current-ip nil
  (let ((ipchunk (parsing-get-ip-from-buffer)))
    (if ipchunk 
        ipchunk
        (car (parsing-module-ip-stack (get-module parsing))))
    ))

(defun parsing-mod-current-ip (modlist)
  (let* ((current-ip (parsing-current-ip)))
    (mod-chunk-fct current-ip modlist)
    (model-warning " +++ Modifying IP +++")
    ))

(defun parsing-pop-current-ip nil
  (pop (parsing-module-ip-stack (get-module parsing)))
  )

(defun parsing-read-current-ip-slot (slot)
  (let* ((current-ip (parsing-current-ip)))
    (chunk-slot-value-fct current-ip slot)
   ))



;;;
;;; KEEPING TRACK OF C-COMMANDERS
;;;
(defun parsing-push-clause nil
  (push (new-name c) (parsing-module-clause-id-stack (get-module parsing)))
  )

(defun parsing-pop-clause nil
  (pop (parsing-module-clause-id-stack (get-module parsing)))
  (car (parsing-module-clause-id-stack (get-module parsing)))
  )

(defun parsing-current-clause nil
  (car (parsing-module-clause-id-stack (get-module parsing)))
  )


;;;
;;; MESSAGES
;;; TODO: schedule these as events of the module
;;; 

(defun parsing-skip-message (word)
  (info-message (format nil "Word ~s already attached - Skipping integration!" word))
  nil)

(defun parsing-boost-message (chunk)
  (event-message (format nil "BOOSTING ACTIVATION OF ~s!" chunk))
  )



;;;
;;; Output message formats
;;;
(defun event-message (message)
  (command-output "+++ ~6,3F   ~A" (mp-time) message)
  nil)

(defun info-message (message)
  (command-output "+++  ~A  +++" message)
  nil)

(defun priority-event-message (message)
  (command-output 
"-----------------------------------------------------------------------
    ~6,3F   ~A
-----------------------------------------------------------------------" 
    (mp-time) message)
  nil)

(defun priority-info-message (message)
  (command-output 
"-----------------------------------------------------------------------
     ~A  
-----------------------------------------------------------------------"
    message)
  nil)






;;;
;;; FUNCTIONS FOR ACCESSING PARSER INFORMATION
;;;

;; TODO: add clause maintenance

(defun parsing-busy nil
  (parsing-module-parser-busy (get-module parsing)))
(defun parsing-get-index nil
  (parsing-module-current-index (get-module parsing)))
(defun parsing-get-word nil
  (parsing-module-current-word (get-module parsing)))
(defun parsing-get-loc nil
  (parsing-module-parse-loc (get-module parsing)))
(defun parsing-get-durations nil
  (parsing-module-durations (get-module parsing)))
(defun parsing-get-attached-items nil
  (parsing-module-attached-items (get-module parsing)))
(defun parsing-get-attached-positions nil
  (parsing-module-attached-positions (get-module parsing)))
(defun parsing-get-unattached-positions nil
  (parsing-module-unattached-positions (get-module parsing)))
(defun parsing-check-attached (index)
  (let ((poslist (parsing-module-attached-positions (get-module parsing)))
        (current-index (parsing-module-current-index (get-module parsing))))
    (if (or (member index poslist) (eq current-index index))
        t
        nil)
    ))

(defun parsing-print-info nil
  (format t "Parser busy: ~s~%" (parsing-module-parser-busy (get-module parsing)))
  (format t "Current word: ~s~%" (parsing-module-current-word (get-module parsing)))
  (format t "Current index: ~s~%" (parsing-module-current-index (get-module parsing)))
  (format t "Last parse-loc: ~s~%" (parsing-module-parse-loc (get-module parsing)))
  (format t "Current IP: ~s~%" (parsing-current-ip))
  (format t "Current clause: ~s~%" (parsing-current-clause))
  (format t "Last begin-time: ~s~%" (parsing-module-begin-time (get-module parsing)))
  (format t "Attachment durations: ~s~%" (parsing-module-durations (get-module parsing)))
  (format t "Attached positions: ~s~%" (parsing-module-attached-positions (get-module parsing)))
  (format t "Attached items: ~s~%" (parsing-module-attached-items (get-module parsing)))
  )





;;;
;;; MODULE DEFINITION
;;;

(define-module-fct 'parsing
  '(grammatical lexical contextual structural structural2 structural3 XPb IPb DPb DP2b NPb CPb VPb PPb VP2b AdjPb AdvPb lex prediction)             ;; buffers
  
  (list (define-parameter :SURPRISAL-FACTOR
              :owner T
              :valid-test #'nonneg
              :warning "a non-negative number" 
              :default-value 0.001
              :documentation "Scaling constant for surprisal")
        (define-parameter :SURPRISAL-HL-FACTOR
              :owner T
              :valid-test #'nonneg
              :warning "a non-negative number" 
              :default-value 0.01
              :documentation "Scaling constant for high level surprisal")
        (define-parameter :gram-lf :owner T :valid-test #'nonneg :default-value 1.0
          :warning "a non-negative number" :documentation "Latency Factor for grammatical buffer")
        (define-parameter :gram-rt :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Retrieval Threshold for grammatical buffer")
        (define-parameter :lex-lf :owner T :valid-test #'nonneg :default-value 1.0
          :warning "a non-negative number" :documentation "Latency Factor for lexical buffer")
        (define-parameter :lex-rt :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Retrieval Threshold for lexical buffer")
        (define-parameter :att-util :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Utility for special attachement production")
        (define-parameter :att-util2 :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Utility for special attachement production")
        (define-parameter :regr-util :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Utility for regression production")
        (define-parameter :sp-time :owner T :valid-test #'numberp :default-value 0.0
          :warning "a number" :documentation "Action time for special production")
        (define-parameter :gram-force-merge :owner T :valid-test #'tornil :default-value T
          :warning "T or nil" :documentation "When true, all chunks retrieved and modified in grammatical buffer
                                             will be force-merged with their originals in DM, also when unequal.")
        )
  :version "1.2"
  :documentation "A parsing module"
  :creation 'create-parsing-module
  :delete 'delete-parsing-module
  :reset '(parsing-primary-reset parsing-secondary-reset)
  :query 'parsing-query
  :request 'parsing-requests
  :buffer-mod 'goal-style-mod-request
  :params 'parsing-module-params
)


