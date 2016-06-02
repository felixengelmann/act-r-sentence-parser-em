;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Felix Engelmann
;;; Copyright   : (c) 2010-13 Felix Engelmann
;;; Availability: 
;;; Address     : Department Linguistik
;;;             : University of Potsdam
;;;             : 14467 Potsdam
;;;             : felix.engelmann@uni-potsdam.de
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : interface-emma.lisp
;;; Version     : 1.0
;;; 
;;; Description : 
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2010.10.22 Felix
;;;             : * started file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defmacro word-name (word) `(first ,word))
;(defmacro word-frequency (word) `(second ,word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METRICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-frequency (obj)
  (let ((freq (word-frequency (obj->wordinfo obj *sentence-plist*))))
    (if (numberp freq)
        (if *raw-freq* 
            (setf freq (/ freq 1e6))  ; divided by 1 million
            (setf freq freq))
        ; (setf freq 0.01))
        (setf freq 0.0037))  ;; mean freq in PSC
    freq))


(defun surprisal (obj)
  (let ((sf (parsing-module-srpr (get-module PARSING)))
        (surpr nil))
    (if *surprisal-on*
        (setf surpr (word-sp (obj->wordinfo obj *sentence-plist*))))
    (cond ((numberp surpr)
           (setf surpr (* sf surpr))
           ; (format t "~%        Surprisal: ~F" surpr) 
           surpr)
          (t 0))))


(defun surprisal-hl (obj)
  (let ((sf (parsing-module-srprhl (get-module PARSING)))
        ; (word (word-name (obj->wordinfo obj *sentence-plist*)))
        (surpr nil))
    (if *surprisal-hl-on*
        (setf surpr (word-sp (obj->wordinfo obj *sentence-plist*))))
    (cond ((numberp surpr)
           (setf surpr (* sf surpr))
           ; (model-output "   > HL-Surprisal (~S): ~3,3F" word surpr) 
           surpr)
          (t 0))))


(defun retrieval (obj)
  (let (;(word (word-name (obj->wordinfo obj *sentence-plist*)))
        (retr nil))
    (if *fake-retrieval-on*
        (setf retr (word-rv (obj->wordinfo obj *sentence-plist*))))
    (cond ((numberp retr)
           (setf retr (* 0.001 retr)); (- retr 50)))
           ; (model-output "   > Retrieval (~S): ~3,3F" word retr) 
           retr)
          (t .001))   ;; if zero then goal modification event will finish BEFORE 
    ;; goal modification of the actual production (wrong order!)
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Eventually remove
(defun record-fixations (sentence snr trace &optional (location (concatenate 'string *output-dir* "/fixations.txt")))
  (push (list snr trace) *traces*)
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (fix trace)
                    (if (>= (first fix) 0)
                        (format outfile "~S ~D ~S ~D ~S ~D~%" *experiment* *simulation* snr (1+ (first fix)) (format nil "~a" (word-name (nth (first fix) sentence))) (second fix))  
                        nil))
                  ))

(defun record-attachment-times (snr times &optional (location (concatenate 'string *output-dir* "/attachments.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (x times)
                    (if (>= (first x) 0)
                        (format outfile "~S ~D ~S ~D ~S ~D~%" *experiment* *simulation* snr (1+ (first x)) (second x) (third x))  
                        nil))
                  ))

(defun record-encoding-times (snr times &optional (location (concatenate 'string *output-dir* "/enctimes.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (x times)
                    (format outfile "~S ~D ~S ~D ~S ~D ~5,3F ~5,5F~%" *experiment* *simulation* snr (1+ (first x)) (second x) (third x) (fourth x) (fifth x)))
                  ))

(defun record-timeouts (snr times &optional (location (concatenate 'string *output-dir* "/timeouts.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (x times)
                    (if (>= (first x) 0)
                        (format outfile "~S ~D ~S ~D ~S ~D~%" *experiment* *simulation* snr (1+ (first x)) (second x) (1+ (third x)))
                        nil))
                  ))

(defun record-trialmessages (msg &optional (location (concatenate 'string *output-dir* "/trialmessages.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  ; (dolist (x msg)
                    (format outfile "~:{~&~S ~D ~S ~D ~S ~S ~S~}" msg)
                    (format outfile "~%")
                    ; )
                  ))

(defun record-visloc-activations (item acthist &optional (location (concatenate 'string *output-dir* "/visloc-activations.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (row acthist)
                    (format outfile "~S ~D ~S" *experiment* *simulation* item)
                    (dolist (x row)
                      (format outfile " ~S" x))
                    (format outfile "~%")
                    )
                  ))

; (defun record-table (sent table &optional (location (concatenate 'string *output-dir* "/default.txt")))
;   (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
;                   (dolist (row table)
;                     (format outfile "~S ~D ~S" *experiment* *simulation* sent)
;                     (dolist (x row)
;                       (format outfile " ~S" x))
;                     (format outfile "~%")
;                     )
;                   ))


;; TODO: move?
(defun table (l)
  (let ((table nil))
    (dolist (x l)
      (push-last (append (list *experiment* *simulation* *item*) x) table))
    table))

(defun fixations-table (l)
  (let ((table nil))
    (dolist (fix l)
      (if (>= (first fix) 0)
          (push-last (list *experiment* *simulation* *item* (1+ (first fix)) 
                           (format nil "~a" (word-name (nth (first fix) *sentence-plist*))) (second fix)) 
                     table)))
    table))

(defun encodings-table (l)
  ;; (pos word enctime eccentricity freq)
  (let ((table nil))
    (dolist (x l)
      (push-last (list *experiment* *simulation* *item* (1+ (first x))
                       (second x) (third x) (fourth x) (fifth x))
                 table))
    table))

(defun attachments-table (l)
  ;; (pos word time)
  (let ((table nil))
    (dolist (x l)
      (push-last (list *experiment* *simulation* *item* (1+ (first x))
                       (second x) (third x))
                 table))
    table))

(defun timeouts-table (l)
  ;; (cause-pos wordname eye-pos)
  (let ((table nil))
    (dolist (x l)
      (push-last (list *experiment* *simulation* *item* (1+ (first x))
                       (second x) (1+ (third x)))
                 table))
    table))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Eye Tracing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Move to EMMA module
; (defun reset-emma ()
;   (when (mymember :emma *features*)
;     ;    (reset-emma-module (get-module :vision))
;     (setf (trace-eye-p (get-module :vision)) t)   ;; ensure EMMA records eye trace
;     (set-eye-loc (get-module :vision) #(10 150))  ;; make sure that the eccentricity of first word is not too enormous
;     ))

(defun reset-emma ()
  (when (mymember :emma *features*)
  (setf (eye-trace (get-module :vision)) nil)
  (setf (shift-start (get-module :vision)) 0)
  (setf (shift-target (get-module :vision)) nil)
  (setf (prep-event (get-module :vision)) nil)
  (setf (next-loc (get-module :vision)) nil)
  (clrhash (freq-ht (get-module :vision)))
  (setf (trace-eye-p (get-module :vision)) t)   ;; ensure EMMA records eye trace
  (set-eye-loc (get-module :vision) #(10 150))  ;; make sure that the eccentricity of first word is not too enormous
  ))


;;
(defun get-em-trace ()
  (when (mymember :emma *features*)
    (let ((em-trace (reverse (eye-trace (get-module :vision)))))
      (unless (neq (aref (cdr (first em-trace)) 1) 0)
        (pop em-trace))
      em-trace)))


(defun em-trace->fixations (em-trace sentence)
  (unless (or (null em-trace) (null (cdr em-trace)))
    (cons (list (location->index (coerce (cdr (first em-trace)) 'list) sentence)
                (round (* 1000 (- (first (second em-trace)) (first (first em-trace))))))
          (em-trace->fixations (cdr em-trace) sentence))))


;;;
;;; EYE-LOC ACCESS
;;;
(defun current-eye-loc ()
  (if (mymember :emma *features*)
      (let ((eye-loc (eye-loc (get-module :vision))))
        (coerce eye-loc 'list)))
  (list (index->location (parsing-get-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Move into EMMA module

(defun report-fixation-time (newloc)
  (let* ((fixtime (- (mp-time) *fix-start-time*))
         (pos (location->index (coerce *fix-loc* 'list) *sentence-plist*))
         (word (word-name (nth pos *sentence-plist*)))
         (pos2 (location->index (coerce newloc 'list) *sentence-plist*))
         (word2 (word-name (nth pos2 *sentence-plist*))))
    (command-output 
"---------------------------------------------------------------------------
+++  Fixation time on ~A (~D) (~S): ~6,3F  +++
---------------------------------------------------------------------------" *fix-loc* (+ 1 pos) word fixtime)
    (command-output "*** ~6,3F   Fixating ~A (~D) (~S)" 
                    (mp-time) newloc (+ 1 pos2) word2)
    (setf *fix-start-time* (mp-time))
    (setf *fix-loc* newloc)
    ))


(defun report-attention-shift (obj freq enctime eccentricity srpr)
  (let* ((location (obj->location obj))
         (word (chunk-slot-value-fct obj 'value))
         (pos (location->index location *sentence-plist*)))
    (push-last (list pos word (round (* enctime 1000)) eccentricity freq) *encoded-items*)
    (command-output "*** ~6,3F   Encoding ~D (~S)" 
                    (mp-time) (1+ pos) word)
    (command-output "     +++ Enctime: ~3,3F +++ Freq.: ~3,5F +++ Ecc.: ~3,5F +++ Surpr.: ~3,5F +++" enctime freq eccentricity srpr)
    ; (command-output "     +++ HL-Surprisal: ~3,3F +++ Raw Retrieval: ~3,3F +++" (surprisal-hl obj) (retrieval obj))
    ; (unless (or (chunk-slot-value-fct (buffer-read 'goal) 'regression) (mymember pos *encoded-positions*))
    ;   (tag-unexpected obj))
    ; (surprisal-regression-early srpr)
    (push-last pos *encoded-positions*)
    ))


(defun start-time-out (loc-obj)
  (let* ((eye-loc (current-eye-loc))
         (pos (location->index eye-loc *sentence-plist*))
         (word (word-name (nth pos *sentence-plist*)))
         (targetloc (list (chunk-slot-value-fct loc-obj 'screen-x) (chunk-slot-value-fct loc-obj 'screen-y)))
         (tpos (location->index targetloc *sentence-plist*))
         (tword (word-name (nth tpos *sentence-plist*))))
    (push-last (list tpos tword pos) *timeout-items*)
    (command-output 
"---------------------------------------------------------------------------
*** ~6,3F   SLOW FAIL at ~D (~S) ==> TIME OUT Regr. from ~D (~S)
---------------------------------------------------------------------------" 
     (mp-time) (+ 1 tpos) tword (+ 1 pos) word)
    ))



(defun exit-time-out () (priority-event-message 'EXIT-TIME-OUT))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REGRESSIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun regression-landing-position (target-pos &optional (eye-loc (car (current-eye-loc))) (index (parsing-get-index)))
  (- (+ eye-loc 10) (* eye-loc (/ (- index target-pos) index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SURPRISAL EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tag-unexpected (obj)
  (let* ((s (surprisal-hl obj))
         ; (sf (parsing-module-srpr (get-module PARSING)))
         ; (scale (* sf 10))
         (p (rand-time (/ s 10)))
         (r (if (> p 0.5) t nil))
         )
    (if r
        ; (setf chunk-expected NIL)
        ; (mod-chunk-fct 'obj (list 'status "unexpected"))
        (mod-chunk-fct (buffer-read 'goal) (list 'unexpected t))
        ; (mod-focus em-state "unexpected")
        ; (setf chunk-expected T)
        )
    ))


(defun surprisal-regression-early (s)
  (let* ((sf (parsing-module-srpr (get-module PARSING)))
         (scale (* sf 10))
         (p (rand-time (/ s scale)))
         (r (if (> p 0.5) t nil)))
    (if r
        (mod-focus em-state "random-regression"))
    ))

(defun surprisal-regression-late (obj)
  (let* ((s (surprisal-hl obj))
         ; (sf (parsing-module-srpr (get-module PARSING)))
         ; (scale (* sf 10))
         (p (rand-time (/ s 10)))
         (r (if (> p 0.5) t nil))
         (location (obj->location obj))
         (word (chunk-slot-value-fct obj 'value))
         (pos (location->index location *sentence-plist*))
         (eye-loc (current-eye-loc))
         (spos (location->index eye-loc *sentence-plist*)))
    (if r
        (progn
          ; (mod-focus state "read")
          ; (mod-focus em-state "random-regression")
          (command-output
"     =============================================
        RAPID FAIL at ~D (~S) ==> RANDOM Regr. from ~D
     ============================================="
     (+ 1 pos) word (+ 1 spos))
          "random-regression"
          )
        "free")
    ))



(defun start-fail (loc-obj)
  (let* ((eye-loc (current-eye-loc))
         (spos (location->index eye-loc *sentence-plist*))
         (loc (list (chunk-slot-value-fct loc-obj 'screen-x) (chunk-slot-value-fct loc-obj 'screen-y)))
         (pos (location->index loc *sentence-plist*))
         (word (word-name (nth pos *sentence-plist*))))
    ; (push-last (list pos tword pos) *timeout-items*)
    (command-output 
"     =============================================
        RAPID FAIL at ~D (~S) ==> RANDOM Regr. from ~D
     =============================================" 
     (+ 1 pos) word (+ 1 spos))
    ))


(defun report-regression (loc-obj tpos tloc)
  (let* ((eye-loc (current-eye-loc))
         (spos (location->index eye-loc *sentence-plist*))
         (loc (list (chunk-slot-value-fct loc-obj 'screen-x) (chunk-slot-value-fct loc-obj 'screen-y)))
         (pos (location->index loc *sentence-plist*))
         (word (word-name (nth pos *sentence-plist*))))
    ; (push-last (list pos tword pos) *timeout-items*)
    (command-output 
"     =============================================
        REANALYSIS at ~D (~S) ==> TARGETED REGR. from ~D to ~D (aimed at ~4,1F)
     =============================================" 
     (+ 1 pos) word (+ 1 spos) (+ 1 tpos) tloc)
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCATION ACTIVATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (schedule-periodic-event 1 (lambda ()
;                              (model-output "Periodic event"))
;                          :initial-delay .5)


(defun start-visloc-record nil
  (schedule-periodic-event .05 'update-visloc-activations
                           :priority :max
                           :initial-delay 0.05)
  )

(defun stop-visloc-record nil
  (setf *event* *)
  (mp-show-queue)
  (delete-event *event*)
  )

(defun get-ref-count (c)
  (no-output (car (car (sdp-fct (list c :reference-count))))))

(defun reset-ref-count (c)
  (no-output (sdp-fct (list c :reference-count 0))))

(defun get-activation (c)
  (no-output (car (car (sdp-fct (list c :activation))))))

(defun increase-ref-count (c)
  (when *record-visloc-activations*
    (let* ((ref (get-ref-count c))
           (newref (+ 1 ref))
           (index (+ 1 (visloc->index c))))
      (no-output (sdp-fct (list c :reference-count newref)))
      (trialmessage 'boost index)
      )))

(defun increase-ref-count-parseloc nil
  (when *record-visloc-activations*
    (let* ((loc (parsing-get-loc)))
      (increase-ref-count loc)
      ; (update-visloc-activations)
      ; (trialmessage 'parsing-boost (parsing-get-index))
      )))

(defun register-new-visloc (loc)
  (let ((index (+ 1 (visloc->index loc))))
    (setf (nth index *visloc-list*) (cons index loc))
    ))

(defun update-visloc-activations NIL
  (let* ((acts (copy-list (car (last *visloc-activations*))))
         ; (refs (copy-list acts))
         ; (locs (copy-list acts))
         )
    (dolist (v *visloc-list*)
      (unless (eq v nil)
        (setf (nth (car v) acts) (get-activation (cdr v)))
        ; (setf (nth (car v) refs) (get-ref-count (cdr v)))
        ; (setf (nth (car v) locs) (cdr v))
        ))
    (setf (car acts) (mp-time))
    ; (setf (car refs) (mp-time))
    ; (setf (car locs) (mp-time))
    ; (push-last locs *visloc-activations*)
    ; (push-last refs *visloc-activations*)
    (push-last acts *visloc-activations*)
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXATION MEASURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gaze (fixations)
  (let ((gazedurs (make-array (1+ (n-words fixations))))
        (collapsed (collapse-fixations fixations)))
    (dolist (f collapsed)
        (cond
            ((eq (aref gazedurs (first f)) 0) 
                (setf (aref gazedurs (first f)) (second f)))))
  (array-to-index-list gazedurs)    
))


;; first fixation iff progressive
(defun ffd (fixations)
  (let ((ffds (make-array (1+ (n-words fixations))))
        (firstfix (firstfixations fixations))
        (lastf -1))
    (dolist (f firstfix)
      (when (and (eq (aref ffds (first f)) 0) (< lastf (first f)))
        (setf (aref ffds (first f)) (second f)))
      (setf lastf (first f)))
    (array-to-index-list ffds)    
    ))


(defun tft (fixations)
  (let ((tfts (make-array (1+ (n-words fixations)))))
    (dolist (f fixations)
        (setf (aref tfts (first f)) (+ (aref tfts (first f)) (second f))))
  (array-to-index-list tfts)
))

(defun rrt (fixations)
  (let ((rrts nil)
        (tfts (tft fixations))
        (fprts (gaze fixations)))
    (loop for i from 0 to (- (length tfts) 1) do
        (push-last (list i (- (second (nth i tfts)) (second (nth i fprts)))) rrts))
    rrts))

(defun reread (fixations)
  (let ((rereads nil)
        (rrts (rrt fixations)))
    (loop for i from 0 to (- (length rrts) 1) do
        (if (> (second (nth i rrts)) 0)
        (push-last (list i 1) rereads)
        (push-last (list i 0) rereads)))
    rereads))

(defun refix (fixations)
  (let ((refixs nil)
        (ffds (ffd fixations))
        (gazes (gaze fixations)))
    (loop for i from 0 to (- (length ffds) 1) do
        (if (> (second (nth i gazes)) (second (nth i ffds)))
        (push-last (list i 1) refixs)
        (push-last (list i 0) refixs)))
    refixs))


(defun fpreg (fixations)
  (let ((fpregs (make-array (1+ (n-words fixations)) :initial-element nil))
        (collapsed (collapse-fixations fixations)))
    (loop for i from 0 to (- (length collapsed) 1) do
        (let ((w0 (first (nth i collapsed)))
              (w1 (first (nth (1+ i) collapsed))))
            (when (null (aref fpregs w0))
                (if (and w1 (< w1 w0)) 
                    (setf (aref fpregs w0) 1)
                    (setf (aref fpregs w0) 0)))
        )
    )
  (array-to-index-list fpregs)
))



(defun firstfixations (fixations)
      (let ((last nil)
            (ffds nil))
        (do ((fixs fixations (rest fixs)))
            ((null fixs))
          (let* ((fixation (first fixs))
                 (index (first fixation)))
            (unless (eq index last)
              (push-last fixation ffds))
            (setf last index)))
        ffds
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun collapse-fixations (fixations)
  (cond ((< (length fixations) 2) fixations)
        ((equalp (first (first fixations)) (first (second fixations)))
         (collapse-fixations
          (cons (list (first (first fixations))
                      (+ (second (first fixations)) (second (second fixations))))
                (rest (rest fixations)))))
        (t (cons (first fixations) (collapse-fixations (rest fixations))))))

(defun n-words (trace)
  (let ((n 0))
    (dolist (w trace)
        (when (> (first w) n) (setf n (first w))))
    n))
    
(defun array-to-index-list (ar)
  (let ((ilist nil)
        (i 0))
    (loop for v across ar do
        (push-last (list i v) ilist)
        (incf i))
    ilist))






