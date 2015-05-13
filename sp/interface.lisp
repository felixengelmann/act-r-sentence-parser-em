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
;;; ===================================================================
;;;
;;; The  model is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; The model is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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




;;; INTERFACE PARAMETERS ;;;
(defvar *output-dir* "output")
(defparameter *real-time* nil "real time simulation")
(defparameter *record-times* T)
(defparameter *VERBOSE* T "verbose parameter")
(defparameter *estimating* nil)
(defvar *max-time* 30.0 "maximum time to run.")
;(defparameter *dcnn* T "dynamic chunk name normalizing during run time. Switch off for better performance")
(defparameter *show-window* T) 
(defparameter *width* 1000 "width of experiment window")
(defparameter *start-x* 25)
(defparameter *char-width* 7)
;(defparameter *n-sentences* 10)
(defparameter *trace-to-file* nil)

;;; MODEL PARAMETERS ;;;
(defvar *read-corpus* nil)
(defvar *raw-freq* nil)
(defparameter *surprisal-on* nil)
(defparameter *surprisal-hl-on* nil)
(defparameter *fake-retrieval-on* nil)
(defparameter *time-penalty-factor* 0.1 "Factor for penalizing use of time-out productions (p = -FACTOR*FIRING-COUNT)")
(defparameter *sacc-suppr* T)
(defparameter *time-out* T)

;;; PRESETS ;;;
(defparameter *output-setting* '(:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil))
(defparameter *params* nil) ;'(:v t))
(defparameter *performance-on-params* '(:ncnar nil :dcnn nil :save-buffer-history nil :save-p-history nil :ol t))
(defparameter *performance-off-params* '(:ncnar t :dcnn nil :save-buffer-history t :save-p-history t :ol t))
(defparameter *prefix-fct* nil)
(defparameter *postfix-fct* nil)

;;; RECORDING VISLOC ACTIVATIONS ;;;
(defparameter *record-visloc-activations* T)
(defvar *visloc-list* (list nil) "list of visual location chunks")
(defvar *visloc-activations* nil "visloc activation history")

;;; RUNTIME VARIABLES ;;;
(defvar *sentence* "holds the current sentence string")
(defvar *sentence-plist* "current sentence augmented with word properties")

; TODO: Variables not needed any more
(defvar *current-index* 0 "current word index")
(defvar *word* nil "current word")
(defvar *begin-time* 0 "a variable to hold the start time of processing a word after lexical access")
(defvar *end-time* 0 "a variable to hold the end time of processing a word")
(defvar *attach-time* nil "attachment time for each word")
(defvar *attach-times* nil "list of attachment times for each word")
(defvar *attached-positions* nil "list of positions of words being attached so far")
(defvar *attached-items* nil "list of attachment times for each word (pos word time)")
(defvar *fixation-trace* nil "fixation trace of a sentence")
(defvar *encoded-positions* nil "list of positions of words being encoded so far")
(defvar *encoded-items* nil "list of encoding times for each word (pos word enctime eccentricity freq)")
(defvar *timeout-items* nil "list of timeouts (cause-pos cause-word eye-pos)")
(defvar *trialmessages* nil "list of var/value pairs for trial events like regressions and attachment decisions")
(defvar *response* nil "a variable to hold the key press value")
(defvar *reading-time* nil)
(defvar *response-time-start* nil)
(defvar *response-times* nil)
(defvar *RUNTIMES* nil)

;;; EMMA VARIABLES ;;;
(defvar *fix-start-time* 0 "start time of a fixation")
(defvar *fix-loc* #(10 150) "current fixation location")

;;; EXPERIMENT CONTROL VARIABLES ;;;
(defvar *experiments* nil "a variable to hold experiment definitions") 
(defvar *sentences* nil "holds the sentences for the model to parse")
(defvar *experiment* 1 "number or name")
(defvar *subject* 1 "number or name")
(defvar *simulation* 1 "simulation or subject")
(defvar *iteration* 1 "current iteration")
(defvar *item* 1 "current sentence or condition number (starting with 1) or name")
(defvar *paramset* 0 "number or name as reference to a specific set of parameters")
(defvar *traces* nil "list of all eye movement traces")
(defvar *exp-traces* nil "structured eye movement traces of one experimental run")
(defvar *experiment-results* nil "a variable to hold experiment results")
(defvar *correct-responses* nil)
(defvar *responses* nil)
(defvar *correlations* nil)

;;; INFORMATION STORAGE ;;;
(defvar *fixation-data* nil "Tabular list of lists of collected fixations plus word/item/exp information")
(defvar *attachment-data* nil "Table of collected attachment times")
(defvar *encoding-data* nil "Table of collected word encoding durations")
(defvar *timeout-data* nil "Table of recorded time-out regressions")
(defvar *trialmessage-data* nil "Table of recorded trialmessages")
(defvar *visloc-activations-data* nil "Table of recorded visloc activations")
(defvar *fixations-file* (concatenate 'string *output-dir* "/fixations.txt"))
(defvar *attachments-file* (concatenate 'string *output-dir* "/attachments.txt"))
(defvar *encodings-file* (concatenate 'string *output-dir* "/enctimes.txt"))
(defvar *timeouts-file* (concatenate 'string *output-dir* "/timeouts.txt"))
(defvar *trialmessages-file* (concatenate 'string *output-dir* "/trialmessages.txt"))
(defvar *visloc-activations-file* (concatenate 'string *output-dir* "/visloc-activations.txt"))




;;; DEMOS ;;;
(defvar *demo* "the dog bit the boy *stop*")
(defvar *demo1* "the boy that the dog with the fish bit saw the man *stop*")
(defvar *demo2* "kein mann der einen bart hatte war jemals gluecklich *stop*")
(defvar *demo3* "Sophie und Maria gruessten den Direktor xden die Schwester von Maria und die Mutter von Franziska ignoriert hatten *stop*")




;;; Shortcuts
(defun ps (sentence &key (time *max-time*) (params nil))
  (present-whole-sentence sentence time params))

(defun pl (&optional (params nil))
  (present-sentence-list :params params))

(defun pn (n &optional (params nil))
  (present-sentence-number n :params params))

(defun rps (sentence times &key (time *max-time*) (params nil))
  (let ((success nil))
  (delete-output)
  (setprint off)
  (setf *VERBOSE* nil)
  (setf *show-window* nil)
  (dotimes (i times)
    (setf *experiment* "rps")
    (setf *simulation* (+ 1 i))
    (setf *item* 1)
    (format t "~%Iteration ~D" (+ 1 i))
    (setf success (present-whole-sentence sentence time params T))
    (when (null success)
          ; (trialmessage "fail" "T")
          (format t " F!")
          )
    )
  (setprint on)
  ))


;;; Main function for presenting a sentence
(defun present-whole-sentence (sentence time &optional (params nil) (performance nil))
  ; (sgp-fct '(:model-warnings nil))
  (if params
      (setf *params* params)
      (setf params nil))
  (if performance (setf *params* (append *performance-on-params* *params*)) (setf *params* (append *performance-off-params* *params*)))
  ; (format t "~s~%" *params*)
  (soft-reset-sp)
  (sgp-fct *output-setting*)
    
  (let ((plist sentence)
        (stringlist nil)
        ; (em-trace nil)
        ; (fixations nil)
        (x *start-x*)
        (window (open-exp-window "Sentence Experiment"
                                 				 :visible *show-window*
                                 				 :width *width*
                                 				 :x 300
                                 				 :y 300)))
    
    (if (not (listp plist)) (setf plist (string->listOflists plist)))
    (if (listp sentence)
        (progn
          (setf stringlist (mapcar (lambda (x) (format nil "~a" (word-name x))) sentence))
          (setf sentence (format nil "~{~a ~}" stringlist))))
    
    (setf *attach-times* nil)
    (setf *attached-positions* nil)
    ; (setf *attached-items* nil)
    (setf *fixation-trace* nil)
    (setf *encoded-positions* nil)
    (setf *encoded-items* nil)
    (setf *timeout-items* nil)
    (setf *trialmessages* nil)
    (setf *fix-start-time* 0)
    (setf *fix-loc* #(10 150))
    (setf *sentence* sentence)
    (setf *sentence-plist* plist)
    (setf *word* nil)
    (setf *current-index* -1)
    (setf *visloc-list* (make-list (length plist)))
    (setf *visloc-activations* (list (make-list (length plist))))
    
    (add-text-to-exp-window :text sentence :x x :y 150)
    
    (install-device window)
    ; (proc-display :clear t)
    (proc-display)
    
    (if *prefix-fct* (funcall *prefix-fct*))
    
    (when *VERBOSE* 
(format t "
********************************************************************

SENTENCE:   ~s

********************************************************************

" sentence))
    
    (reset-emma)
    
    (when *record-visloc-activations* (start-visloc-record))
        
    ; (run time :full-time t))
    (setf *reading-time* (run time :real-time *real-time*))    
    (if *postfix-fct* (funcall *postfix-fct*))
    (if (equal "stop" (chunk-slot-value-fct (buffer-read 'goal) 'state))
          T
          (trialmessage "fail" "T")
        )
    ; (setf em-trace (get-em-trace))
    (setf *fixation-trace* (em-trace->fixations (get-em-trace) plist))
    ; (setf *fixations* fixations)
    
    (when *record-times* 
      (record-fixations plist *item* *fixation-trace*)
      ; (record-attachment-times *item* *attached-items*)
      (record-attachment-times *item* (parsing-get-attached-items))
      (record-encoding-times *item* *encoded-items*)
      (record-timeouts *item* *timeout-items*)
      ; (record-trialmessages *trialmessages*)
      (write-table *trialmessages* *trialmessages-file*)
      (when *record-visloc-activations*
        (write-table (table *visloc-activations*) *visloc-activations-file*))
      )
    (if (equal "stop" (chunk-slot-value-fct (buffer-read 'goal) 'state))
        T
        nil
        )
    ))




(defun present-sentence-number (n &key (set *sentences*) (time *max-time*) (params nil))
  (present-whole-sentence (nth (1- n) set) time params))

(defun present-sentence-list (&key (set *sentences*) (time *max-time*) (params nil))
  "This function calls present-whole-sentence once for each 
   item in set and passes it the time as supplied."
  (setf *item* 0)
  (delete-output)
  (dolist (x set)
    (incf *item*)
    (present-whole-sentence x time params))
  )

(defun start(&rest s)
  (if s
    (present-sentence-list :set s :time 0.05)
    (present-sentence-list :time 0.05))
  )

(defun next(&optional (time 0.05))
  (run time)
  (print-runtime-vars)
  )


(defun test(&rest s)
  (let ((iterations 1))
    (when (numberp (car s))
      (setf iterations (pop s)))
    (do ((j iterations (- j 1)))
	((= j 0))
      (if s
        (present-sentence-list :set s)
	(present-sentence-list))))
  )



(defun trace-to-file (sentence filename)
  (with-open-file (*standard-output* filename :direction :output  :if-exists :overwrite)
  ;(with-open-file (*standard-output* filename :direction :output)
      (present-whole-sentence sentence *max-time*)))


;;(defmethod rpm-window-key-event-handler ((win rpm-window) key)
;;  (setf *response-time* (pm-get-time))
;;  (setf *response* (string key)))



(defun permute-list (lis)
  "This function returns a random permutation of the list that's passed in."
  (do* ((item (nth (random (length lis)) lis) (nth (random (length temp)) temp))
        (temp (remove item lis) (remove item temp))
        (result (list item) (cons item result)))
       ((null temp) result)))




(defun write-table (data &optional (location (concatenate 'string *output-dir* "/some-data.txt")))
  (with-open-file (outfile (ensure-directories-exist location) :direction :output :if-exists :append :if-does-not-exist :create)
                  (dolist (row data)
                    (dolist (x row)
                      (format outfile "~S " x))
                    (format outfile "~%")))
  t)






;;; ========================= DEMO FUNCTIONS =========================
(defun demo (&optional (params nil))
 (present-whole-sentence *demo* 30 params)
)

(defun demo1 (&optional (params nil)) 
 (present-whole-sentence *demo1* 30 params)
)

(defun demo2 (&optional (params nil)) 
 (present-whole-sentence *demo2* 30 params)
)

(defun demo3 (&optional (params nil)) 
 (present-whole-sentence *demo3* 30 params)
)
;;; ===================================================================








;;;
;;; PRINT RUNTIME VARIABLES 
;;;
(defun print-info ()
  (format t "Sentence: ~s~%" *sentence*)
  (format t "~s~%" *sentence-plist*)
  (parsing-print-info)
  (format t "Encoded items: ~s~%" *encoded-items*)
  (format t "Time-out items: ~s~%" *timeout-items*)
  (format t "Fixation location: ~s~%" (current-eye-loc))
;  (format t "Traces: ~s~%" *traces*)
;  (format t "Response: ~s~%" *response*)
  )


;;;
;;; PRINT INTERFACE PARAMS
;;;
(defun print-interface-params ()
  (format t "*VERBOSE* ~s~%" *VERBOSE*)
  (format t "*real-time* ~s~%" *real-time*)
  (format t "*show-window* ~s~%" *show-window*)
  (format t "*trace-to-file* ~s~%" *trace-to-file*)
  (format t "*record-times* ~s~%" *record-times*)
  (format t "*estimating* ~s~%" *estimating*)
  (format t "*output-setting* ~s~%" *output-setting*)
  )



;;;
;;;  DELETE OUTPUT
;;;
(defun delete-output ()
  (let ((file (first (directory (concatenate 'string *output-dir* "/fixations.txt")))))
    (if file (delete-file file)))
  (let ((file (first (directory (concatenate 'string *output-dir* "/enctimes.txt")))))
    (if file (delete-file file)))
  (let ((file (first (directory (concatenate 'string *output-dir* "/attachments.txt")))))
    (if file (delete-file file)))
  (let ((file (first (directory (concatenate 'string *output-dir* "/timeouts.txt")))))
    (if file (delete-file file)))
  (let ((file (first (directory (concatenate 'string *output-dir* "/trialmessages.txt")))))
    (if file (delete-file file)))
  (let ((file (first (directory *visloc-activations-file*))))
    (if file (delete-file file)))
  (let ((file (first (directory (concatenate 'string *output-dir* "/subjects.txt")))))
    (if file (delete-file file)))
  )



;;;
;;; LOOKUP TABLE FOR TREE-VIEWER
;;;
(defvar *valid-parse-slots-for-tree-viewer*)
(setf *valid-parse-slots-for-tree-viewer* '(ID cat coindexed-with head spec comp comp2 modifier adjunct conjunct conjunct-head adj-modifier))
(defun valid-parse-slot-for-tree (chunk-type slot)
  ;; return t if you want to use the given slot in the chunk-type
  ;; during the graph-trace.
  ;; Some sort of look-up or other table reference needs to be
  ;; built for this.
  ;; default assumes all slots valid.
  (if (member slot *valid-parse-slots-for-tree-viewer*) t)
  ; t
  )

