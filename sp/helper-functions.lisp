
;;;
;;;  Word Abstraction
;;;
(defmacro word-name (word) `(first ,word))
(defmacro itemnumber (word) `(second ,word))
(defmacro condname (word) `(third ,word))
(defmacro word-frequency (word) `(fourth ,word))
(defmacro word-sp (word) `(fifth ,word))
(defmacro word-rv (word) `(sixth ,word))
;(defmacro word-freq-class (word) `(seventh ,word))
;(defmacro word-sp-class (word) `(eighth ,word))
;(defmacro word-rv-class (word) `(ninth ,word))





;;;
;;;   Word object helpers
;;;
;; To do: make more generic
(defun obj->wordinfo (obj sentence)
  (let* ((location (obj->location obj))
         (index (location->index location sentence)))
    (nth index sentence)))

(defun obj->location (obj)
  (let* ((screen-pos (chunk-slot-value-fct obj 'SCREEN-POS))
         (x (chunk-slot-value-fct screen-pos 'screen-x))
         (y (chunk-slot-value-fct screen-pos 'screen-y)))
    (list x y)))

(defun object->index (text-obj)
  (let* ((loc (obj->location text-obj))
         (index (location->index loc *sentence-plist*)))
    index
    ))

(defun visloc->location (visloc)
  (let* ((x (chunk-slot-value-fct visloc 'screen-x))
         (y (chunk-slot-value-fct visloc 'screen-y)))
    (list x y)))

(defun visloc->index (visloc)
  (let* ((x (chunk-slot-value-fct visloc 'screen-x))
         (y (chunk-slot-value-fct visloc 'screen-y)))
    (location->index (list x y))))


(defun location->index (location &optional (sentence *sentence-plist*))
  (let ((lx (first location)))
    (if (not (listp sentence)) (setf sentence (string->listOflists sentence)))
    (do ((i 0 (1+ i))
         (x *start-x* x))
        ((or (< lx x) (>= i (length sentence)))
         (max 0 (- i 1)))
      (incf x (+ (* (length (format nil "~a" (word-name (nth i sentence)))) *char-width*)
                 *char-width*)))))


(defun index->location (index &optional (sentence *sentence-plist*))
    (if (not (listp sentence)) (setf sentence (string->listOflists sentence)))
    (do ((i 0 (1+ i))
         (x *start-x* x))
        ((or (= i index) (>= i (length sentence)))
         (+ x (* 0.5 (length (format nil "~a" (word-name (nth i sentence)))) *char-width*))) ; return x + wordlength/2
      (incf x (+ (* (length (format nil "~a" (word-name (nth i sentence)))) *char-width*)
                 *char-width*))))


(defun index->wordname (index sentence)
  (string (word-name (nth index sentence))))




;;; 
;;;  General helper functions
;;;
(defun mymean (l)
  (let ((s (apply '+ l))
        (n (length l)))
    (/ s n)))

(defun mymember (e l) (if (member e l) t))  ;; old
(defun ismember (e l) (if (member e l) t))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun string->listOflists (string)
  (mapcar 'list (split-by-one-space string)))

(defun datetimestamp ()
  (let ((stamp
          (multiple-value-bind
            (second minute hour date month year)
            (get-decoded-time)
            second
            (format nil "~d~2,'0d~d-~2,'0d~2,'0d"
                    year
                    month
                    date
                    hour
                    minute)
            )))
    stamp)
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
;;; Set output options
;;;
(defvar *output-setting-mappings* nil)
(setf (symbol-plist *output-setting-mappings*) '(
    full        (:v t :CMDT t :trace-detail high :trace-filter nil :model-warnings t)
    condensed   (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
    on          (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
    default     (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
    notrace     (:v nil :CMDT t :model-warnings t :buffer-trace nil)
    trace       (:v t :CMDT nil :trace-detail medium :trace-filter nil :model-warnings nil :buffer-trace nil)
    firing      (:v t :CMDT nil :trace-detail high :trace-filter production-firing-only :model-warnings nil)
    off         (:v nil :CMDT nil :model-warnings nil :buffer-trace nil)
    buffertrace (:V t :CMDT nil :trace-detail high :model-warnings NIL :trace-filter nil :buffer-trace t)
    ))

(defmacro setprint (s)
  `(progn
     (sgp-fct (get *output-setting-mappings* ',s))
     (setf *output-setting* (get *output-setting-mappings* ',s))
     ))




;;;
;;; Parameter shortcuts
;;;
(defun get-par (par)
  (no-output 
    (car (sgp-fct (list par)))))

(defun get-parlist (params)
  (no-output 
    (sgp-fct params)))

(defun set-par (params)
  (sgp-fct params))

(defun print-params nil
  (sgp
    :v
    :esc
    :er
    :randomize-time
    :ncnar
    :dcnn
    :short-copy-names
    :lf
    :rt
    :ga
    :ans
    :md
    :mp
    :mas
    :gram-lf
    :gram-rt
    :lex-lf
    :lex-rt
    :gram-force-merge
    :att-util
    :att-util2
    :regr-util
    :sp-time
    :SURPRISAL-FACTOR
    :SURPRISAL-HL-FACTOR
    :VISUAL-ENCODING-FACTOR
    :VISUAL-ENCODING-EXPONENT
    :SACCADE-PREPARATION-TIME
    :FIXED-PREP-TIME
    :EYE-SACCADE-RATE
    :SACCADE-BASE-TIME
    :vis-obj-freq
    :visual-attention-latency
    :visual-finst-span
    :visual-num-finsts
    :visual-onset-span
  ))




;;; (MACROS:
;;; The simplest way to generate a form in the body of your macro expander is to use 
;;; the backquote (`) reader macro. This behaves like the quote (') reader macro, 
;;; except for when a comma (,) appears in the backquoted form.
;;; Like quote, backquote suppresses evaluation. But a comma within a backquoted form 
;;; "unsuppresses" evaluation for just the following subform.)

