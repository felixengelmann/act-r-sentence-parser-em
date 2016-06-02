; TODO: correct frequencies
; TODO: psc.dat ==> sentence.dat created by ACT-R
; TODO: SWIFT output
; TODO: word recognition speed should be constrained by swift (recognition finished when SWIFT activation at maximum) OR: attention can only be shifted to words with state 2 (SWIFT changes state to 2)


;;; SWIFT INTERACTION ;;;
(defvar *la-complete* nil)
(defvar *int-complete* nil)
(defvar *listener* "/tmp/swiftlistener")
(defparameter *swift-running* nil)
(defparameter *word-processing-states* nil)
(defparameter *attention-target* 0)
(defparameter *swift-target-probabilities* '())
(setf *working-dir* (current-directory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWIFT INTERACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-word-processing-completed (index)
  (if (= -1 index) (setf index 0))
  (if *swift-running* (swift-request (mp-time)))
  (format t "set-word-processing-completed ~D~%" (+ 1 index))
  (setf (nth index *word-processing-states*) 2)
  )

(defun reset-word-processing-state (index)
  "Resets processing state of word at index to 1, so it becomes an eye movement target again.
  Called in productions-control.lisp by (find-previous-location) and (start-time-out)."
  (if (= -1 index) (setf index 0))
  (if *swift-running* (swift-request (mp-time)))
  (format t "reset-word-processing-state ~D~%" (+ 1 index))
  (setf (nth index *word-processing-states*) 1)
  )

(defun reset-all-processing-states nil
  (let ((wordlist (split-by-one-space *sentence*)))
    (if *swift-running* (swift-request (mp-time)))
    (format t "reset-all-processing-states~%")
    (setf *word-processing-states* (make-list (length wordlist) :initial-element 1))
    ))

(defun request-swift-state nil
  "Wrapper for (swift-request). Requests word-processing-states from SWIFT at current time."
  (if *swift-running* (swift-request (mp-time))))

(defun recognition-complete (visloc)
  "Tests whether the word index of a given visual location has state 2 or higher in the word-processing-states. Only in this case, ACT-R can shift attention to that word.
  Called in productions-control.lisp at (lexical-retrieval-request) and (encode-word)."
  (let ((index (visloc->index visloc)))
    ; (format t "visloc ~S, index ~S~%" visloc (+ 1 index))
    (> (nth index *word-processing-states*) 1)))

(defun swift-request (rawtime)
  "Sends last updated word-processing-states to SWIFT and requests updated states from SWIFT at given time."
  (let ((msg nil)
        (trgt nil)
        (time (* 1000 rawtime)))
    (format t "swift-send-states ~S at time ~D~%" *word-processing-states* time)
    (write-listener (format nil "(~S)~S" time *word-processing-states*))
    (sleep 0.10)
    (setf msg (read-listener))
    (setf *word-processing-states* (map 'list #'digit-char-p (prin1-to-string (car (first (string-to-list msg))))))
    (setf trgt (car (second (string-to-list msg))))
    ;; SWIFT sends target=0 when finished reading:
    (if (= 0 trgt) (setf *swift-running* nil))
    (format t "swift-read-states ~S~%" *word-processing-states*)
    (setf *attention-target* trgt)
    ; (format t "New target: ~S~%" *attention-target*)
    ))


;;;
;;; LISTENER INTERACTION
;;:
(defun connect-listener nil
  (format t "Connecting to listener ~S~%" *listener*)
  (run-program "mkfifo" (list *listener*))
  )

(defun delete-listener nil
  (format t "Disconnecting listener~%")
  (run-program "rm" (list *listener*))
  )

(defun write-listener (message)
	(with-open-file (listener *listener* :direction :output :if-exists :overwrite)
                  (format listener "~A" message))
  )

(defun read-listener nil
  (with-output-to-string (stream) (run-program "cat" (list *listener*) :output stream))
  )


;;;
;;; SWIFT CONTROL
;;;
(defun start-swift nil
  (delete-listener)
  (connect-listener)
  (sleep 0.3)
  (format t "Initializing SWIFT...~%")
  (cwd "../SWIFT/")
  (run-program "./swift" '() :wait nil)
  (setf *swift-running* T)
  ; (cwd *working-dir*)
  )

(defun stop-swift nil
  (if *swift-running* (swift-request 10000))
  (sleep 0.5)
  (delete-listener)
  (cwd *working-dir*)
  (format t "SWIFT stopped.~%")
  )

(defun connect-swift nil
  (delete-listener)
  (connect-listener)
  (format t "Listener connected. Now run SWIFT from the command line.~%")
  (setf *swift-running* T)
  )


;;;
;;; HELPERS
;;;
(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

 (defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))


