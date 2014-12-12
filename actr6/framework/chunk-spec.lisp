;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : chunk-spec.lisp
;;; Version     : 1.1
;;; 
;;; Description : Definition of chunk specifications and corresponding functions
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Finish the documentation.
;;;             : * Investigate optimizations after there's some use.
;;;             : * Add a function to check chunk-specs to make module writing
;;;             :   easier.
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation.
;;; 2004.12.29 Dan
;;;             : Realized that the comparitors are backwards with respect
;;;             : to productions in test-chunk-slots.
;;; 2005.02.03 Dan
;;;             : * Changing the internal slot-value-lists of a chunk to be a
;;;             :   hash-table instead of an alist...
;;; 2005.02.09 Dan
;;;             : * Some minor cleanup - changing member to find where possible.
;;; 2005.04.19 Dan
;;;             : * Added pprint-chunk-spec.
;;; 2005.05.16 Dan
;;;             : * Modified chunk-spec-variable-p to test that the name has
;;;             :   a length > 1 to reject the symbol itself as a variable.
;;;             :   That fixes a minor problem in production parsing and I
;;;             :   don't think it breaks anything else.
;;; 2005.09.09 Dan
;;;             : * Renamed chunk-to-chunk-spec chunk-name-to-chunk-spec to
;;;             :   clarify its use because I introduced a bug somewhere
;;;             :   along the line with its usage that didn't actually affect
;;;             :   any existing modules, but may cause problems for other
;;;             :   module writers.
;;;             : * Also fixed chunk-name-to-chunk-spec because it didn't 
;;;             :   include nil slots in the spec, but it probably should (it
;;;             :   did prior to my "fixing" it when I changed over to hash
;;;             :   tables).
;;; 2005.11.17 Dan
;;;             : * Fixed chunk-name-to-chunk-spec because it incorrectly
;;;             :   referenced the internal chunk-type slot list instead of
;;;             :   using ct-slot-names.
;;; 2006.09.11 Dan
;;;             : * Changed chunk-slot-equal so that it uses equalp instead of
;;;             :   equal when the string and chunk checks fall through because
;;;             :   numbers (which is the typical value that'd fall through)
;;;             :   don't test well with equal...
;;; 2007.06.18 Dan
;;;             : * Added slot-specs-to-chunk-spec-list and chunk-spec-to-chunk-def
;;;             :   as "official" commands now and also added slot-specs-to-chunk-spec
;;;             :   which removes the need to call define-chunk-spec after slot-specs-
;;;             :   to-chunk-spec-list.
;;; 2007.07.02 Dan
;;;             : * Changed PROCESS-SLOTS-SPECS so that it keeps the slot-spec
;;;             :   in the order provided during definition.
;;; 2007.08.07 Dan
;;;             : * Fixed pprint-chunk-spec so that it actually has the : on the
;;;             :   front of the request parameters.
;;; 2008.06.16 Dan
;;;             : * Changed test-chunk-slots so that it always fails if there
;;;             :   are unbound variables even when they're used in other 
;;;             :   tests.  Doesn't change the normal operation, just shuts
;;;             :   down the possibility of "odd" chunk-specs doing unusual
;;;             :   matches that result in a true result.
;;; 2008.06.17 Dan
;;;             : * Changed test-chunk-slots so that it ignores request parameters
;;;             :   in the chunk-spec for matching purposes.
;;;             : * Added tests for current model and mp to match-chunk-spec-p.
;;; 2008.06.18 Dan
;;;             : * Added a test to find-matching-chunks to verify that the 
;;;             :   chunks parameter is a list when it's not :all.
;;;             : * Fixed the warning in chunk-spec-slot-spec to name the right
;;;             :   function.
;;; 2008.06.20 Dan
;;;             : * Changed chunk-spec-to-chunk-def so that tests to make sure
;;;             :   the parameter is a chunk-spec.
;;;             : * Changed chunk-spec-to-chunk-def so that it also rejects a
;;;             :   spec with request parameters.
;;;             : * Removed the macros of slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec and renamed the functions to not
;;;             :   have the -fct since none of the other chunk-spec manipulation
;;;             :   commands use that convention and they aren't the types of
;;;             :   things that would be entered 'directly' and need a macro
;;;             :   form.
;;; 2008.07.31 Dan
;;;             : * Took chunk-slot-equal out of here and put it into the chunks
;;;             :   file.
;;; 2008.10.30 Dan
;;;             : * Performance improvement for test-chunk-slots - fewer loops
;;;             :   over the spec/slots.
;;; 2013.01.28 Dan
;;;             : * Changed the internal storing of the chunk-type to the name
;;;             :   since that's what got used anyway and changed process-slots-specs
;;;             :   to call valid-chunk-type-slot instead (which is the command
;;;             :   it really should have been using anyway) and similarly don't
;;;             :   use the internal chunk/chunk-type accessors in chunk-name-to-chunk-spec.
;;; 2013.02.14 Dan
;;;             : * Changed strip-request-parameters-from-chunk-spec so that it
;;;             :   maintains the ordering of the slots and removed an unneeded
;;;             :   cond from chunk-spec-slot-spec.
;;; 2013.03.13 Dan [1.1]
;;;             : * Changed process-slots-specs so that it allows a parent type
;;;             :   to specify slots of child types in chunk-specs.
;;;             : * Changed test-chunk-slots so that it requires that a chunk
;;;             :   have the specified slot as well as matching its value --
;;;             :   not having the slot is not the same as having the slot with
;;;             :   a value of nil.
;;; 2013.03.18 Dan 
;;;             : * Fixed some order dependence issues in test-chunk-slots.
;;; 2013.06.07 Dan
;;;             : * Changed test-chunk-slots so that it now requires a parameter
;;;             :   indicating whether the chunk is static or not because now a
;;;             :   static chunk which doesn't have a particular slot will match
;;;             :   to a spec which says "= <that slot> nil" because for statics
;;;             :   not having a slot is the same as having a slot with an explict
;;;             :   value of nil.
;;; 2014.02.14 Dan
;;;             : * Fixed a bug in test-chunk-slots with how it tests variables
;;;             :   between slots that was broken with the 13/3/13 change.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The structures are not for external use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



(defmacro define-chunk-spec (&rest specifications)
  `(define-chunk-spec-fct ',specifications))

(defun define-chunk-spec-fct (specifications-list)
  (verify-current-mp  
   "define-chunk-spec-fct called with no current meta-process."
   (verify-current-model
    "define-chunk-spec-fct called with no current model."
    (cond ((null specifications-list)
           (print-warning "No specification in call to define-chunk-spec."))
          ((= (length specifications-list) 1)
           (if (get-chunk (car specifications-list))
               (chunk-name-to-chunk-spec (car specifications-list))
             (print-warning 
              "define-chunk-spec's 1 parameter doesn't name a chunk: ~S" 
              specifications-list)))
          
          ((not (eq (car specifications-list) 'isa))
           (print-warning 
            "First element to define-chunk-spec isn't the symbol ISA. ~s" 
            specifications-list))
          ((not (get-chunk-type (second specifications-list)))
           (print-warning 
            "Second element in define-chunk-spec isn't a chunk-type. ~S" 
            specifications-list))
          (t
           (let* ((type-name (second specifications-list))
                  (new-spec (make-act-r-chunk-spec :type type-name))
                  (slots (process-slots-specs type-name (cddr specifications-list))))
             (unless (eq slots :error)
               (setf (act-r-chunk-spec-slots new-spec) slots)
               new-spec)))))))

(defun chunk-name-to-chunk-spec (chunk-name)
  (when (chunk-p-fct chunk-name)
    (let* ((ct (chunk-chunk-type-fct chunk-name))
           (spec (make-act-r-chunk-spec :type ct)))
      (dolist (slot (chunk-type-slot-names-fct ct) spec)
        (push (make-act-r-slot-spec :name slot 
                                    :value (fast-chunk-slot-value-fct chunk-name slot))
              (act-r-chunk-spec-slots spec))))))


(defun chunk-spec-chunk-type (chunk-spec)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning 
          "chunk-spec-chunk-type called with a non-chunk-spec"))
        (t
         (act-r-chunk-spec-type chunk-spec))))

(defun chunk-spec-slots (chunk-spec)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning 
          "chunk-spec-slots called with something other than a chunk-spec"))
        (t
         (remove-duplicates (mapcar #'act-r-slot-spec-name
                              (act-r-chunk-spec-slots chunk-spec))))))
  
(defun chunk-spec-slot-spec (chunk-spec &optional slot)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning 
          "chunk-spec-slot-spec called with something other than a chunk-spec"))
        ((and slot (find slot (act-r-chunk-spec-slots chunk-spec) :key #'act-r-slot-spec-name))
         (mapcar #'slot-spec-to-list 
           (remove-if-not #'(lambda (x)
                              (eq x slot))
                          (act-r-chunk-spec-slots chunk-spec)
                          :key #'act-r-slot-spec-name)))
        (slot
         (print-warning 
          "Slot ~S is not specified in the chunk-spec." slot))
        (t
         (mapcar #'slot-spec-to-list 
           (act-r-chunk-spec-slots chunk-spec)))))

(defun slot-in-chunk-spec-p (chunk-spec slot)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning 
          "slot-in-chunk-spec-p called with something other than a chunk-spec"))
        (t
         (find slot (mapcar #'act-r-slot-spec-name
                              (act-r-chunk-spec-slots chunk-spec))))))

(defun slot-spec-to-list (slot-spec)
  (list (act-r-slot-spec-modifier slot-spec)
        (act-r-slot-spec-name slot-spec)
        (act-r-slot-spec-value slot-spec)))
         
(defun process-slots-specs (chunk-type specs)
  (let ((slots nil))
    (loop 
      (when (null specs)
        (return (reverse slots)))
      (let ((spec (make-act-r-slot-spec)))
        (when (find (car specs) '(= - > < >= <=))
          (setf (act-r-slot-spec-modifier spec) (pop specs)))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (unless (or (possible-chunk-type-slot chunk-type (car specs)) 
                    (keywordp (car specs)))
          (print-warning "Invalid slot-name ~S in call to define-chunk-spec." 
                         (car specs))
          (return :error))
        (setf (act-r-slot-spec-name spec) (pop specs))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (setf (act-r-slot-spec-value spec) (pop specs))
        (push spec slots)))))

(defun chunk-spec-variable-p (chunk-spec-slot-value &optional (char #\=))
  (and (symbolp chunk-spec-slot-value) 
       (eql (aref (string chunk-spec-slot-value) 0) char)
       (> (length (string chunk-spec-slot-value)) 1)))


(defun match-chunk-spec-p (chunk-name chunk-spec 
                                      &key (=test #'chunk-slot-equal) 
                                      (-test #'chunk-slot-not-equal)
                                      (>test #'safe>) (>=test #'safe>=) 
                                      (<test #'safe<) (<=test #'safe<=)
                                      (variable-char #\=))
  (verify-current-mp  
   "Match-chunk-spec-p called with no current meta-process."
   (verify-current-model
    "Match-chunk-spec-p called with no current model."
    (let ((chunk (get-chunk chunk-name)))
      (cond ((null chunk)
             (print-warning "~s does not name a chunk in call to match-chunk-spec-p." chunk-name))
            ((not (act-r-chunk-spec-p chunk-spec))
             (print-warning "~s is not a valid chunk-spec in call to match-chunk-spec-p." chunk-spec))
            ((find (act-r-chunk-spec-type chunk-spec)
                   (act-r-chunk-type-supertypes (act-r-chunk-chunk-type chunk)))
             
             (handler-case (test-chunk-slots (act-r-chunk-type-static (act-r-chunk-chunk-type chunk)) 
                                             (act-r-chunk-slot-value-lists chunk)
                                             (act-r-chunk-spec-slots chunk-spec)
                                             =test -test >test >=test <test <=test
                                             variable-char)
               (error (condition) 
                 (print-warning "Error ~S encountered in matching chunk ~s." condition chunk-name)))
             
             
             
             )
            (t
             nil))))))



(defun chunk-slot-not-equal (arg1 arg2)
 (not (chunk-slot-equal arg1 arg2)))

(defun test-chunk-slots (static slots specs =test -test >test >=test <test <=test variable-char)
  (let ((bindings nil)
        (rest nil))
    
    ;; First pass to get all the bindings
    
    (dolist (x specs)
      (let ((value (act-r-slot-spec-value x))
            (modifier (act-r-slot-spec-modifier x))
            (slot (act-r-slot-spec-name x)))
        
        (cond ((chunk-spec-variable-p value variable-char)
               (if (assoc value bindings) ;; The variable has been encountered before
                   (aif (cdr (assoc value bindings)) ;; it's got a binding already
                        (multiple-value-bind (content exists) (gethash slot slots)
                          ;; If things are consistent this spec is done otherwise it has failed
                          (unless (and exists (funcall (case modifier
                                                         (= =test)
                                                         (- -test)
                                                         (> >test)
                                                         (>= >=test)
                                                         (< <test)
                                                         (<= <=test))
                                                       content it))
                            (return-from test-chunk-slots nil)))
                        ;; otherwise it doesn't have a binding yet
                        (multiple-value-bind (content exists) (gethash slot slots)
                          (if (and exists content)
                              ;; If the slot is full make that the binding
                              (setf (cdr (assoc value bindings)) content)
                            ;; otherwise we've failed to match the spec
                            (return-from test-chunk-slots nil))))
                 ;; first encounter with this variable
                 (if (eq modifier '=)
                     ;; if it's got a value in the slot make that the binding
                     (multiple-value-bind (content exists) (gethash slot slots)
                       (if (and exists content)
                           (push (cons value content) bindings)
                         (return-from test-chunk-slots nil)))
                   ;; otherwise save this test and just create an empty binding
                   (progn
                     (push (cons value nil) bindings)
                     (push x rest)))))
              ((keywordp slot)
               ;; skip keywords i.e. request parameters
               )
              (t ;; test constant slots immediately
               (multiple-value-bind (content exists) (gethash slot slots)
                 (unless (or (and exists ;; it's got the slot so test it
                                  (funcall (case modifier
                                             (= =test)
                                             (- -test)
                                             (> >test)
                                             (>= >=test)
                                             (< <test)
                                             (<= <=test))
                                           content value))
                             ;; it is a static which doesn't have the and the test is = nil
                             (and static (null exists) (eq modifier '=) (null value)))
                   (return-from test-chunk-slots nil)))))))
    
    ;; Second pass if all variables bound to check things
    ;; that needed the bindings.
    (unless (some #'(lambda (x) (null (cdr x))) bindings)
      (dolist (x rest t)
        (let* ((modifier (act-r-slot-spec-modifier x))
               (slot (act-r-slot-spec-name x))
               (var (act-r-slot-spec-value x))
               (value (cdr (assoc var bindings))))
          (multiple-value-bind (content exists) (gethash slot slots)
            (unless (and exists (funcall (case modifier
                                           (= =test)
                                           (- -test)
                                           (> >test)
                                           (>= >=test)
                                           (< <test)
                                           (<= <=test))
                                         content value))
              (return-from test-chunk-slots nil))))))))





(defun find-matching-chunks (chunk-spec 
                             &key 
                             (chunks :all) (=test #'chunk-slot-equal) 
                             (-test #'chunk-slot-not-equal)
                             (>test #'safe>) (>=test #'safe>=) 
                             (<test #'safe<) (<=test #'safe<=)
                             (variable-char #\=))
  
  (verify-current-mp  
   "Find-matching-chunks called with no current meta-process."
   (verify-current-model
    "Find-matching-chunks called with no current model."
    
    (let ((found nil))
      (cond ((not (act-r-chunk-spec-p chunk-spec))
             (print-warning 
              "~s is not a valid chunk-spec in call to find-matching-chunks." 
              chunk-spec))
            ((eq :all chunks)
             (maphash #'(lambda (name chunk)
                          (declare (ignore chunk))
                          (when (match-chunk-spec-p name chunk-spec 
                                                    :=test =test :-test -test
                                                    :>test >test :>=test >=test 
                                                    :<test <test :<=test <=test
                                                    :variable-char variable-char
                                                    )
                            (push name found)))
                      (act-r-model-chunks-table (current-model-struct)))
             found)
            ((listp chunks)
             (dolist (name chunks found)
               (when (match-chunk-spec-p name chunk-spec 
                                         :=test =test :-test -test
                                         :>test >test :>=test >=test 
                                         :<test <test :<=test <=test
                                         :variable-char variable-char)
                 (push name found))))
            (t (print-warning "~S isa not a valid value for the :chunks keyword parameter to find-matching-chunks." chunks)))))))
                       


(defun strip-request-parameters-from-chunk-spec (chunk-spec)
  "Return a chunk-spec that doesn't have any request parameters in it"
  (if (act-r-chunk-spec-p chunk-spec)
      (if (some (lambda (x) (keywordp (act-r-slot-spec-name x))) (act-r-chunk-spec-slots chunk-spec))
            (let ((new-chunk-spec (copy-act-r-chunk-spec chunk-spec)))
              (setf (act-r-chunk-spec-slots new-chunk-spec)
                (remove-if #'keywordp (act-r-chunk-spec-slots new-chunk-spec) :key 'act-r-slot-spec-name))
              new-chunk-spec)
          chunk-spec)
    (print-warning "strip-request-parameters-from-chunk-spec called with something other than a chunk-spec.")))

(defun pprint-chunk-spec (chunk-spec)
  "Print a chunk specification in a 'production like' way to the
   command output"
  (when (act-r-chunk-spec-p chunk-spec)
    (command-output "    ISA ~A" (chunk-spec-chunk-type chunk-spec))
    (dolist (slot (act-r-chunk-spec-slots chunk-spec))
      (if (eql '= (act-r-slot-spec-modifier slot))
          (command-output "    ~s ~S"
                          (act-r-slot-spec-name slot)
                          (act-r-slot-spec-value slot))
          (command-output " ~2a ~s ~S"
                          (act-r-slot-spec-modifier slot)
                          (act-r-slot-spec-name slot)
                          (act-r-slot-spec-value slot))))))


(defun slot-specs-to-chunk-spec-list (chunk-type slot-specs)
  (append (list 'isa chunk-type)
          (apply #'append slot-specs)))


(defun slot-specs-to-chunk-spec (chunk-type slot-specs)
  (define-chunk-spec-fct (slot-specs-to-chunk-spec-list chunk-type slot-specs)))



(defun chunk-spec-to-chunk-def (chunk-spec)
  "Convert a chunk-spec to a chunk definition list"
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((slots (chunk-spec-slots chunk-spec))
            (slot-spec-list (chunk-spec-slot-spec chunk-spec)))
        (cond ((and slots
                    (< 1 (apply #'max (mapcar #'(lambda (x) 
                                                  (count x slot-spec-list 
                                                         :key #'second))
                                        slots))))
               (print-warning 
                "A chunk creation request can only specify a slot once."))
              ((notevery #'(lambda (x) (equal '= (car x))) slot-spec-list)
               (print-warning 
                "A chunk creation request may only use the = modifier."))
              ((some #'(lambda (x) 
                         (chunk-spec-variable-p (third x))) 
                     slot-spec-list)
               (print-warning 
                "A chunk creation request may not have variables in the slots."))
              ((some #'(lambda (x) 
                         (keywordp  (second x))) 
                     slot-spec-list)
               (print-warning 
                "A chunk creation request may not have request parameters."))
              (t
               (append (list 'isa (chunk-spec-chunk-type chunk-spec)) 
                       (mapcan #'(lambda (x) 
                                   (list (second x) (third x)))
                         slot-spec-list)))))
    (print-warning "chunk-spec-to-chunk-def called with something other than a chunk-spec.")))

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
