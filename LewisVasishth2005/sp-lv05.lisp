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


;; WORKING DIRECTORY
;(cwd "/Users/felix/Dropbox/Workspace/ACT-R_EMMA/PortedParser3/")
;;#+:acl(set-mac-default-directory #P"/Users/felix/Dropbox/Workspace/ACT-R_EMMA/PortedParserEMMA/")
; (setf *default-pathname-defaults* #P"/Users/felix/Workspace/ACT-R-Parser/Projects/LewisVasishth2005/")


(setf *output-dir* "output")
(defparameter *read-corpus* NIL)
(defvar *lang*)
; (setf *lang* 'spanish)
(setf *lang* 'english)



(defun load-sp-core NIL
  (load "../sp/helper-functions")
  (load "../sp/interface")
  (load "../sp/interface-emma")
  (load "../sp/experiment-control-original")
  (load "../sp/experiment-control")
  (load "../sp/support-lexicon")
  (load "../sp/interface-parser")
  )


(defun load-model-support-sp nil
  (load "../sp/constants")
  (load "../sp/support-productions")
  (load "../sp/productions-control")
  ; (load "productions-control-adjusted")
  (load "../sp/productions-parser")
  ; (load "demo-productions.lisp")
  ; (load "demo-productions-adjusted.lisp")
  (load "../sp/chunks")
) 


(defun load-sp nil
  (load-sp-core)
  (load "model")
  (load-model-support-sp)
  (load "sentences")
)


;; (clear-all):
;; there is no model defined, the time is set to 0.0, 
;; the event queue is cleared, waiting events are removed and the event hooks are cleared.
(defun clear-sp nil
  (clear-all)
  (reset)
;  (suppress-warnings
    (load "sp-lv05")
);)


;; (reload)
;; Reload is essentially a shortcut for reloading a model file
;; that has been edited to incorporate those changes.
(defun reload-sp nil
 ; (suppress-warnings
    (clear-all)
    (load-sp)
);)
(defun rl NIL
  (reload-sp))


;; (reset):
;; for the current meta-process the time is set to 0.0, 
;; the event queue is cleared, all waiting events are removed 
;; and then each of the currently defined models is reset.
(defun reset-sp nil
  (reset)
  (suppress-warnings
    (load-model-support-sp)
))


(defun soft-reset-sp nil
  ; reset parsing module?
  (reset-sp)
)



(load-sp-core)
(compile-file "../sp/helper-functions")
(compile-file "../sp/interface")
(compile-file "../sp/interface-emma")
(compile-file "../sp/experiment-control-original")
(compile-file "../sp/experiment-control")
(compile-file "../sp/support-lexicon")
; (compile-file "../sp/interface-parser")

(clear-all)
(load-sp)
; (reload-sp)

