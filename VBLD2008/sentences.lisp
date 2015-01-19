;;;-*-LISP-*-
;;; ===================================================================
;;; ACT-R Sentence Parsing Model
;;;      
;;; Copyright (C) 2006 Sven Bruessow, Shravan Vasishth, Rick Lewis
;;;
;;; This file is part of the ACT-R Sentence Parsing Model processing
;;; German negative and positive polarity items as described in the
;;; Cognitive Science article  Vasishth, Bruessow & Lewis (2007).
;;;	
;;; The original English model is described in the Cognitive Science
;;; article Lewis & Vasishth (2004). 
;;; 
;;; Version 2.9
;;; July 15 2006
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


(defvar *current* nil)

(setf *demo* "kein mann der einen bart hatte war jemals gluecklich *stop*")
(setf *demo1* "ein mann der keinen bart hatte war jemals gluecklich *stop*")
(setf *demo3* "kein pirat der einen  braten gegessen hatte war jemals sparsam *stop*")

(defparameter *cond-a* "kein mann der einen bart hatte war jemals gluecklich *stop*"())
(defparameter *cond-b* "ein mann der keinen bart hatte war jemals gluecklich *stop*")
(defparameter *cond-c* "ein mann der einen bart hatte war jemals gluecklich *stop*")
(defparameter *cond-d* "kein mann der einen  bart hatte war durchaus gluecklich *stop*")
(defparameter *cond-e* "ein mann der keinen bart hatte war durchaus gluecklich *stop*")
(defparameter *cond-f* "ein mann der einen  bart hatte war durchaus gluecklich *stop*")

(defparameter *pirat-a* "kein pirat der einen  braten gegessen hatte war jemals sparsam *stop*")
(defparameter *pirat-b* "ein pirat der keinen  braten gegessen hatte war jemals sparsam *stop*")
(defparameter *pirat-c* "ein pirat der einen  braten gegessen hatte war jemals sparsam *stop*")
(defparameter *pirat-d* "kein pirat der einen  braten gegessen hatte war durchaus sparsam *stop*")
(defparameter *pirat-e* "ein pirat der keinen  braten gegessen hatte war durchaus sparsam *stop*")
(defparameter *pirat-f* "ein pirat der einen  braten gegessen hatte war durchaus sparsam *stop*")



(define-experiment vbld08-mann
  :full-name "Vasishth, Bruessow, Lewis (2007) Exp."
  :conditions
  ((a   *cond-a*    ())
   (b   *cond-b*    ())
   (c   *cond-c*    ())
   (d   *cond-d*    ())
   (e   *cond-e*    ())
   (f   *cond-f*    ())
   )
  )

(define-experiment vbld08-pirat
  :full-name "Vasishth, Bruessow, Lewis (2007) Exp."
  :conditions
  ((a   *pirat-a*    (PI :position 9 :data .564))
   (b   *pirat-b*    (PI :position 9 :data .701))
   (c   *pirat-c*    (PI :position 9 :data .706))
   (d   *pirat-d*    (PI :position 9 :data .572))
   (e   *pirat-e*    (PI :position 9 :data .477))
   (f   *pirat-f*    (PI :position 9 :data .425))
   )
  )


;;;
;;; Search param-space
;;;
(setf *pspace* '(
                     (:lf .2 .4 .1)
                     (:ans .15 .45 .15)
                     (:mp 1 2 .5)
                     ))
