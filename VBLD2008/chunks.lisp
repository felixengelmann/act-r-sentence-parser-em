;;;-*-LISP-*-
;;; ===================================================================
;;;    ACT-R Sentence Parsing Model
;;;      
;;; Copyright (C) 2006 Sven Bruessow, Shravan Vasishth, Rick Lewis
;;;
;;; This file is part of the ACT-R Sentence Parsing Model processing
;;; German negative and positive polarity items as described in the
;;; Cognitive Science article  Vasishth, Bruessow & Lewis (2007).
;;;	
;;;    The original English model is described in the Cognitive Science
;;;    article Lewis & Vasishth (2004). 
;;; 
;;;    Version 2.9
;;;    July 15 2006
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



(noun mann  :id mann-n-nom
            :case     nom
            :number   singular
            :gender   masc
)

(noun pirat  :id pirat-n-nom
            :case     nom
            :number   singular
            :gender   masc
)

(noun bart  :id bart-n-acc
            :case     acc
            :number   singular
            :gender   masc
)

(noun braten  :id braten-n-acc
            :case     acc
            :number   singular
            :gender   masc
)


;; R E L A T I V E   P R O N O U N S

(noun der :id     der-n
          :cat    wh-pronoun
          :case   nom
          :gender masc
          :number singular
)

(noun den :id     den-n
          :cat    wh-pronoun
          :case   acc
          :gender masc
          :number singular
)


;; D E T E R M I N E R

(det ein    :id       ein-det-nom
            :case     nom
            :number   singular
            :gender   masc
            :polarity positive
)

(det kein   :id        kein-det-nom
            :case      nom
            :number    singular
            :gender    masc
            :polarity  negative
)

(det einen  :id       einen-det-acc
            :case     acc
            :number   singular
            :gender   masc
            :polarity positive
)

(det keinen :id        keinen-det-acc
            :case      acc 
            :number    singular
            :gender    masc
            :polarity  negative
)

;; V E R B S

(verb trug  :id trug-v
            :cat    V
            :number singular
            :tense  past
)

(verb gegessen  :id gegessen-v
            :cat    V
            :number singular
            :tense  past
)

(verb war   :id     war-i-v
            :cat    I-V
            :number singular
            :tense  past
)


(verb hatte :id     hatte-i-v 
            :cat    I-V
            :number singular
            :tense  past
)



;; A D J E C T I V E S

(adj gluecklich :id gluecklich-adj)
(adj sparsam :id sparsam-adj)



;; A D V E R B S

(adv jemals   :id jemals-adv   :polarity negative)
(adv durchaus :id durchaus-adv :polarity positive)
;(adv immer    :id immer-adv    :polarity zero)


