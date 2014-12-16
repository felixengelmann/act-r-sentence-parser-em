;;;-*-LISP-*-

(noun girl :id girl-n :gender fem)
(noun boy :id boy-n :gender masc)
(noun man :id man-n :gender masc)
(noun dog :id dog-n)
(noun duck :id duck-n)
(noun assistant :id assistant-n)
(noun reporter :id reporter-n)
(noun photographer :id photographer-n)
(noun nurse :id nurse-n)
(noun administrator :id administrator-n)
(noun receptionist :id receptionist-n)
(noun witness :id witness-n)
(noun lawyer :id lawyer-n)
(noun neighbor :id neighbor-n)
(noun clinic :id clinic-n)
(noun medic :id medic-n)
(noun editor :id editor-n)
(noun editors :id editors-n)
(noun writer :id writer-n)
(noun writers :id writers-n)
(noun student :id student-n)
(noun fish :id fish-n)
(noun hallway :id hallway-n)
(noun exam :id exam-n)
(noun story :id story-n)
(noun book :id book-n)
(noun evidence :id evidence-n)
(noun press :id press-n)
(noun journal :id journal-n)

(noun claim :id claim-n :subcat transitive-CP-finite)
(noun news :id news-n :subcat transitive-CP-finite)

(noun who :id who-n :cat wh-pronoun)

(det the :id the-det)
(det his :id his-det)

(prep with :id with-p)
(prep for :id for-p)
(prep from :id from-p)
(prep by :id by-p)
(prep to :id to-p)
(prep in :id in-p)

(verb bit :id bit-v)
(verb chased :id chased-v)
(verb sent :id sent-v)
(verb supervised :id supervised-v)
(verb scolded :id scolded-v)
(verb hired :id hired-v)
(verb scold :id scold-v :finite infinite)
(verb implicated :id implicated-v)
(verb saw :id saw-v)
(verb amused :id amused-v)
(verb married :id married-v)
(verb quit :id quit-v)
(verb liked :id liked-v)
(verb admired :id admired-v)
(verb surprised :id surprised-v)
(verb upset :id upset-v)

(verb standing :id standing-v    ;; features are not right here
      :finite infinite
      :subcat gerund)
(verb waiting :id waiting-v    ;; features are not right here
      :finite infinite
      :subcat gerund)

(verb examined :id examined-v    ;; features are not right here
      :finite finite
      :subcat past-participle-transitive)



(verb was :id was-i  :cat I-V   :number singular)
(verb is :id is-i  :cat I-V   :number singular)

(verb thought :id thought-v :subcat transitive-CP-finite)
(verb said :id said-v       :subcat transitive-CP-finite)

(verb knew :id knew-v     :subcat transitive-CP-DP-finite)
(verb forgot :id forgot-v :subcat transitive-CP-DP-finite)

(verb cried :id cried-v    :subcat intransitive)
(verb ripped :id ripped-v    :subcat intransitive)
(verb laughed :id laughed-v    :subcat intransitive)
(verb hoped :id hoped-v    :subcat intransitive)
      
(adj important :id important-adj)
(adj wrong :id wrong-adj)

(comp that :id that-c)

(comp forc :id for-c :finite infinite)

(inf toi :id to-inf)


;; Add-on: Staub (2010)
(verb noticed :id noticed-v)
(verb hurried :id hurried-v :subcat intransitive)
(noun fireman :id fireman-n)
(noun employees :id employees-n)


;; Add-on: Kemper et al.
(noun soldiers :id soldiers-n)

