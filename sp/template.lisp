(p set-retrieval-cues-input-
   =goal>
      ISA               comprehend-sentence
      state             "lexical-retrieval"
      goal-cat          
   =retrieval>
      ISA               lexical-entry
      cat               
      subcat            
      word              =word
      case              -lex
==>
   =lex>                =retrieval
   =goal>
      state             "wm-retrieval"
      goal-cat          -goal
      cue1              nil
      cue2              nil
      cue3              nil
      cue4              nil
   +retrieval>                           
      ISA               syn-obj
      cat               
      case              
      waiting-for-cat   wait-for-
      waiting-for-finite   wait-for-finite
      gap               

   !eval! (set-begin-time =word)
)


(p attach-
   =goal>
      ISA               comprehend-sentence
      state             "wm-retrieval"
      goal-cat          
   =retrieval>
      ISA               syn-obj
      cat               
      ID                =ID-
      case              
      number            
      gender            
      next-goal         next-
      waiting-for-case  wait-for-
      waiting-for-cat   wait-for-
   =lex>
      ISA               lexical-entry
      cat               
      word              =word
      case              -lex
      number            
      gender            
==>
   !bind! =goal-cat (map-goal-category =next-goal)
   =goal>
      state             "read"
      goal-cat          -goal
      regression        nil
   
   !bind! =ID-P (new-name P)
   !bind! =goal-P (map-next-goal-category =goal-cat)
   +Pb>
      ISA               syn-obj
      cat               
      ID                =ID-
      case              
      filler            yes-filler/done
      number            
      gender            
      tense             
      head              =word
      spec              
      comp              
      modifier          
      spec-of           
      comp-of           =retrieval
      modif-of          
      adjunct-of            
      conjunct-of       
      waiting-for-case  wait-for
      waiting-for-cat   wait-for
      next-goal         =goal-
      gap               none/open/spec (IP, VP)
      gapped            gapped (IP)
      embedded          embedded
      coind-with        
   =retrieval>
      head              =word
      comp              
      waiting-for-cat   
      waiting-for-case  
   !bind! =currentpos *current-index*
   =imaginal>
      clause-pos        =currentpos
      filler-pos        =currentpos


   !eval! (mod-current-ip (list 
     'number 'sing
     'subj-word =word
     ))
   !eval! (attach-message =word "subject" =subj-word)
   !eval! (set-end-time)
)