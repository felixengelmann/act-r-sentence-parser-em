
proc select_audicon {} {

  if {[currently_selected_model] == "nil"} {

    tk_messageBox -icon info -type ok -title "Audicon" -message "Inspector tools require a current model."
  } else {

    set win ".audicon_[currently_selected_model]"

    if {[winfo exists $win] == 1} {
      wm deiconify $win
      raise $win
    } else {
      # make it now

      toplevel $win

      wm withdraw $win

      record_new_window $win "Audicon"

      wm geometry $win [get_configuration .audicon $win]

      set f [frame $win.frame -borderwidth 0]  
    
      set t [text $f.text -font text_font -yscrollcommand "$f.scrl set" -state disabled]
          
      set s [scrollbar $f.scrl -command "$t yview"]

      send_environment_cmd \
        "create text-output-handler $t $t \
            (lambda (x) (declare (ignore x)) (print-audicon)) (post) [send_model_name]"

      bind $t <Destroy> {
        remove_handler %W
      }

      # Make the window useable for copy operations on Windows
   
      bind $t <1> {focus %W}
  
      pack $s -side right -fill y 
      pack $t -side left -expand 1 -fill both
  
      place $f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

      # now show the window 

      wm deiconify $win
    }
  }
}

button [control_panel_name].audicon_button \
       -command {select_audicon} -text "Audicon" -font button_font

pack [control_panel_name].audicon_button

