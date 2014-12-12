
proc select_vert_graphic_trace {} {
  
    # make a new one

  if {[currently_selected_model] == "nil"} {
    tk_messageBox -icon info -type ok -title "Vertical Graphic Trace" -message "Tracing tools require a current model."
  } else {

    set win [toplevel [new_variable_name .vert_graphic_trace]]

    global $win.scale

    global $win.dm_viewer
    set $win.dm_viewer 0
    global $win.p_viewer
    set $win.p_viewer 0

    wm withdraw $win

    record_new_window $win $win

    wm geometry $win [get_configuration .vert_graphic_trace $win]

     
    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
         -yscrollcommand "$win.frame.scrl set" \
         -width 1000 -height 2000 -scrollregion {0 -4 1000 2000} -bg white
          
          
    scrollbar $win.frame.scrl \
              -command "$win.frame.canvas yview" -orient vertical

    set $win.scale 1.0

    bind $win <MouseWheel> " \
     
     if { %D < 0 } { \
       $win.frame.canvas yview scroll 1 units \
     } else { \
       $win.frame.canvas yview scroll -1 units \
     } "
    
  
    canvas $win.canvas1  -width 1000 -height 30 -bg white


    label $win.text -font text_font  -textvariable $win.textvar
  
    set $win.textvar ""

    label $win.note -font text_font  -text "Notes:"
  
    label $win.notes -font text_font  -textvariable $win.notesvar -anchor w
  
    set $win.notesvar ""

    ## Create a dummy handler to set the :save-buffer-trace parameter to t
    # whenever a buffer-trace window is open.

    send_environment_cmd \
      "create simple-handler $win.note $win.dummy \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-buffer-trace t)) nil) (reset) [send_model_name]"


    bind $win.note <Destroy> "remove_handler $win.note"


    button $win.stop_gt \
      -command "draw_vert_items $win [send_model_name]" -text "Get Trace" -font button_font

    send_environment_cmd \
      "create list-handler $win.stop_gt $win.return (lambda (x) (declare (ignore x))) () [send_model_name]"

    bind $win.stop_gt <Destroy> "remove_handler $win.stop_gt"

    button $win.redisplay -command "display_vert_data $win 1 [send_model_name]" -text "Redisplay" -font button_font

    button $win.zoom_in -command "vert_zoom_in $win" -text "+" -font button_font

    button $win.zoom_out -command "vert_zoom_out $win" -text "-" -font button_font
        
    button $win.hide_text -command "$win.frame.canvas delete trace_text" -text "Remove Text" -font button_font
 
    button $win.save -command "save_vert_graphic_trace $win" -text "Save 1P" -font button_font
    button $win.save2 -command "save_vert_graphic_trace_multi $win" -text "Save Multi." -font button_font

    button $win.save_data -command "save_v_graphic_trace_data $win" -text "Save data" -font button_font
    button $win.read_data -command "read_v_graphic_trace_data $win" -text "Read data" -font button_font



    label $win.range -font text_font -text "Range:"
    label $win.to -font text_font -text "to"
 
    entry $win.min -textvariable $win.min_extent -text "" -font text_font
    entry $win.max -textvariable $win.max_extent -text "" -font text_font
    
    pack $win.frame.scrl -side right -fill y
    pack $win.frame.canvas -side top -fill both 

    place $win.frame -x 0 -y 30 -relwidth 1.0 -height -110 -relheight 1.0
    place $win.canvas1  -x 0 -y 0 -relwidth 1.0 -height 30
    
    place $win.note -x 0 -rely 1.0 -y -79 -width 60 -height 28
    place $win.notes -x 60 -rely 1.0 -y -79 -relwidth 1.0 -height 28

    place $win.text -x 0 -rely 1.0 -y -50 -relwidth .3 -height 49

    place $win.stop_gt -relx .3 -rely 1.0 -y -50 -relwidth .1 -height 24
    place $win.redisplay -relx .4 -rely 1.0 -y -50 -relwidth .1 -height 24
    place $win.hide_text -relx .5 -rely 1.0 -y -50 -relwidth .1 -height 24
    place $win.save -relx .6 -rely 1.0 -y -50 -relwidth .1 -height 24
    place $win.save2 -relx .7 -rely 1.0 -y -50 -relwidth .1 -height 24

    place $win.save_data -relx .8 -rely 1.0 -y -50 -relwidth .1 -height 24
    place $win.read_data -relx .9 -rely 1.0 -y -50 -relwidth .1 -height 24

    place $win.zoom_in -relx .3 -rely 1.0 -y -25 -relwidth .1 -height 24
    place $win.zoom_out -relx .4 -rely 1.0 -y -25 -relwidth .1 -height 24
    place $win.range -relx .5 -rely 1.0 -y -25 -relwidth .07 -height 24
    place $win.min -relx .57 -rely 1.0 -y -25 -relwidth .2 -height 24
    place $win.to -relx .77 -rely 1.0 -y -25 -relwidth .03 -height 24
    place $win.max -relx .8 -rely 1.0 -y -25 -relwidth .2 -height 24

    # now show the window 

    wm deiconify $win
  }
} 


button [control_panel_name].vert_graphic_trace_button \
       -command {select_vert_graphic_trace} -text "Vert. Buffer Trace" -font button_font

pack [control_panel_name].vert_graphic_trace_button


proc vert_zoom_in {win} {
     
 global $win.scale
 upvar $win.scale scale

  if {$scale < 16} {
       set scale [expr 2 * $scale]
       $win.frame.canvas scale scale_items 0 0 1.0 2.0
       $win.frame.canvas configure -scrollregion "0 -4 1000 [expr 2 * [lindex [$win.frame.canvas cget -scrollregion] 3]]"
       $win.frame.canvas configure -height [expr 2 * [$win.frame.canvas cget -height]]
  }
}


proc vert_zoom_out {win} {

 global $win.scale
 upvar $win.scale scale

     set scale [expr .5 * $scale]
     $win.frame.canvas scale scale_items 0 0 1.0 0.5 
     $win.frame.canvas configure -scrollregion "0 -4 1000 [expr .5 * [lindex [$win.frame.canvas cget -scrollregion] 3]]"
     $win.frame.canvas configure -height [expr .5 * [$win.frame.canvas cget -height]]
    }

proc draw_vert_items {win model} {

  global $win.return
               
  upvar $win.textvar display
                
  set display "Busy"
                
  $win.zoom_in configure -state disabled
  $win.zoom_out configure -state disabled
  $win.hide_text configure -state disabled
  $win.stop_gt configure -state disabled
  $win.save configure -state disabled
  $win.save2 configure -state disabled
  $win.redisplay configure -state disabled
  $win.save_data configure -state disabled
  $win.read_data configure -state disabled

  set $win.return ""
                  
  send_environment_cmd "update [get_handler_name $win.stop_gt] vert-graphic-trace-return"

  wait_for_non_null $win.return

  display_vert_data $win 1 $model

  $win.zoom_in configure -state normal
  $win.zoom_out configure -state normal
  $win.hide_text configure -state normal
  $win.stop_gt configure -state normal
  $win.save configure -state normal
  $win.save2 configure -state normal
  $win.redisplay configure -state normal
  $win.save_data configure -state normal
  $win.read_data configure -state normal


  set display "Done"
}


proc display_vert_data {win level model} {

  $win.frame.canvas delete trace_items
  $win.canvas1 delete label_tag

  global $win.scale
  upvar $win.scale scale

  set scale 1.0

  set min_y 0
  set max_y 0

  set min_res [scan [$win.min get] "%f" min_y]
  set max_res [scan [$win.max get] "%f" max_y]

  upvar $level $win.return result

  if {$min_res < 1 || $max_res < 1 || $min_y >= $max_y} {
    foreach x $result {
  
      switch [lindex $x 0] {
        label { 
          $win.frame.canvas create text [expr 40 + [lindex $x 2]] -60 -anchor n -font text_font -text [lindex $x 1] -width [lindex $x 4]  -fill [lindex $x 3] -tag trace_items
          $win.canvas1 create text [expr 40 + [lindex $x 2]] 5 -anchor n -font text_font -text [lindex $x 1] -width [lindex $x 4] -tag label_tag -fill [lindex $x 3]
          if {[lindex $x 1] == "production"} {
            set pop_up 1
          } elseif {[lindex $x 1] == "retrieval"} {
            set pop_up 2
          } else {
            set pop_up 0
          }
        }
        size { 
          $win.frame.canvas configure -height [expr [lindex $x 2] + 5]
          $win.frame.canvas configure -scrollregion "0 -4 1000 [expr 1 + [lindex $x 2]]"
        }

        rectangle {
          set box_name [new_variable_name box]

          $win.frame.canvas create rectangle [expr 40 + [lindex $x 1]] [lindex $x 2] [expr 40 + [lindex $x 3]] [lindex $x 4] -width 1 -fill [lindex $x 5] -outline black -tag [list trace_items $box_name scale_items display_boxes]

          if {[expr 40 + [lindex $x 3]] > [$win.frame.canvas cget -width]} {
            $win.frame.canvas configure -width [expr 40 + [lindex $x 3]]
          }


          if {[lindex $x 8] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar {[lindex $x 8]}" 
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } elseif {[lindex $x 7] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar \"[lindex $x 7]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } elseif {[lindex $x 6] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar \"[lindex $x 6]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } else {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\""
          }


        if {$pop_up == 1} {
          $win.frame.canvas bind $box_name <ButtonPress> "v_trace_p_view $win [lindex $x 6] $model"
        } elseif {$pop_up == 2 && [lindex $x 7] != "nil"} {
          $win.frame.canvas bind $box_name <ButtonPress> "v_trace_dm_view $win [lindex $x 7] $model"
        }

                 
          if {[lindex $x 2] == [lindex $x 4]} { 
            if {[lindex $x 7] != "nil"} {
              $win.frame.canvas create text [expr 40 + [lindex $x 1]] [lindex $x 4] -text [lindex $x 7] -anchor nw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
            }
          } else {
            if {[lindex $x 6] != "nil"} {
              $win.frame.canvas create text [expr 40 + [lindex $x 1]] [lindex $x 2] -text [lindex $x 6] -anchor nw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
            }
            if {[lindex $x 7] != "nil"} {
              $win.frame.canvas create text [expr 40 + [lindex $x 1]] [lindex $x 4] -text [lindex $x 7] -anchor sw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
            }
          }
        }
      }
    }

    set size [$win.frame.canvas cget -height]
    set y 0
    set x [$win.frame.canvas cget -width]

    while {$y <= $size} {
      $win.frame.canvas create line 40 $y $x $y -width 1 -f gray -tag [list trace_items scale_items time_line]
      $win.frame.canvas create text 0 $y -text [format "%.3f" [expr 0.001 * $y]] -anchor w -font graphic_trace_font -tag [list trace_items scale_items]
      incr y 50
    }

    $win.frame.canvas lower time_line display_boxes
  } else {

# user has restricted the size...
 
    # adjust for input in seconds...
    set min_y [expr int( floor ($min_y * 1000))]
    set max_y [expr int( floor ($max_y * 1000))]

    $win.frame.canvas configure -height [expr $max_y - $min_y + 5]
    $win.frame.canvas configure -scrollregion "0 -4 1000 [expr 1 + $max_y - $min_y]"

 foreach x $result {
  
      switch [lindex $x 0] {
        label { 
          $win.frame.canvas create text [expr 40 + [lindex $x 2]] -60 -anchor n -font text_font -text [lindex $x 1] -width [lindex $x 4]  -fill [lindex $x 3] -tag trace_items
          $win.canvas1 create text [expr 40 + [lindex $x 2]] 5 -anchor n -font text_font -text [lindex $x 1] -width [lindex $x 4] -tag label_tag -fill [lindex $x 3]
          if {[lindex $x 1] == "production"} {
            set pop_up 1
          } elseif {[lindex $x 1] == "retrieval"} {
            set pop_up 2
          } else {
            set pop_up 0
          }
        }
     
        rectangle {
          
          set y1 [lindex $x 2]
          set y2 [lindex $x 4]

          if {$y1 < $max_y && $y2 > $min_y} {

            if {$y1 < $min_y} {
              set y1 0
            } else {
              set y1 [expr $y1 - $min_y]
            }

            if {$y2 > $max_y} {
              set y2 [expr $max_y - $min_y]
            } else {
              set y2 [expr $y2 - $min_y]
            }
       

            set box_name [new_variable_name box]

            $win.frame.canvas create rectangle [expr 40 + [lindex $x 1]] $y1 [expr 40 + [lindex $x 3]] $y2 -width 1 -fill [lindex $x 5] -outline black -tag [list trace_items $box_name scale_items display_boxes]

            if {[expr 40 + [lindex $x 3]] > [$win.frame.canvas cget -width]} {
              $win.frame.canvas configure -width [expr 40 + [lindex $x 3]]
            }

          if {[lindex $x 8] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar \"[lindex $x 8]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } elseif {[lindex $x 7] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar \"[lindex $x 7]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } elseif {[lindex $x 6] != "nil"} {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\" 
                                                      set $win.notesvar \"[lindex $x 6]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\" 
                                                      set $win.notesvar \"\""
          } else {
            $win.frame.canvas bind $box_name <Enter> "set $win.textvar \"[format "%.3f" [expr 0.001 * ([lindex $x 4] - [lindex $x 2])]]: [format "%.3f" [expr 0.001 * [lindex $x 2]]] - [format "%.3f" [expr 0.001 * [lindex $x 4]]]\""
            $win.frame.canvas bind $box_name <Leave> "set $win.textvar \"\""
          }
                 
          if {$pop_up == 1} {
            $win.frame.canvas bind $box_name <ButtonPress> "v_trace_p_view $win [lindex $x 6] $model"
          } elseif {$pop_up == 2 && [lindex $x 7] != "nil"} {
            $win.frame.canvas bind $box_name <ButtonPress> "v_trace_dm_view $win [lindex $x 7] $model"
          }



            if {[lindex $x 2] == [lindex $x 4]} { 
              if {[lindex $x 7] != "nil"} {
                $win.frame.canvas create text [expr 40 + [lindex $x 1]] $y2 -text [lindex $x 7] -anchor nw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
              }
            } else {
              if {[lindex $x 6] != "nil"} {
                $win.frame.canvas create text [expr 40 + [lindex $x 1]] $y1 -text [lindex $x 6] -anchor nw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
              }
              if {[lindex $x 7] != "nil"} {
                $win.frame.canvas create text [expr 40 + [lindex $x 1]] $y2 -text [lindex $x 7] -anchor sw -font graphic_trace_font -tag [list trace_items trace_text $box_name scale_items] -width [expr [lindex $x 3] - [lindex $x 1]]
              }
            }
          }
        }
      }
    }

    set y $min_y
    set x [$win.frame.canvas cget -width]

    while {$y <= $max_y} {
      $win.frame.canvas create line 40 [expr $y - $min_y] $x [expr $y - $min_y] -width 1 -f gray -tag [list trace_items scale_items time_line]
      $win.frame.canvas create text 0 [expr $y - $min_y] -text [format "%.3f" [expr 0.001 * $y]] -anchor w -font graphic_trace_font -tag [list trace_items scale_items]
      incr y 50
    }

    $win.frame.canvas lower time_line display_boxes

  }
}

proc v_trace_dm_view {win chunk model} {
 
  global $win.dm_viewer

  upvar $win.dm_viewer viewer

  if {$viewer == 0 || [winfo exists $viewer] != 1} {

    if {$model == "nil" || [set_currently_selected_model $model] != 0} {
    
      set win [make_declarative_viewer]

      set box "$win.list_frame.list_box"

      global  $win.list_frame.list_box.var
      wait_for_non_null $win.list_frame.list_box.var

      set index [lsearch -exact [$box get 0 end] $chunk] 

      $box selection set $index

      event generate $box <<ListboxSelect>>

      set viewer $win
    } else {
      tk_messageBox -icon warning -title "No Model" \
                    -message "Model $model is not currently defined so declarative viewer unavailable" -type ok
    }

  } else {
    
    wm deiconify $viewer
    raise $viewer

    set box "$viewer.list_frame.list_box"

    set index [lsearch -exact [$box get 0 end] $chunk] 

    $box selection clear 0 end

    event generate $box <<ListboxSelect>>

    $box selection set $index

    event generate $box <<ListboxSelect>>
  }
}

proc v_trace_p_view {win prod model} {
 
  global $win.p_viewer

  upvar $win.p_viewer viewer

  if {$viewer == 0 || [winfo exists $viewer] != 1} {

    if {$model == "nil" || [set_currently_selected_model $model] != 0} {
      set win [make_procedural_viewer]

      set box "$win.list_frame.list_box"

      global  $win.list_frame.list_box.var
      wait_for_non_null $win.list_frame.list_box.var

      set index [lsearch -exact [$box get 0 end] $prod] 

      $box selection set $index

      event generate $box <<ListboxSelect>>

      set viewer $win
    } else {
      tk_messageBox -icon warning -title "No Model" \
                    -message "Model $model is not currently defined so procedural viewer unavailable" -type ok
    }


  } else {
    
    wm deiconify $viewer
    raise $viewer

    set box "$viewer.list_frame.list_box"

    set index [lsearch -exact [$box get 0 end] $prod] 

    $box selection clear 0 end

    event generate $box <<ListboxSelect>>

    $box selection set $index

    event generate $box <<ListboxSelect>>
  }
}

proc save_v_graphic_trace_data {win} {
  set fname [tk_getSaveFile -title "Save graphic trace data" -filetypes {{"All files" "*.*"}}]

  if {$fname != ""} {  
    upvar $win.return return
    write_data $return $fname
  }
}

proc read_v_graphic_trace_data {win} {

  set fname [tk_getOpenFile -title "Load graphic trace data" -filetypes {{"All files" "*.*"}}]

  if {$fname != ""} {  

    set fileid [open $fname "r"]
    upvar $win.return return
    set return [read $fileid]  
    close $fileid
    
    $win.canvas1 delete label_tag
    display_vert_data $win 2 "Loaded from File"

  }
}


proc save_vert_graphic_trace {win} {
  set fname [tk_getSaveFile -title "Save graphic trace as" -filetypes {{"Encapsulated PostScript" "*.eps"}}]

  if {$fname != ""} {  
   # $win.frame.canvas postscript -file $fname -width [$win.frame.canvas cget -width] -height [expr 60 + [$win.frame.canvas cget -height]] -x 0 -y -60 -pageanchor nw -pagex 0.0 -pagey [expr 60 + [$win.frame.canvas cget -height]]  -pageheight [expr 60 + [$win.frame.canvas cget -height]] 
      $win.frame.canvas postscript -file $fname -width [$win.frame.canvas cget -width] -height [expr 60 + [$win.frame.canvas cget -height]] -x 0 -y -60 -pageanchor nw -pagex 0.0 -pagey [expr 60 + [$win.frame.canvas cget -height]]  -pagewidth [$win.frame.canvas cget -width] 

  }
}

proc save_vert_graphic_trace_multi {win} {
  set fname [tk_getSaveFile -title "Save graphic trace as" -filetypes {{"PostScript" "*.ps"}}]

  if {$fname != ""} {  
   # $win.frame.canvas postscript -file $fname -width [$win.frame.canvas cget -width] -height [expr 60 + [$win.frame.canvas cget -height]] -x 0 -y -60 -pageanchor nw -pagex 0.0 -pagey [expr 60 + [$win.frame.canvas cget -height]]  -pageheight [expr 60 + [$win.frame.canvas cget -height]] 
   
   set scale [expr ceil([$win.frame.canvas cget -width] / 8)]
   set width [expr ceil($scale * 8)]
   set height [expr $scale * 10]

   

   set yMax [expr 60 + [$win.frame.canvas cget -height]]
   set NOP [expr ceil ([expr (60.0 + [$win.frame.canvas cget -height]) / $height])]   
  
# The following code was modified from code written by Robert Heller
# in a file called bridge.tcl which was posted to comp.lang.tcl as
# an example of producing multi-page ps files.

   set prFile [open $fname w]

  puts $prFile "%!PS-Adobe-2.0"
  puts $prFile "%%Creator: ACT-R Environment Copyright 2007 Dan Bothell"
  puts $prFile "%%Title: Vertical Graphic Trace"
  puts -nonewline $prFile "%%CreationDate: "
  global tcl_version
  if {$tcl_version >= 7.6} {
    puts $prFile "[clock format [clock seconds]]"
  } else {
    puts $prFile "[exec date]"
  }
  puts $prFile "%%Pages: $NOP"
  puts $prFile "%%EndComments"
  puts $prFile "/EncapDict 200 dict def EncapDict begin"
  puts $prFile "/showpage {} def /erasepage {} def /copypage {} def end"
  puts $prFile "/BeginInclude {0 setgray 0 setlinecap 1 setlinewidth"
  puts $prFile "0 setlinejoin 10 setmiterlimit \[\] 0 setdash"
  puts $prFile "/languagelevel where {"
  puts $prFile "  pop"
  puts $prFile "  languagelevel 2 ge {"
  puts $prFile "    false setoverprint"
  puts $prFile "    false setstrokeadjust"
  puts $prFile "  } if"
  puts $prFile "} if"
  puts $prFile "newpath"
  puts $prFile "save EncapDict begin} def"
  puts $prFile "/EndInclude {restore end} def"
  puts $prFile "%%EndProlog"
  set pageNo 1


  for {set yoff 0} {$yoff < $yMax} {set yoff [expr $yoff + $height]} {

      puts $prFile "%%Page: $pageNo $pageNo"
      puts $prFile "BeginInclude"

      set eps "[$win.frame.canvas postscript -height $height -width $width -x 0 -y [expr $yoff - 60] -pageanchor nw -pagex 0.25i -pagey 10.5i -pagewidth 8.0i]"

      set EOC [string first "%%BeginProlog\n" "$eps"]
      set EOF [expr [string first "%%EOF\n" "$eps"] - 1]

      puts $prFile "[vert_StripPSComments [string range $eps $EOC $EOF]]"
      puts $prFile "EndInclude showpage"
      incr pageNo
  }
  puts $prFile "%%EOF"
  close $prFile
  }
}

proc vert_StripPSComments {PSString} {
  set result {}
  foreach l [split "$PSString" "\n"] {
    set i [string first "%" "$l$"]
    if {$i == 0} {
      set result "$result\n"
    } elseif {$i > 0 && [regexp {(^.*[^\\])(%.*$)} "$l" whole prefix comment]} {
      set result "$result$prefix\n"
    } else {
      set result "$result$l\n"
    }
  }
  return "$result"
}
