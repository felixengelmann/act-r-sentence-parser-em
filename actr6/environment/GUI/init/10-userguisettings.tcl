# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1920 || [winfo screenheight .] != 1080 || [lindex [wm maxsize .] 0] != 3200 || [lindex [wm maxsize .] 1] != 1035} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.declarative_tree) 1920x1031+0+23
  set changed_window_list(.declarative_tree) 1
  set window_config(.control_panel) 198x864+1082+131
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 420x300+750+390
  set changed_window_list(.declarative) 1
  set window_config(.copyright) 400x466+760+307
  set changed_window_list(.copyright) 1
}
