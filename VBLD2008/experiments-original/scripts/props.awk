#!/usr/bin/awk -f 

# USAGE: for f in trace.*; do ./props.awk $f; done

BEGIN {
run =0
cond=ARGV[1]
}

{ 
if (cond=="trace.a") cond = "a"
if (cond=="trace.b") cond = "b"
if (cond=="trace.c") cond = "c"
if (cond=="trace.d") cond = "d"
if (cond=="trace.e") cond = "e"
if (cond=="trace.f") cond = "f"
}


/.*Attach-Npi-Adv.+nom.*Fired.*/ {run++; printf "%s\t%s\tnom\n", run, cond} 
/.*Attach-Npi-Adv.+acc.*Fired.*/ {run++; printf "%s\t%s\tacc\n", run, cond} 
/.*Attach-Ppi-Adv.+nom.*Fired.*/ {run++; printf "%s\t%s\tnom\n", run, cond}
/.*Attach-Ppi-Adv.+acc.*Fired.*/ {run++; printf "%s\t%s\tacc\n", run, cond}

