#!/bin/sh

## Uses att4plotting-pirat.awk to extract total attachment times from pirat traces
## and writes them to {a-f}.txt
## Additionally, {a-f}.completed are created from only complete parse runs.

#for f in trace.*; do tr '\r' '\n' < $f > tmp && mv tmp $f; done

#./att4plotting.awk cond=a trace.a > a.txt
#./att4plotting.awk cond=b trace.b > b.txt
#./att4plotting.awk cond=c trace.c > c.txt
#./att4plotting.awk cond=d trace.d > d.txt
#./att4plotting.awk cond=e trace.e > e.txt
#./att4plotting.awk cond=f trace.f > f.txt
./att4plotting-pirat.awk cond=a trace.pirat-a > a.txt
./att4plotting-pirat.awk cond=b trace.pirat-b > b.txt
./att4plotting-pirat.awk cond=c trace.pirat-c > c.txt
./att4plotting-pirat.awk cond=d trace.pirat-d > d.txt
./att4plotting-pirat.awk cond=e trace.pirat-e > e.txt
./att4plotting-pirat.awk cond=f trace.pirat-f > f.txt


egrep -B9 .+SPARSAM a.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > a.completed  
egrep -B9 .+SPARSAM b.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > b.completed 
egrep -B9 .+SPARSAM c.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > c.completed 
egrep -B9 .+SPARSAM d.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > d.completed 
egrep -B9 .+SPARSAM e.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > e.completed 
egrep -B9 .+SPARSAM f.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > f.completed

