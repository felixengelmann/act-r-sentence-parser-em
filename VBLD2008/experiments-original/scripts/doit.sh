#!/bin/sh

#for f in trace.*; do tr '\r' '\n' < $f > tmp && mv tmp $f; done

./att4plotting.awk cond=a trace.a > a.txt
./att4plotting.awk cond=b trace.b > b.txt
./att4plotting.awk cond=c trace.c > c.txt
./att4plotting.awk cond=d trace.d > d.txt
./att4plotting.awk cond=e trace.e > e.txt
./att4plotting.awk cond=f trace.f > f.txt


egrep -B8 .+GL.+LICH a.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > a.completed  
egrep -B8 .+GL.+LICH b.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > b.completed 
egrep -B8 .+GL.+LICH c.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > c.completed 
egrep -B8 .+GL.+LICH d.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > d.completed 
egrep -B8 .+GL.+LICH e.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > e.completed 
egrep -B8 .+GL.+LICH f.txt | awk '{if ($0 ~ /^--$/) ;else print $0}' > f.completed

