#!/bin/sh

cat trace.a >> ./traces/trace.a &&
cat trace.b >> ./traces/trace.b &&
cat trace.c >> ./traces/trace.c &&
cat trace.d >> ./traces/trace.d &&
cat trace.e >> ./traces/trace.e &&
cat trace.f >> ./traces/trace.f &&
rm trace.*
