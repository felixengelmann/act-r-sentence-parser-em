#!/bin/sh


cat trace.pirat-a >> ./pirat-lf$1/ans$2/trace.pirat-a &&
cat trace.pirat-b >> ./pirat-lf$1/ans$2/trace.pirat-b &&
cat trace.pirat-c >> ./pirat-lf$1/ans$2/trace.pirat-c &&
cat trace.pirat-d >> ./pirat-lf$1/ans$2/trace.pirat-d &&
cat trace.pirat-e >> ./pirat-lf$1/ans$2/trace.pirat-e &&
cat trace.pirat-f >> ./pirat-lf$1/ans$2/trace.pirat-f &&
rm trace.*
