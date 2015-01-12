#!/bin/sh

# example: for f in trace*; do ./stats.sh $f; done

#tr '\r' '\n' < $1 > tmp.$$ && mv tmp.$$ $1

echo "\n\n$1"

## PROBLEM: m is INT.
awk '
BEGIN {c=0; m=0} 
{
	if ($0 ~ /.*SENTENCE:.+/) {
		sentence= $0
		pi      = toupper($10)
	} 
	###else if ($0 ~ /.*TOTAL.+GL.+CKLICH.*/) c++
	else if ($0 ~ /.*TOTAL.+(JEMALS|DURCHAUS).*/) {m=m+$6;c++} 
} 
END {printf "%f / %s \n", m, c;
	mean=m/c; printf "\n%s\n\nMean Attachment time of %s: %s\n\n", sentence, pi, mean}' $1

#egrep .+Try-Acc.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
#echo "try-acc-DP-rather-than-nom-DP"

#egrep .+Successful.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
#echo "successful-retrieval-of-positive-matrix-DP-at-NPI-and-attach"


egrep .+FIRED.+ATTACH-NPI.+NOM.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPnom"
egrep .+FIRED.+ATTACH-NPI.+ACC.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPacc"
egrep .+FIRED.+ATTACH-PPI.+NOM.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPnom"
egrep .+FIRED.+ATTACH-PPI.+ACC.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPacc"


#egrep .+Fail.+Fired.*        $1 | wc | awk '{printf "%s ", $1}'
#echo "fail-at-polarity-item-and-proceed"

#egrep '.+TOTAL.+(JEMALS|DURCHAUS).*'      $1 | wc | awk '{printf "%s ", $1}'
#echo "successful attachments of NPI/PPI"

#egrep .+TOTAL.+GL.+CKLICH.*   $1 | wc | awk '{printf "%s ", $1}'
egrep .+TOTAL.+SPARSAM.*   $1 | wc | awk '{printf "%s ", $1}'
echo "completed runs"
egrep '.+0.000.+VISION' trace.pirat-a | wc | awk '{printf "%s ", $1}'
echo "initiated runs"
egrep '.+TOTAL.+(JEMALS|DURCHAUS).*'   $1 | wc | awk '{printf "%s ", $1}'
echo "successful attachments of NPI/PPI"


