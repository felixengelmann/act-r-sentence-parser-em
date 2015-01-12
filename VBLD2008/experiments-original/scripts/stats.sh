#!/bin/sh

# example: for f in trace*; do ./stats.sh $f done

#tr '\r' '\n' < $1 > tmp.$$ && mv tmp.$$ $1

echo -e "\n\n$1"

awk '
BEGIN {c=0; m=0} 
{
	if ($0 ~ /.*SENTENCE:.+/) {
		sentence= $0
		pi      = toupper($9)
	} 
	###else if ($0 ~ /.*TOTAL.+GL.+CKLICH.*/) c++
	else if ($0 ~ /.*TOTAL.+(JEMALS|DURCHAUS).*/) {m=m+$6;c++} 
} 
END {mean=m/c; printf "\n%s\n\nMean Attachment time of %s: %s\n\n", sentence, pi, mean}' $1

#egrep .+Try-Acc.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
#echo "try-acc-DP-rather-than-nom-DP"

#egrep .+Successful.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
#echo "successful-retrieval-of-positive-matrix-DP-at-NPI-and-attach"


egrep .+Attach-Npi.+nom.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPnom"
egrep .+Attach-Npi.+acc.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-npi-adv-as-modifier-of-predicted-predicate-AdjP-retrieved-DPacc"
egrep .+Attach-Ppi.+nom.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPnom"
egrep .+Attach-Ppi.+acc.+Fired.*  $1 | wc | awk '{printf "%s ", $1}'
echo "attach-ppi-adv-as-modifier-of-predicted-predicate-AdjP-DPacc"


#egrep .+Fail.+Fired.*        $1 | wc | awk '{printf "%s ", $1}'
#echo "fail-at-polarity-item-and-proceed"

#egrep '.+TOTAL.+(JEMALS|DURCHAUS).*'      $1 | wc | awk '{printf "%s ", $1}'
#echo "successful attachments of NPI/PPI"

egrep .+TOTAL.+GL.+CKLICH.*   $1 | wc | awk '{printf "%s ", $1}'
echo "completed runs"
egrep 'Time  0.000: Vision found' trace.a | wc | awk '{printf "%s ", $1}'
echo "initiated runs"
egrep '.+TOTAL.+(JEMALS|DURCHAUS).*'   $1 | wc | awk '{printf "%s ", $1}'
echo "successful attachments of NPI/PPI"


