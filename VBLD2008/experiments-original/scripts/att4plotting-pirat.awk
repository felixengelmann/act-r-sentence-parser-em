#!/usr/bin/awk -f 

## Used by: doit-pirat.sh
## Extracts the "TOTAL attachment time" line from the traces 
##  and prints a table with value and metadata.


BEGIN {
	run  =0
}
{
if ($0 ~/.+TOTAL.+/) {

	word=$5
	sub(/:/, "", word)
	
	if (word ~ /K?EIN$/)        { wpos=1; run++ }
	if (word ~ /PIRAT/)           wpos=2
	if (word ~ /DER/)             wpos=3
	if (word ~ /K?EINEN/)         wpos=4
	if (word ~ /BRATEN/)          wpos=5
	if (word ~ /GEGESSEN/)        wpos=6
	if (word ~ /HATTE/)           wpos=7
	if (word ~ /WAR/)             wpos=8
	if (word ~ /DURCHAUS|JEMALS/) wpos=9
	if (word ~ /SPARSAM/)         wpos=10

		
		if (word ~ /GL.+CKLICH/) {
			printf "%s\t%s\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}
		else if ($5 ~ /DURCHAUS/) {
			printf "%s\t%s\t%s\t%s\t%s\n", run, word, wpos, cond, $6
                }

		else if ($5 ~ /GEGESSEN/) {
			printf "%s\t%s\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}
		else {
			printf "%s\t%s\t\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}

}
}

                                                                             
