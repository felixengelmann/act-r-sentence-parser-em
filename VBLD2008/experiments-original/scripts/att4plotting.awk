#!/usr/bin/awk -f 

BEGIN {
	run  =0
}
{
if ($0 ~/.+TOTAL.+/) {

	word=$5
	sub(/:/, "", word)
	
	if (word ~ /K?EIN$/)           { wpos=1; run++ }
	if (word ~ /MANN/)            wpos=2
	if (word ~ /DER/)             wpos=3
	if (word ~ /K?EINEN/)         wpos=4
	if (word ~ /BART/)            wpos=5
	if (word ~ /HATTE/)           wpos=6
	if (word ~ /WAR/)             wpos=7
	if (word ~ /DURCHAUS|JEMALS/) wpos=8
	if (word ~ /GL.+CKLICH/)      wpos=9

		
		if (word ~ /GL.+CKLICH/) {
			printf "%s\t%s\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}
		else if ($5 ~ /DURCHAUS/) {
			printf "%s\t%s\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}
		else {
			printf "%s\t%s\t\t%s\t%s\t%s\n", run, word, wpos, cond, $6
		}

}
}

                                                                             
