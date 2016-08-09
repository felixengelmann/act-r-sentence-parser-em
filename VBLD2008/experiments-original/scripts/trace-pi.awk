#!/usr/bin/awk -f 

BEGIN {
}

{
	if ($0 ~ /.*SENTENCE:.+/) printf "%s\n\n", $0 
	else if  ($0 ~ /.*NOW READING: \"(jemals|durchaus)\" at.*/) { 
		
		print $0
		
		do {
			(getline tmp)
			print tmp 
		}	
		while (tmp !~ /.*TOTAL attachment time for (JEMALS|DURCHAUS).*/)
		
		(getline end)
		printf "%s\n\n",  end
	}

}

END {
}
                                                                             
