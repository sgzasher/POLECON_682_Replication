#!/bin/bash

# Change Directory to Relevant 
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR
cd ../data/raw/gov_finances

# Get all the "A" files with the relevant variables
list_files=$(ls | grep 'a.Txt')

# Verifying the columns are correct - see also "UserGuide.xls"
# Total expenditure, population, type code, state & county FIPS, Name, Year4
varcols=(3 6 7 8 9 25 147) 
for i in ${varcols[@]}; do 
	awk -F "," -v i="$i" 'NR==1{print $i; exit}' $list_files[1]
done

# Main output file; give it the correct column names
touch ../gov_finances_relevant.txt
cat $list_files[1] | awk -F "," -v OFS="," 'NR==1{print $3, $6, $7, $8, $9, $25, $147; exit}' > ../gov_finances_relevant.txt

# For each file, print relevant columns to a temp file
# then concatenate that temp file to a main file 
# With a sleep because my computer writes so slow :( 

for file in ${list_files[@]}; do
	sleep .5
	touch Temp.txt
	cat $file | awk -F "," -v OFS="," 'NR!=1{print $3, $6, $7, $8, $9, $25, $147}' > Temp.txt
	cat "Temp.txt" >> ../gov_finances_relevant.txt
	rm Temp.txt
done

