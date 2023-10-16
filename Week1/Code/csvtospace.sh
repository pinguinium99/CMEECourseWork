#!/bin/sh
# Author: Your name p.thazhakaden23@imperial.ac.uk
# Script: csvtospace.sh
# Description: substitute the commas in the files with tabs
#
# Saves the output into a .tsv file
# Arguments: 1 -> comma separated values
# Date: Oct 2023

if [[ $1 == "" ]] || [[ $1 != *.csv ]]
then
    echo "Wrong file type, input .csv file"
else

    echo "Creating a tab delimited version of $1 ..."
    cat $1 | tr -s "," "\t" >> $(basename "$1" | cut -d. -f1).tsv
    echo "Done!"
    exit

fi
