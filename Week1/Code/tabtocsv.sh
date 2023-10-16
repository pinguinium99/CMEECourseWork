#!/bin/sh
# Author: Your name p.thazhakaden23@imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2023

if [[ $1 == "" ]] || [[ $1 != *.txt ]]
then
    echo "Wrong file type, input .txt file "
else
    echo "Creating a comma delimited version of $1 ..."
    cat $1 | tr -s "\t" "," >> $(basename "$1" | cut -d. -f1).csv
    echo "Done!"
    exit
fi
