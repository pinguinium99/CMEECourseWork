#!/bin/bash

# Author: Your name p.thazhakaden23@imperial.ac.uk
# Script: ConcatenateTwoFiles.sh
# Description: combines files of the same type
#
# Saves the output into a single file
# Arguments: 1 -> input_file_1 input_file_2 output_file
# Date: Oct 2023

if [[ $1 = "" ]] || [[ $2 = "" ]] || [[ $3 = "" ]]
then
    echo "Files missing, check input files and output file"      
elif [[ "${1: -4}" == "${2: -4}" ]]
then
    cat $1 > $3
    cat $2 >> $3
    echo "Merged File is in" $3
    echo "Merged File is" 
    cat $3
else
    
    echo "Warning! diffrent file types"
    
fi