#!/bin/bash

# Author: Your name you p.thazhakaden23@imperial.ac.uk
# Script: tabtocsv.sh
# Description: counts lines in file
#
# Arguments: 1 -> file to count
# Date: Oct 2023


NumLines=`wc -l < $1`
echo "The file $1 has $NumLines lines"
echo 