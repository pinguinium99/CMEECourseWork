#!/bin/bash

# Author: Your name p.thazhakaden23@imperial.ac.uk
# Script: tiff2png.sh
# Description: converts .tif files to .png
#
# Saves the output into code directory as a .png file
# Arguments: 1 -> .tif file
# Date: Oct 2023

if [[ $1 = "" ]] 
then
      echo "input files missing"

else 

      for f in *.tif;
            do
                  echo "Converting $1";
                  convert "$1" "$(basename "$1" .tif).png";
                  echo "png in Code directory"
            done

fi