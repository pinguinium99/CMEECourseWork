#!/bin/bash

R_SCRIPT="Miniproject.R"
texfile="miniproject.tex"

Rscript "$R_SCRIPT"



texfile=$(basename "$1" | cut -d. -f1)
pdflatex $texfile.tex
bibtex $texfile
pdflatex -shell-escape $texfile.tex
pdflatex -shell-escape $texfile.tex
evince $texfile.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg