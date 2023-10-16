#!/bin/bash

texfile=$(basename "$1" | cut -d. -f1)
pdflatex $texfile.tex
bibtex $texfile
pdflatex $texfile.tex
pdflatex $texfile.tex
evince $texfile.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg