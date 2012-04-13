#!/bin/sh
Rscript -e "require(knitr); knit('hw1.Rnw')"
pdflatex hw1.tex
rm *out
rm *aux
rm *div
rm *out
evince hw1.pdf &
