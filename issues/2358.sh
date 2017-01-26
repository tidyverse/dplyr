#!/bin/sh

git co .
rm src/*.d
R -e 'rmarkdown::render("bindr-2.Rmd")'
git co .
