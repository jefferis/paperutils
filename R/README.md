# pdfutils

R package with utility functions to support preparation of journal articles

## Introduction
We use a toolchain consisting of

1. LyX + pdflatex
2. Bibdesk (or Jabref etc)
3. Adobe Illustrator
4. R

to prepare our journal articles for submission. This package contains functions that are useful for checking which files are currently linked in the master LyX document (e.g. to ensure they are gitified). It also contains functions to work with the output from pdflatex

## Installation
### Dependencies
  * [pdftk](http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
  * ghostscript e.g. from [mactex](http://tug.org/mactex)
  * perl

### Released versions
    install.packages("pdfutils",repos='http://jefferislab.org/R',type='source')
### Bleeding Edge
Straight from github with Hadley Wickham's [devtools](https://github.com/hadley/devtools) package:

    install.packages("devtools")
    library(devtools)
    install_github('pdfutils','jefferis')
