[![Build Status](https://travis-ci.org/jefferis/paperutils.png?branch=master)](https://travis-ci.org/jefferis/paperutils)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
# paperutils

R package with utility functions to support preparation of journal articles

## Introduction
We use a toolchain consisting of

1. LyX + pdflatex
2. Bibdesk (or Jabref etc)
3. Adobe Illustrator
4. R

to prepare our journal articles for submission. This package contains functions that are useful for checking which files are currently linked in the master LyX document (e.g. to ensure they are gitified). It also contains functions to work with the output from pdflatex

## Installation

### System Dependencies
  * [pdftk](http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
  * ghostscript e.g. from [mactex](http://tug.org/mactex)
  * perl

### R Dependencies
The package [scholar](https://github.com/jkeirstead/scholar) is necessary for 
some bibliograpy related functions. 
However currently the CRAN version is 
misssing the ability to specify a number of publications to return for each
author, so that only 20 can be returned. Therefore the package installs the 
latest version from GitHub.

### Package Install
Install straight from github with Hadley Wickham's 
[devtools](https://github.com/hadley/devtools) package:

```r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/paperutils")
```

