<!-- README.md is generated from README.Rmd. Please edit that file -->
shapAAR
=======

[![Travis-CI Build Status](https://travis-ci.org/MartinHinz/shapAAR.svg?branch=master)](https://travis-ci.org/MartinHinz/shapAAR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/shapAAR)](https://cran.r-project.org/package=shapAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/MartinHinz/shapAAR/master.svg)](https://codecov.io/github/MartinHinz/shapAAR?branch=master)

This is an R package for the extraction, analysis and classification of (not only) archaeological objects derived from scanned images. Especially it aims at the analysis of the shapes/profiles of eg. ceramic vessels or arrow heads.

Currently the extraction is implemented using [active contour](https://en.wikipedia.org/wiki/Active_contour_model) to identify and extract the shape informations. You can get an idea about its abilities in a [vignette](vignettes/object-extraction.md).

Licence
-------

`shapAAR` is released under the [GNU General Public Licence, version 3](https://www.r-project.org/Licenses/LGPL-3). Comments and feedback are welcome, as are code contributions.

Installation
------------

`shapAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/shapAAR')
