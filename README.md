
<!-- README.md is generated from README.Rmd. Please edit that file -->
rstansim
========

[![Travis Build Status](https://travis-ci.org/Ewan-Keith/rstansim.svg?branch=master)](https://travis-ci.org/Ewan-Keith/rstansim) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Ewan-Keith/rstansim?branch=master&svg=true)](https://ci.appveyor.com/project/Ewan-Keith/rstansim) [![codecov](https://codecov.io/gh/Ewan-Keith/rstansim/branch/master/graph/badge.svg)](https://codecov.io/gh/Ewan-Keith/rstansim)

Overview
--------

*PACKAGE STILL IN DEVELOPMENT AND UNLIKELY TO WORK FOR YOU. SORRY*

`rstansim` provides a simple way to fit a single [stan](http://mc-stan.org/) model to multiple datasets and to extract estimates from each. Making it easy to run simulation studies of Bayesian models using stan in R. When ran in parallel each model is single threaded, with models estimated concurrently, taking full advantage of computational resources, even when the number of available cores is much greater than the number of chains ran per model.

Installation
------------

``` r
# Package not yet on CRAN, needs installed from Github:
# install.packages("devtools")
devtools::install_github("ewan-keith/rstansim")
```

Usage
-----

`rstansim` provides a single function `stan_sim()` that fits the specified model to all provided datasets, and returns a set of requested model estimates.
