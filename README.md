
<!-- README.md is generated from README.Rmd. Please edit that file -->
rstansim: An R package for running simulation studies using stan
================================================================

[![Travis Build Status](https://travis-ci.org/Ewan-Keith/rstansim.svg?branch=master)](https://travis-ci.org/Ewan-Keith/rstansim) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Ewan-Keith/rstansim?branch=master&svg=true)](https://ci.appveyor.com/project/Ewan-Keith/rstansim) [![codecov](https://codecov.io/gh/Ewan-Keith/rstansim/branch/master/graph/badge.svg)](https://codecov.io/gh/Ewan-Keith/rstansim)

Overview
--------

rstansim provides a set of helper and utility functions to simplify running simulation studies using R and [stan](http://mc-stan.org/). The package addresses three aspects of running a simulation study:

-   [Data simulation](simulating_data.html).
-   Model fitting and capture of relevant data.
-   Management of simulation results.

All simulation data is output in a [tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) format, for ease of interaction with [tidyverse](https://www.tidyverse.org/) packages for analysis and visualisation. Additionally reproducability information such as seeds, initial values, and data used, are stored alongside the data.

Installation
------------

``` r
# Package not yet on CRAN, needs installed from Github:
# install.packages("devtools")
devtools::install_github("ewan-keith/rstansim")
```

Usage
-----

The four vignettes below provide exmples of how rstansim can be used to simplify simulation studies with stan. The first vignette provides a minimal, end to end example of a simualtion study and it is recommended that this is read first. The next three provide greater detail on the three primary aspects of running a simulation study listed above.

-   Using rstansim to run a simulation study
-   Simulating datasets using rstan and rstansim
-   Model fitting and capture of relevant data
-   Managing simulation results with rstansim

If looking to produce reproducable samples with stans own RNG functions then [read this note](reproducability.html) on the behaviour of these functions.

Documentation
-------------

Further information on the packages functions can be found at [its site reference](https://ewan-keith.github.io/rstansim/reference/index.html).
