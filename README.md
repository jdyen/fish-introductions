### Overview
This repository contains code and supporting information for Maceda-Veiga et al. (submitted), Patterns in abundance and individual size distributions of riverine native fish assemblages experiencing exotic- and native-fish invasions.

Last updated: 06 April 2022

### Abstract
Understanding how species introductions, which are an outstanding perturbation in many biological systems, affect the structure of local assemblages is crucial for biodiversity management. Introduced species can be from locations sharing much evolutionary history with local assemblages (‘native invaders’) or from regions very different from the local areas (‘exotic invaders’). Here, we explored whether the relative effects differ between species introductions having two broadly different evolutionary histories on local stream assemblages in 15 Spanish catchments (99,700 km2). We contrasted the effects of introduced fish from catchments within Spain (i.e. native invaders) with those of species arising from catchments outside Spain (i.e. exotic invaders). We measured local species responses by means of: (1) species richness; (2) abundance; and (3) individual size-distributions irrespective of species identity (ISDs). We used hierarchical Bayesian models to relate richness, abundance and the ISDs in native-species assemblages to the presence, abundance, and weighted trophic level of native and exotic invaders, alongside potential environmental predictors. Environmental predictors dominated the fraction of explained variance (≥ 65%) for all responses. Native invaders accounted for more of the explained variance than exotic invaders for ISDs and abundance, but not for richness. Our study suggests that native invaders sometimes may be as problematic as exotic invaders at a time that angling and water transfers among catchments to deal with climate change may increase the presence of native invaders in streams.


### Analysis details

Analyses require the `greta` and `greta.fda` packages. The `greta` package can be installed from CRAN (`install.packages("greta")`, following all prompts to install dependencies) and the `greta.fda` package can be installed directly from GitHub using the `remotes` package:
``` r
remotes::install_github("jdyen/greta.fda")
```
See Yen et al. (2015, MEE) and https://github.com/jdyen/greta.fda for details of functional data analysis methods.

All analysis scripts are in the code directory, numbered in order of their use. Helper functions are included in the remaining files and are sourced when required. Analyses are not currently reproducible from this code because data and fitted model objects are not provided. Summary scripts require fitted model objects, not currently uploaded due to large file sizes. Data will be uploaded following publication of the above-listed manuscript (Maceda-Veiga et al. submitted).
