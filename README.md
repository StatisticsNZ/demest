
# demest

Bayesian demographic estimation and forecasting. 

`demest` provides tools for making inferences about cross-classified rates, probabilities, means, and counts, using Bayesian methods.  The software and methods are still under development.

The main functions are:

* `estimateModel`: estimate rates, probabilities, or means from a single reliable dataset.

* `estimateCounts`: estimate counts, and the associated rates or probabilities, from multiple noisy datasets.

* `estimateCounts`: estimate a demographic account, and the associated rates or probabilities, from multiple noisy datasets.  `estimateAccount` is not finished yet.

`demest` uses functions and data structures from package `dembase`.

Install `demest` from github with:
```{r, echo = FALSE}
devtools::install_github("StatisticsNZ/demest")
```

The package is Crown copyright (c) 2016, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under a MIT License (see LICENSE file).
