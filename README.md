
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
library(devtools)
install_github("StatisticsNZ/demest")
```

---
__Copyright and Licensing__

The package is Crown copyright (c) 2016, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the MIT License (see LICENSE file).

<br /><a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This document is Crown copyright (c) 2016, Statistics New Zealand on behalf of the New Zealand Government, and is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
