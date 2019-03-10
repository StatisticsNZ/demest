
## library(dplyr)
## library(demest)
## y <- demdata::uk.deaths %>%
##     Counts(dimscales = c(year = "Intervals")) %>%
##     subarray(year > 1950) %>%
##     collapseIntervals(dimension = "age", breaks = seq(0, 90, 5))
## expose <- demdata::uk.exposure %>%
##     Counts(dimscales = c(year = "Intervals")) %>%
##     subarray(year > 1950) %>%
##     collapseIntervals(dimension = "age", breaks = seq(0, 90, 5))
## filename <- tempfile()

## estimateModel(Model(y ~ CMP(mean ~ age + sex + year,
##                             dispersion = Dispersion(mean = -1, sd = 0.2)),
##                     age ~ DLM(damp = NULL),
##                     jump = 0.02),
##               y = y,
##               exposure = expose,
##               filename = filename,
##               nBurnin = 10,
##               nSim = 10,
##               nChain = 4,
##               nThin = 1)
## fetchSummary(filename)




