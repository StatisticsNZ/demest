
library(dplyr)
library(demest)
y <- demdata::uk.deaths %>%
    Counts(dimscales = c(year = "Intervals")) %>%
    subarray(year > 1950) %>%
    collapseIntervals(dimension = "age", breaks = seq(0, 90, 5))
expose <- demdata::uk.exposure %>%
    Counts(dimscales = c(year = "Intervals")) %>%
    subarray(year > 1950) %>%
    collapseIntervals(dimension = "age", breaks = seq(0, 90, 5))
filename <- tempfile()

estimateModel(Model(y ~ CMP(mean ~ age + sex + year,
                            dispersion = Dispersion(mean = Norm(mean = 0, sd = 0.1),
                                                    scale = HalfT(scale = 0.1))),
                    age ~ DLM(damp = NULL),
                    jump = 0.02),
              y = y,
              exposure = expose,
              filename = filename,
              nBurnin = 10000,
              nSim = 10000,
              nChain = 4,
              nThin = 25)
fetchSummary(filename)




