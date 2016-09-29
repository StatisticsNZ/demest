
## Preliminaries #############################################################

library(demest)
library(latticeExtra)
library(xtable)

kBaseDir <- "/Users/johnbryant/Documents/NewZealandChina/migration"
kDataDir <- file.path(kBaseDir, "Second_Revised_Data")
kModelDir <- file.path(kBaseDir, "Second_Revised_Models")
kPaperDir <- file.path(kBaseDir, "Second_Revised_Paper")
kOutDir <- "/Users/johnbryant/Documents/Output"


    
setwd(kDataDir)
load("erp.RData")
load("covariates.RData")

kAucklandTAs <- c("Franklin",
                  "Manukau",
                  "North Shore",
                  "Papakura",
                  "Rodney",
                  "Waitakere",
                  "Auckland")

kExampleTAs <- c("Kaikoura",
                 "Masterton",
                 "Papakura",
                 "Rotorua",
                 "Manukau")


## Construct data structures to use in estimation #######################

## Exposure

setwd(kDataDir)
load("erp.RData")  ## loads array 'erp'
erp <- Counts(erp)
exposure <- subarray(erp, subarray = region != "Area Outside Territorial Authority")


## Covariates
setwd(kDataDir)
load("covariates.RData")  ## loads data.frame 'covariate.data'
covariates[-1] <- sapply(covariates[-1], scale)


## Departures

setwd(kDataDir)
load("depart.corrected.old.RData") ## loads array 'depart.corrected'
depart.old <- Counts(depart.corrected)
load("depart.corrected.new.RData") ## loads array 'depart.corrected'
depart.new <- Counts(depart.corrected)


## Concordance

ta.old <- dimnames(depart.old)$region
ta.new <- ta.old
ta.new[ta.new %in% kAucklandTAs] <- "Auckland"
conc <- data.frame(ta.old, ta.new)
conc <- Concordance(conc)

## Total departures

depart.total <- dbind(collapseDimension(depart.old, dimension = "region"),
                      collapseDimension(depart.new, dimension = "region"),
                      along = "time")
depart.total <- subarray(depart.total, iteration == 1)


## Combined departures data, with NAs for Auckland after 2010

i.reg <- match("region", names(depart.new))
dim.auckland <- c(dim(depart.new)[-i.reg], length(kAucklandTAs))
dimnames.auckland <- c(dimnames(depart.new)[-i.reg], list(region = kAucklandTAs))
auckland <- array(as.integer(NA), dim = dim.auckland, dimnames = dimnames.auckland)
auckland <- Counts(auckland)
depart <- dbind(auckland,
                subarray(depart.new, subarray = region != "Auckland"),
                along = "region")
depart <- dbind(depart, depart.old, along = "time")


## Departures from Auckland, 2011-2013

auckland1113 <- subarray(depart.new,
                         subarray = region == "Auckland" & time > 2010,
                         drop = FALSE)
auckland1113 <- collapseDimension(auckland1113, dimension = "region")


## Datasets for validation

depart.held.back <- subarray(depart, time > 2005 & time < 2010)
depart.training <- subarray(depart, time < 2005)
stopifnot(identical(names(depart.training)[c(3, 5)], c("time", "region")))
depart.training[ , , c("2003", "2004", "2005"), , kAucklandTAs] <- NA
exposure.held.back <- subarray(exposure, time > 2005 & time < 2010)
auckland0305 <- subarray(depart.old,
                         subarray = region %in% kAucklandTAs & time > 2002 & time < 2005)
auckland0305 <- collapseDimension(auckland0305, dimension = "region")
depart.02 <- subarray(depart, subarray = time == "2002")
depart.05 <- subarray(depart, subarray = time == "2005")
depart.02 <- collapseIterations(depart.02, FUN = median)
depart.05 <- collapseIterations(depart.05, FUN = median)
depart.02 <- as.data.frame(depart.02, direction = "long", responseName = "count02")
depart.05 <- as.data.frame(depart.05, direction = "long", responseName = "count05")


## 49 random TAs

set.seed(1)
kRandomTAs <- collapseDimension(exposure, mar = "region")
kRandomTAs <- sort(kRandomTAs)
kRandomTAs <- dimnames(kRandomTAs)$region
kRandomTAs <- setdiff(kRandomTAs, kExampleTAs)
kRandomTAs <- kRandomTAs[sort(sample(length(kRandomTAs), size = 49))]




## Basic model ################################################################

m <- 1
set.seed(2)
y <- subarray(depart, subarray = iteration == m)
setwd(kOutDir)
filename.est <- "experiment.est"
filename.pred <- "experiment.pred"
estimateModel(Model(y ~ Poisson(mean ~ region * age + age * sex + time),
                    age ~ Exch(),
                    jump = 0.1),
              y = y,
              exposure = exposure,
              filename = filename.est,
              nBurnin = 5,
              nSim = 5,
              nChain = 4,
              nThin = 1)
print(fetchSummary(filename.est))
pred <- predictModel(filenameEst = filename,
                     filenamePred = filename.pred,
                     n = 5)
print(fetchSummary(filename.pred))

## y <- subarray(depart, subarray = iteration == 1)
## subtotals <- subarray(auckland1113, subarray = iteration == 1)
## subtotals <- addDimension(subtotals, name = "region", labels = "Auckland")
## y <- attachSubtotals(y, subtotals = subtotals, concordances = list(region = conc))

## setwd(kOutDir)
## est.at <- estimateModel(Model(y ~ Poisson(mean ~ age * region + age * sex + age * time,
##                                           jump = 0.5),
##                               region ~ Exch(mean ~ l.pc.overseas.born + l.pc.study,
##                                             data = covariates),
##                               age ~ Exch(),
##                               time ~ AR1(coef = 1),
##                               age:region ~ Exch(),
##                               age:sex ~ Exch(),
##                               age:time ~ AR1(coef = 1)),
##                         y = y,
##                         exposure = exposure,
##                         filename = "est.at.res",
##                         nBurnin = 75000,
##                         nSim = 75000,
##                         nChain = 4,
##                         nThin = 200)
## pred.at <- predictModel(est.at,
##                         n = 25,
##                         filename = "pred.at.res")
## save(est.at, file = "est.at.RData")
## save(pred.at, file = "pred.at.RData")

## setwd(kOutDir)
## est.rt <- estimateModel(Model(y ~ Poisson(mean ~ age*region + age*sex + region*time,
##                                           jump = 0.5),
##                               region ~ Exch(mean ~ l.pc.overseas.born + l.pc.study,
##                                             data = covariates),
##                               age ~ Exch(),
##                               time ~ AR1(coef = 1),
##                               age:region ~ Exch(),
##                               age:sex ~ Exch(),
##                               region:time ~ AR1(coef = 1)),
##                         y = y,
##                         exposure = exposure,
##                         filename = "est.rt.res",
##                         nBurnin = 75000,
##                         nSim = 75000,
##                         nChain = 4,
##                         nThin = 200)
