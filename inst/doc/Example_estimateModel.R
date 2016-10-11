## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(demest)
births <- demdata::nz.births
popn <- demdata::nz.popn.reg
births <- Counts(births,
                 dimscales = c(year = "Intervals"))
popn <- Counts(popn,
               dimscales = c(year = "Intervals"))
births <- subarray(births, year > 1995)
females <- subarray(popn, sex == "Female")

## ---- fig.width = 6, fig.height = 5--------------------------------------
plot(births)
plot(females)

## ---- fig.width = 6.5, fig.height = 7------------------------------------
dplot(~ year | region,
      data = births / females,
      groups = age,
      midpoints = "year",
      auto.key = list(lines = TRUE, points = FALSE))

## ------------------------------------------------------------------------
model.main <- Model(y ~ Poisson(mean ~ age + region + year))

## ------------------------------------------------------------------------
model.main

## ---- fig.width = 4, fig.height = 3--------------------------------------
plotHalfT(df = 7, scale = 1, ylim = c(0, 1.6))
plotHalfT(df = 7, scale = 0.5, add = TRUE, col = "red")

## ------------------------------------------------------------------------
model.inter <- Model(y ~ Poisson(mean ~ age * region + age * year))
model.inter

## ------------------------------------------------------------------------
filename.main.est <- tempfile()

## ------------------------------------------------------------------------
set.seed(1) ## for reproducibility
estimateModel(model = model.main,
              y = births,
              exposure = females,
              filename = filename.main.est,
              nBurnin = 100,
              nSim = 100,
              nChain = 4,
              nThin = 2)

## ------------------------------------------------------------------------
showModel(filename.main.est)

## ------------------------------------------------------------------------
fetchSummary(filename.main.est)

## ------------------------------------------------------------------------
model.main <- Model(y ~ Poisson(mean ~ age + region + year),
                    jump = 0.15)

## ------------------------------------------------------------------------
set.seed(1)
estimateModel(model = model.main,
              y = births,
              exposure = females,
              filename = filename.main.est,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename.main.est)

## ------------------------------------------------------------------------
listContents(filename.main.est)

## ------------------------------------------------------------------------
rates <- fetch(filename.main.est, 
               where = c("model", "likelihood", "rate"))
## we can abbreviate when there is no ambiguity
age.effect <- fetch(filename.main.est,
                    where = c("mod", "pr", "age"))

## ------------------------------------------------------------------------
class(rates)
summary(rates)
summary(age.effect)

## ---- fig.width = 6.5, fig.height = 7------------------------------------
dplot(~ year | age * region,
      data = rates,
      subarray = region %in% c("Auckland", "Taranaki", "Southland"))

## ------------------------------------------------------------------------
age.effect.quant <- collapseIterations(age.effect,
                                       prob = c(0.025, 0.5, 0.975))
round(age.effect.quant, 2)

## ---- fig.width = 6, fig.height = 4.5------------------------------------
region.scale.error.mcmc <- fetchMCMC(filename.main.est,
                                     where = c("model", "hyper", "region", "scaleError"))
plot(region.scale.error.mcmc,
     smooth = FALSE)

## ------------------------------------------------------------------------
filename.main.pred <- tempfile()
predictModel(filenameEst = filename.main.est,
             filenamePred = filename.main.pred,
             n = 10)

## ------------------------------------------------------------------------
showModel(filename.main.pred)

## ---- fig.width = 6.5, fig.height = 7------------------------------------
rates.both <- fetchBoth(filenameEst = filename.main.est,
                        filenamePred = filename.main.pred,
                        where = c("model", "likelihood", "rate"))
dplot(~ year | age * region,
      data = rates.both,
      subarray = region %in% c("Auckland", "Taranaki", "Southland"))

## ------------------------------------------------------------------------
prior.year <- DLM(level = Level(scale = HalfT(scale = 0.1)),
                  trend = NULL,
                  damp = NULL,
                  error = Error(scale = HalfT(scale = 0.1)))
prior.year

## ------------------------------------------------------------------------
prior.age <- DLM(trend = NULL, damp = NULL)
prior.age

## ------------------------------------------------------------------------
data.reg <- demdata::nz.census.reg
covariates.reg <- Covariates(mean ~ pr.maori + pr.inc.50,
                             data = data.reg)
error.reg <- Error(robust = TRUE, scale = HalfT(mult = 2))
prior.reg <- Exch(covariates = covariates.reg,
                  error = error.reg)
prior.reg

## ------------------------------------------------------------------------
error.age.reg <- Error(robust = TRUE, scale = HalfT(mult = 2))
prior.age.reg <- Exch(error = error.reg)
prior.age.reg

## ------------------------------------------------------------------------
level.age.year <- Level(scale = HalfT(scale = 0.05))
damp.age.year <- Damp(coef = 0.9)
error.age.year <- Error(scale = HalfT(scale = 0.05))
prior.age.year <- DLM(level = level.age.year,
                      trend = NULL,
                      damp = damp.age.year,
                      error = error.age.year)
prior.age.year

## ------------------------------------------------------------------------
model.fancy <- Model(y ~ Poisson(mean ~ age * region + age * year),
                     age ~ prior.age,
                     region ~ prior.reg,
                     year ~ prior.year,
                     age:region ~ prior.age.reg,
                     age:year ~ prior.age.year,
                     jump = 0.08)
model.fancy

## ---- eval = FALSE-------------------------------------------------------
#  filename.fancy.est <- tempfile()
#  filename.fancy.pred <- tempfile()
#  set.seed(1)
#  estimateModel(model.fancy,
#                y = births,
#                exposure = females,
#                filename = filename.fancy.est,
#                nBurnin = 20000,
#                nSim = 20000,
#                nThin = 40,
#                nChain = 4)
#  predictModel(filename.fancy.est,
#               filename.fancy.pred,
#               n = 10)
#  rates.fancy <- fetchBoth(filenameEst = filename.fancy.est,
#                           filenamePred = filename.fancy.pred,
#                           where = c("model", "likelihood", "rate"))
#  dplot(~ year | age * region,
#        data = rates.fancy,
#        subarray = region %in% c("Auckland", "Taranaki", "Southland"))

