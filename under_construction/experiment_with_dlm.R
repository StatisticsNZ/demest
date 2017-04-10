
library(demdata)
library(demest)

deaths <- demdata::sweden.deaths
deaths <- Counts(deaths,
                 dimscales = c(year = "Intervals"))


popn <- demdata::sweden.popn
popn <- Counts(popn,
               dimscales = c(year = "Intervals"))
deaths <- collapseIntervals(deaths,
                            dimension = "age",
                            breaks = c(0, 1, seq(5, 90, 5)))
popn <- collapseIntervals(popn,
                          dimension = "age",
                          breaks = c(0, 1, seq(5, 90, 5)))
deaths <- collapseDimension(deaths,
                            dimension = "region")
popn <- collapseDimension(popn,
                          dimension = "region")
age.levels <- c("0",
                "1-4",
                paste(seq(5, 85, 5), seq(9, 89, 5), sep = "-"),
                "90+")
is.infant <- c(1, rep(0, length(age.levels) - 1))
is.infant.df <- data.frame(age = age.levels,
                           is.infant)
model <- Model(y ~ Poisson(mean ~ age * sex + year),
               age ~ DLM(covariates = Covariates(mean ~ is.infant,
                                                 data = is.infant.df),
                         damp = NULL),
               year ~ DLM(damp = NULL,
                          level = Level(scale = HalfT(scale = 0.2)),
                          trend = Trend(scale = HalfT(scale = 0.2)),
                          error = Error(scale = HalfT(scale = 0.2))),
               jump = 0.08)
filename.est <- "baseline2.est"
estimateModel(model,
              y = deaths,
              exposure = popn + 0.5,
              filename = filename.est,
              nBurnin = 1000,
              nSim = 1000,
              nThin = 10,
              nChain = 4)
s <- fetchSummary(filename.est)
print(s)
for (i in 1:4) {
    continueEstimation(filename = filename.est,
                       nBurnin = 2000,
                       nSim = 2000)
    s <- fetchSummary(filename.est)
    print(s)
}


filename.pred <- "baseline.pred"
predictModel(filenameEst = filename.est,
             filenamePred = filename.pred,
             n = 10)


age.year <- fetchBoth(filename.est,
                      filename.pred,
                 where = c("model", "prior", "age:year"))
dplot(~ year | age,
      data = age.year,
      midpoints = "year"



rate <- fetchBoth(filename.est, filename.pred,
                  where = c("model", "likelihood", "rate"))


dplot( ~ age | year,
      data = rate,
      subarray = sex == "Females" & year %in% c("1968", "1990", "2015", "2025"),
      midpoints = "age")


dplot(log(value) ~ age | year,
      data = rate,
      subarray = sex == "Females" & year %in% c("1968", "1990", "2015", "2025"),
      midpoints = "age")


dplot(value ~ year | age,
      data = rate,
      subarray = sex == "Females" & age %in% c("0", "20-24", "60-64", "90+"),
      midpoints = "year")




rate.mcmc <- fetchMCMC(filename.est,
                       where = c("model", "likelihood", "rate"))
plot(rate.mcmc, sm = F, ask = T)


age.year <- fetch(filename.est,
                 where = c("model", "prior", "age:year"))
dplot(~ age | year,
      data = age.year,
      midpoints = "age")





weights <- fetch(filename.est,
                 where = c("model", "hyper", "age:year", "weights"))
dplot(~ year | component,
      data = weights,
      midpoints = TRUE,
      as.table = TRUE)

weights.mcmc <- fetchMCMC(filename.est, 
                          where = c("model", "hyper", "age:year", "weights"))
plot(weights.mcmc, smooth = FALSE, ask = TRUE)



level <- fetch(filename.est,
                 where = c("model", "hyper", "age:year", "level2AR"))
dplot(~ year | component,
      data = level)



components <- fetch(filename.est,
                 where = c("model", "hyper", "age:year", "components"))
dplot(~ age | component,
      data = components,
      midpoints = "age")


      subarray = component %in% 1:5)

dplot(~ age,
      data = components,
      prob = 0.5,
      groups = component,
      midpoints = "age")

components.mcmc <- fetchMCMC(filename.est,
                 where = c("model", "hyper", "age:year", "components"))
plot(components.mcmc, sm = F, ask = T)




age.year.mcmc <- fetchMCMC(filename.est, 
                           where = c("model", "prior", "age:year"))
plot(age.year.mcmc, ask = T, sm= F)

