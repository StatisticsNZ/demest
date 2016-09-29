
context("estimate-functions")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


## estimateModel ##################################################################

## ## Poisson fixed, no exposure
## mu <- rnorm(1, mean = 10, sd = 2)
## sigma <- runif(1, min = 0.1, max = 0.4)
## theta <- rgamma(n = 100, shape = sigma, rate = sigma / mu)
## theta <- Values(array(theta, dim = c(10, 10), dimnames = list(age = 0:9, region = letters[1:10])))
## y <- rpois(n = length(theta), lambda = theta)
## y <- Counts(array(y, dim = dim(theta), dimnames = dimnames(theta)))
## ## filename <- tempfile()
## filename <- "deleteme"
## estimateModel(Model(y ~ Poisson(mean = mu, exposure = FALSE, jump = 1)),
##               y = y,
##               filename = filename,
##               nBurnin = 300,
##               nSim = 100,
##               nChain = 4,
##               nThin = 2)
## theta.est <- fetch(filename, c("model", "likelihood", "mean"))
## mean.est <- fetch(filename, c("model", "prior", "mean"))
## dplot(count ~ age | region, data = theta.est,
##       overlay = list(values = theta, col = "red"),
##       midpoints = "age")


## filename1 <- tempfile()
## filename2 <- tempfile()
## y <- Counts(array(rpois(n = 12, lambda = 1:24),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"),
##                       age = 0:2,
##                       time = 2000:2003)))
## set.seed(1)
## estimateModel(Model(y ~ Poisson(mean ~ age),
##                     age ~ Exch()),
##               y = y,
##               filename = filename1,
##               nBurnin = 1000,
##               nSim = 1000,
##               nChain = 2,
##               nThin = 2)
## ans2 <- continueEstimation(ans1, nSim = 50)
## set.seed(1)
## ans3 <- estimateModel(Model(y ~ Poisson(mean ~ age, exposure = FALSE)),
##                       y = y,
##                       filename = filename2,
##                       nBurnin = 20,
##                       nSim = 50,
##                       nChain = 2,
##                       nThin = 2)
## slots.same <- setdiff(slotNames(class(ans2)), "control")
## for (name in slots.same) {
##     expect_identical(slot(ans2, name), slot(ans3, name))
## }
## elements.same <- setdiff(names(ans1@control), c("call", "filename"))
## for (name in elements.same)
##     expect_identical(ans2@control[[name]], ans3@control[[name]])

## y <- demdata::nz.visitors
## dimnames(y)$time <- seq_along(dimnames(y)$time)
## y <- Counts(y,
##             dimscales = c(time = "Intervals"))
## model <- Model(y ~ Poisson(mean ~ age + sex + country * time),
##                time ~ DLM(trend = NULL,
##                           damp = NULL,
##                           season = Season(n = 4)),
##                country:time ~ DLM(trend = NULL, damp = NULL),
##                jump = 0.15)
## filename <- tempfile()
## estimateModel(model,
##               y = y,
##               filename = filename,
##               nBurnin = 5000,
##               nSim = 5000,
##               nChain = 4,
##               nThin = 20)
## fetchSummary(filename)
## filename.pred <- tempfile()
## predictModel(filenameEst = filename,
##              filenamePred = filename.pred,
##              n = 20)
## counts <- fetchBoth(filenameEst = filename,
##                     filenamePred = filename.pred,
##                     where = c("mod", "li", "count"))
## dplot(~ time | country, data = counts, midpoints = "time", ylim = c(0, 500000),
##       overlay = list(values = y, col = "black"))

## mu <- fetchBoth(filenameEst = filename,
##                 filenamePred = filename.pred,
##                 where = c("mod", "pr", "mean"))
## mu <- as(mu, "Counts")
## dplot(~ time, data = exp(mu), prob = 0.5, midpoints = "time", 
##       overlay = list(values = y, col = "black"))

##       overlay = list(values = collapseIterations(counts, prob = 0.5), col = "red"))
      



## time <- fetchBoth(filenameEst = filename,
##                   filenamePred = filename.pred,
##                   where = c("mod", "prior", "time"))
## dplot(~ time, data = time)

## season <- fetchBoth(filename, filename.pred, c("mod", "hy", "ti", "season"))
## dplot(~ time, season)

## level <- fetchBoth(filename, filename.pred, c("mod", "hy", "ti", "level"))
## dplot(~ time, level)


## trend <- fetchBoth(filename, filename.pred, c("mod", "hy", "ti", "trend"))
## dplot(~ time, trend)



## age <- fetchBoth(filename, filename.pred, c("mod", "pr", "age"))
## dplot(~ age, age)

## country <- fetchBoth(filename, filename.pred, c("mod", "pr", "country"))
## dplot(~ country, country)







## plot(fetchMCMC(filename, c("mod", "hy", "ti", "lev")), sm = F, ask = T)
## plot(fetchMCMC(filename, c("mod", "hy", "ti", "scaleLev")), sm = F, ask = T)

## season <- fetch(filename, c("mod", "hy", "ti", "sea"))
## dplot(~ time, season)

## exposure <- Counts(array(rpois(n = 12, lambda = 1:24),
##                          dim = 2:4,
##                          dimnames = list(sex = c("f", "m"),
##                              age = 0:2,
##                              time = 2000:2003)),
##                    dimscales = c(age = "Intervals", time = "Intervals"))
## y <- Counts(array(rbinom(n = 12, size = exposure, prob = 0.6),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
##             dimscales = c(age = "Intervals", time = "Intervals"))
## value <- Values(array(0.4,
##                       dim = c(2, 1),
##                       dimnames = list(sex = c("f", "m"), time = "2003")),
##                 dimscales = c(time = "Intervals"))
## ag <- AgCertain(value)
## filename <- tempfile()
## estimateModel(Model(y ~ Binomial(mean ~ time),
##                     aggregate = ag),
##               y = y,
##               exposure = exposure,
##               filename = filename,
##               nBurnin = 20L,
##               nSim = 1000L,
##               nChain = 2L,
##               nThin = 10L)



## exposure <- Counts(array(rpois(n = 510, lambda = 1:510),
##                          dim = c(2, 51, 5),
##                          dimnames = list(sex = c("f", "m"),
##                              time = 2000:2050,
##                              region = 1:5)))
## y <- Counts(array(rbinom(n = 510, size = exposure, prob = 0.5),
##                   dim = c(2, 51, 5),
##                   dimnames = list(sex = c("f", "m"),
##                       time = 2000:2050,
##                       region = 1:5)))
## file.est <- tempfile()
## file.pred <- tempfile()
## estimateModel(Model(y ~ Binomial(mean ~ time),
##                     time ~ DLM(season = Season(n = 4)),
##                     jump = 0.2),
##               y = y,
##               exposure = exposure,
##               filename = file.est,
##               nBurnin = 500,
##               nSim = 500,
##               nChain = 4,
##               nThin = 2)

## theta <- fetch(file.est, c("mod", "li", "pr"))
## pred <- predictModel(fileEst = file.est, filePred = file.pred, n = 20)



## exposure <- Counts(array(rpois(n = 500, lambda = 1:500),
##                          dim = c(2, 50, 5),
##                          dimnames = list(sex = c("f", "m"), time = 2001:2050,
##                              region = 1:5)))
## y <- Counts(array(rbinom(n = 500, size = exposure, prob = rbind(1:50, 6:55)/100),
##                          dim = c(2, 50, 5),
##                          dimnames = list(sex = c("f", "m"), time = 2001:2050,
##                              region = 1:5)))
## filename <- tempfile()
## estimateModel(Model(y ~ Binomial(mean ~ time + sex, jump = 0.2),
##                            time ~ AR1(int = T, coef = 1)),
##                            y = y,
##                      exposure = exposure,
##                      filename = filename,
##                      nBurnin = 500,
##                      nSim = 500,
##                      nChain = 2,
##               nThin = 10)
## theta <- fetch(filename, where = c("model", "like", "prob"))
## pred <- predictModel(est, n = 20)

## set.seed(1)
## exposure <- Counts(array(rpois(n = 60, lambda = 1:60),
##                          dim = 3:5,
##                          dimnames = list(sex = c("f", "m", "u"), age = 0:3,
##                              region = 1:5)))
## y <- Counts(array(rbinom(n = 12, size = exposure, prob = 0.6),
##                   dim = 3:5,
##                   dimnames = list(sex = c("f", "m", "u"), age = 0:3,
##                       region = 1:5)))
## filename <- tempfile()
## estimateModel(Model(y ~ Binomial(mean ~ age + region)),
##               y = y,
##               exposure = exposure,
##               filename = filename,
##               nBurnin = 0,
##               nSim = 50,
##               nChain = 2,
##               nThin = 1)
## fetchSummary(filename)

## exposure <- Counts(array(as.double(rpois(n = 12, lambda = 1:24)),
##                          dim = 2:4,
##                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)))
## y <- Counts(array(as.integer(rpois(n = 12, lambda = exposure * 0.5)),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)))
## benchmarks <- Benchmarks(mean = sum(y)/sum(exposure))
## ans <- estimateModel(Model(y ~ Poisson(mean ~ age, benchmarks = benchmarks)),
##                      y = y,
##                      exposure = exposure,
##                      filename = tempfile(),
##                      nBurnin = 2000,
##                      nSim = 2000,
##                      nChain = 4,
##                      nThin = 10)


## exposure <- Counts(array(as.double(rpois(n = 110, lambda = 10)),
##                          dim = c(2, 10, 11),
##                          dimnames = list(sex = c("f", "m"),
##                              age = 0:9,
##                              time = 2000:2010)))
## y <- Counts(array(as.integer(rpois(n = 220, lambda = exposure * 0.5)),
##                   dim = c(2, 10, 11),
##                   dimnames = list(sex = c("f", "m"),
##                       age = 0:9,
##                       time = 2000:2010)))
## res <- estimateModel(Model(y ~ Poisson(mean ~ sex + age + time, jump = 0.2),
##                            age ~ Poly(order = 1, priorVarTr = matrix(10)),
##                            time ~ Poly(order = 1, priorVarTr = matrix(10))),
##                      y = y,
##                      exposure = exposure,
##                      filename = tempfile(),
##                      nBurnin = 4000,
##                      nSim = 4000,
##                      nChain = 4,
##                      nThin = 50)
## pred <- predictModel(res, along = "time", n = 2,
##                      filename = tempfile())



## y <- Counts(array(rnorm(n = 24, mean = 1:24),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)))
## y[20:24] <- NA
## filename <- tempfile()
## ans <- estimateModel(Model(y ~ Normal(mean ~ sex + time),
##                            time ~ Exch()),
##                      y = y, filename = filename, nBurnin = 500, nSim = 500,
##                      nChain = 2, nThin = 2)


## y <- Counts(array(rpois(n = 24, lambda = 1:24),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"),
##                       age = 0:2,
##                       time = 2000:2003)),
##             dimscales = c(age = "Intervals", time = "Intervals"))
## filename <- tempfile()
## estimateModel(Model(y ~ Poisson(mean ~ sex + time)),
##               y = y,
##               filename = filename,
##               nBurnin = 0,
##               nSim = 100,
##               nChain = 2,
##               nThin = 1)
## fetchSummary(filename)
## time.level <- fetch(filename, c("mod", "hyper", "time", "level"))
## time.scaleLevel <- fetch(filename, c("mod", "hyper", "time", "scaleLevel"))
## time.trend <- fetch(filename, c("mod", "hyper", "time", "trend"))
## time.scaleTrend <- fetch(filename, c("mod", "hyper", "time", "scaleTrend"))
## time.damp <- fetch(filename, c("mod", "hyper", "time", "damp"))


## set.seed(1)
## y <- Counts(array(rpois(n = 60, lambda = 10),
##                   dim = c(2, 3, 10),
##                   dimnames = list(sex = c("f", "m"), age = 0:2, dep = 1:10)))
## exposure <- y + 2
## filename <- tempfile()
## estimateModel(Model(y ~ Poisson(mean ~ sex + age + dep),
##                     dep ~ DLM(),
##                     jump = 0.2),
##               y = y,
##               exposure = exposure,
##               filename = filename,
##               nBurnin = 0,
##               nSim = 100,
##               nChain = 4,
##               nThin = 1)
## fetchSummary(filename)
## time.level <- fetch(filename, c("mod", "hyper", "time", "level"))
## time.scaleLevel <- fetch(filename, c("mod", "hyper", "time", "scaleLevel"))
## time.trend <- fetch(filename, c("mod", "hyper", "time", "trend"))
## time.scaleTrend <- fetch(filename, c("mod", "hyper", "time", "scaleTrend"))
## time.damp <- fetch(filename, c("mod", "hyper", "time", "damp"))




## y <- Counts(array(rpois(n = 24, lambda = 1:24),
##                   dim = 2:4,
##                   dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)))
## filename <- tempfile()
## ans <- estimateModel(Model(y ~ Poisson(mean = 10, exposure = FALSE, jump = 2)),
##                      y = y, filename = filename, nBurnin = 1000, nSim = 200000,
##                      nChain = 2L, nThin = 200)


## library(DemographicEstimation); library(testthat); test.identity <- F; n.test <- 5; options(warn = 2)
## filename1 <- tempfile()
## filename2 <- tempfile()
## lambda <- exp(outer(outer(rnorm(n = 10,
##                                 mean = seq(from = 2, to = 3.5, length = 10),
##                                 sd = 0.1),
##                           rnorm(2, sd = 0.2), "+"), rnorm(5, sd = 0.2), "+"))
## y <- Counts(array(rpois(n = length(lambda), lambda = lambda),
##                   dim = c(10, 2, 5),
##                   dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
## d1 <- Counts(array(rbinom(n = length(y), size = y, prob = 0.7),
##                    dim = dim(y),
##                    dimnames = dimnames(y)))
## d2 <- Counts(array(rpois(n = length(y)/ 2,
##                          lambda = collapseDimension(y, dim = "sex")),
##                    dim = c(10, 5),
##                    dimnames = list(age = 0:9, region = 1:5)))
## d2[c(2, 4)] <- NA
## d3 <- collapseDimension(y, dim = "region")
## observation <- list(Model(d1 ~ Binomial(prob = 0.7, jump = 0.01)),
##                     Model(d2 ~ Poisson(mean ~ region,
##                                        jump = 0.2,
##                                        lower = 0.1,
##                                        upper = 3)),
##                     Model(d3 ~ PoissonBinomial(prob = 0.95)))
## set.seed(1)
## res1 <- estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region,
##                       exposure = FALSE, lower = 2,
##                       jump = 0.3),
##                       age ~ Exch()),
##                       y = y,
##                       observation = observation,
##                       datasets = list(d1 = d1, d2 = d2, d3 = d3),
##                       nBurnin = 50,
##                       nSim = 10,
##                       nThin = 2,
##                       nChain = 2,
##                       filename = filename1)
## res2 <- continueEstimation(res1, nBurnin = 10, nSim = 20)
## set.seed(1)
## res3 <- estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region,
##                       exposure = FALSE, lower = 2,
##                       jump = 0.3),
##                       age ~ Exch()),
##                       y = y,
##                       observation = observation,
##                       datasets = list(d1 = d1, d2 = d2, d3 = d3),
##                       nBurnin = 70,
##                       nSim = 20,
##                       nThin = 2,
##                       nChain = 2,
##                       filename = filename2)
## slots.same <- setdiff(slotNames(class(res2)), "control")
## for (name in slots.same) {
##     expect_identical(slot(res2, name), slot(res3, name))
## }
## elements.same <- setdiff(names(res1@control), c("call", "filename"))
## for (name in elements.same)
##     expect_identical(res2@control[[name]], res3@control[[name]])
## t2 <- fetch(res2, c("mod", "li", "me"))
## t3 <- fetch(res3, c("mod", "li", "me"))
## expect_identical(t2, t3)







## dplot(~ age | region * sex,
##       data = fetch(res, c("model", "likelihood", "mean")),
##       overlay = list(values = y, col = "black"))
## dplot(~ age | region * sex,
##       data = fetch(res, c("observation", "d1", "likelihood", "prob")))


## births <- Counts(births.reg)
## females <- Counts(females.reg)
## res <- estimateModel(Model(y ~ Poisson(mean ~ age + region + year, jump = 0.2),
##                            region ~ Exch(mean ~ income + propn.maori, data = data.reg)),
##                      y = births,
##                      exposure = females,
##                      nBurnin = 200,
##                      nSim = 200,
##                      nThin = 10,
##                      filename = tempfile())


## spend <- USPersonalExpenditure
## names(dimnames(spend)) <- c("category", "year")
## spend <- Values(spend)
## spend <- log(spend)
## spend <- extrapolate(spend, along = "year", labels = c(1965, 1970, 1975),
##                      type = "missing")

## res <- estimateModel(Model(y ~ Normal(mean ~ category + year),
##                            year ~ Poly(order = 1)),
##                      y = spend,
##                      nBurnin = 1000,
##                      nSim = 10000,
##                      nThin = 10,
##                      nChain = 2,
##                      filename = tempfile())

## theta <- fetch(res, c("mod", "lik", "mean"))
## theta <- Values(theta@.Data)
## dplot(value ~ year | category,
##       data = theta,
##       overlay = list(values = spend, col = "orange"))


## sdObs <- fetch(res, c("mod", "hyp", "year", "sdObs"))


## plot(fetchMCMC(res, c("mod", "hyp", "year", "trend", "mean")), ask = T)

## gelman.diag(fetchMCMC(res, c("mod", "hyp", "year", "trend", "mean")))


## plot(fetchMCMC(res, c("mod", "hyp", "year", "trend", "sd", "order2")), ask = T)

## plot(fetchMCMC(res, c("mod", "prior", "param", "category")), ask = T)


## dplot(~ year,
##       data = fetch(res, c("mod", "prior", "param", "year")),
##       overlay = list(values = collapseIterations(fetch(res, c("mod", "hyp", "year", "trend", "mean")), FUN = median), col = "orange"))



## benchmarks <- Benchmarks(mean = 0.2)
## theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
## exposure <- as.integer(rpois(n = 20, lambda = 20))
## exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
## y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
## y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
## res <- estimateModel(Model(y ~ Binomial(mean ~ age + sex, benchmarks = benchmarks, jump = 5),
##                            age ~ Exch()),
##                      y = y,
##                      exposure = exposure,
##                      nBurnin = 10,
##                      nSim = 10,
##                      filename = tempfile())

