
context("query-functions")

test_that("fetchMCMC works with BinomialVarying", {
    MCMCDemographic <- demest:::MCMCDemographic
    fetchSkeleton <- demest:::fetchSkeleton
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.numeric(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 2, nThin = 2,
                  nChain = 2,
                  filename = filename)
    ## thinned is TRUE (default)
    set.seed(1)
    ans.obtained <- fetchMCMC(filename)
    set.seed(1)
    model.likelihood.prob <- MCMCDemographic(fetch(filename, c("model", "likelihood", "prob")),
                                             sample = NULL, nChain = 2L, nThin = 1L)
    model.prior.Intercept <-  MCMCDemographic(fetch(filename, c("model", "prior", "(Intercept)")),
                                              sample = NULL, nChain = 2L, nThin = 1L)
    model.prior.age <-  MCMCDemographic(fetch(filename, c("model", "prior", "age")),
                                        sample = NULL, nChain = 2L, nThin = 1L)
    model.prior.sex <-  MCMCDemographic(fetch(filename, c("model", "prior", "sex")),
                                        sample = NULL, nChain = 2L, nThin = 1L)
    model.prior.sd <- MCMCDemographic(fetch(filename, c("model", "prior", "sd")),
                                      sample = NULL, nChain = 2L, nThin = 1L)
    model.hyper.age.scaleError <- MCMCDemographic(fetch(filename, c("model", "hyper", "age", "scaleError")),
                                                  sample = NULL, nChain = 2L, nThin = 1L)
    ans.expected <- list(model.likelihood.prob = model.likelihood.prob,
                         "model.prior.(Intercept)" = model.prior.Intercept,
                         model.prior.age = model.prior.age,
                         model.prior.sex = model.prior.sex,
                         model.prior.sd = model.prior.sd,
                         model.hyper.age.scaleError = model.hyper.age.scaleError)
    expect_identical(ans.obtained, ans.expected)
    ## thinned is FALSE
    set.seed(1)
    ans.obtained <- fetchMCMC(filename, thinned = FALSE)
    set.seed(1)
    model.likelihood.prob <- MCMCDemographic(fetch(filename, c("model", "likelihood", "prob")),
                                             sample = NULL, nChain = 2L, nThin = 2L)
    model.prior.Intercept <-  MCMCDemographic(fetch(filename, c("model", "prior", "(Intercept)")),
                                              sample = NULL, nChain = 2L, nThin = 2L)
    model.prior.age <-  MCMCDemographic(fetch(filename, c("model", "prior", "age")),
                                        sample = NULL, nChain = 2L, nThin = 2L)
    model.prior.sex <-  MCMCDemographic(fetch(filename, c("model", "prior", "sex")),
                                        sample = NULL, nChain = 2L, nThin = 2L)
    model.prior.sd <- MCMCDemographic(fetch(filename, c("model", "prior", "sd")),
                                      sample = NULL, nChain = 2L, nThin = 2L)
    model.hyper.age.scaleError <- MCMCDemographic(fetch(filename, c("model", "hyper", "age", "scaleError")),
                                                  sample = NULL, nChain = 2L, nThin = 2L)
    ans.expected <- list(model.likelihood.prob = model.likelihood.prob,
                         "model.prior.(Intercept)" = model.prior.Intercept,
                         model.prior.age = model.prior.age,
                         model.prior.sex = model.prior.sex,
                         model.prior.sd = model.prior.sd,
                         model.hyper.age.scaleError = model.hyper.age.scaleError)
    expect_identical(ans.obtained, ans.expected)
    ## 'prob' only
    set.seed(1)
    ans.obtained <- fetchMCMC(filename, where = c("model", "likelihood", "prob"))
    set.seed(1)
    ans.expected <- MCMCDemographic(fetch(filename, c("model", "likelihood", "prob")),
                                    sample = NULL, nChain = 2L, nThin = 1L)
    expect_identical(ans.obtained, ans.expected)
    ## apprioriate errors
    expect_error(fetchMCMC(filename, where = "mcmc"),
                 "'mcmc' has class \"integer\"")
    expect_error(fetchMCMC(filename, where = "y"),
                 "'y' does not have dimension with dimtype \"iteration\"")
    ## supply sample argument
    set.seed(1)
    ans.obtained <- fetchMCMC(filename, where = c("model", "likelihood", "prob"), sample = 1)
    set.seed(1)
    ans.expected <- MCMCDemographic(fetch(filename, c("model", "likelihood", "prob")),
                                    sample = 1, nChain = 2L, nThin = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetch works", {
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.numeric(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 2, nThin = 1,
                  nChain = 2,
                  filename = filename)
    res <- fetchResultsObject(filename)
    ## fetch
    ans.obtained <- fetch(filename, where = "y")
    ans.expected <- y
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- fetch(filename, where = "control")
    ans.expected <- res@control
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- fetch(filename, where = "final")
    ans.expected <- res@final
    expect_identical(ans.obtained, ans.expected)
    expect_identical(ans.obtained, ans.expected)
    expect_error(fetch(filename, where = NULL),
                 "'where' has length 0")
    expect_error(fetch(filename, where = c("model", NA)),
                 "'where' has missing values")
    expect_error(fetch(filename, where = "final", iterations = integer()),
                 "'iterations' has length 0")
    expect_error(fetch(filename, where = "final", iterations = "1"),
                 "'iterations' does not have type \"numeric\"")
    expect_error(fetch(filename, where = "final", iterations = c(1, NA)),
                 "'iterations' has missing values")
    expect_error(fetch(filename, where = "final", iterations = 1.3),
                 "'iterations' has non-integer values")
    expect_error(fetch(filename, where = "final", iterations = 0),
                 "'iterations' has values less than 1")
    expect_error(fetch(filename, where = "final", iterations = 100),
                 "maximum value for 'iterations' argument \\[100\\] exceeds number of iterations \\[4\\]")
    expect_error(fetch(filename, where = "final", iterations = c(1, 1)),
                 "'iterations' has duplicates")
    expect_error(fetch(filename, where = "m"),
                 sprintf("'m' partially matches two or more of %s",
                         paste(sQuote(c("model", "y", "exposure", "mcmc", "control", "final", "seed")),
                               collapse = ", ")))
    expect_error(fetch(filename, where = "wrong"),
                 sprintf("'wrong' not found : choices are %s",
                         paste(sQuote(c("model", "y", "exposure", "mcmc", "control", "final", "seed")),
                               collapse = ", ")))
})

## test_that("fetchBoth works", {
##     combineEstPred <- demest:::combineEstPred
##     fetchResultsObject <- demest:::fetchResultsObject
##     fetchSkeleton <- demest:::fetchSkeleton
##     fetchInner <- demest:::fetchInner
##     sweepAllMargins <- demest:::sweepAllMargins
##     lengthValues <- demest:::lengthValues
##     listsAsSingleItems <- demest:::listsAsSingleItems
##     exposure <- Counts(array(as.double(rpois(n = 110, lambda = 10)),
##                              dim = c(2, 10, 11),
##                              dimnames = list(sex = c("f", "m"),
##                                  age = 0:9,
##                                  time = 2000:2010)),
##                        dimscales = c(time = "Intervals"))
##     y <- Counts(array(as.integer(rpois(n = 220, lambda = exposure * 0.5)),
##                       dim = c(2, 10, 11),
##                       dimnames = list(sex = c("f", "m"),
##                           age = 0:9,
##                           time = 2000:2010)),
##                 dimscales = c(time = "Intervals"))
##     filename.est <- tempfile()
##     filename.pred <- tempfile()
##     estimateModel(Model(y ~ Poisson(mean ~ sex + age * time)),
##                   y = y,
##                   exposure = exposure,
##                   filename = filename.est,
##                   nBurnin = 50,
##                   nSim = 50,
##                   nChain = 2,
##                   nThin = 10)
##     predictModel(filenameEst = filename.est,
##                  filenamePred = filename.pred,
##                  n = 2)
##     ## not time varying
##     ans.obtained <- fetchBoth(filenameEst = filename.est,
##                               filenamePred = filename.pred,
##                               where = c("mod", "pr", "sex"))
##     ans.expected <- fetch(filename.est, where = c("mod", "pr", "sex"))
##     expect_identical(ans.obtained, ans.expected)
##     ans.obtained <- fetchBoth(filenameEst = filename.est,
##                               filenamePred = filename.pred,
##                               where = c("mod", "li", "rate"))
##     est <- fetch(filename.est, c("mod", "li", "rate"))
##     pred <- fetch(filename.pred, c("mod", "li", "rate"))
##     ans.expected <- dbind(est, pred, along = "time")
##     expect_identical(ans.obtained, ans.expected)
##     ans.obtained <- fetchBoth(filenameEst = filename.est,
##                               filenamePred = filename.pred,
##                               where = c("mod", "pr", "time"))
##     est <- fetchInner(fetchResultsObject(filename.est)@model,
##                       nameObject = "model",
##                       where = c("pr", "time"),
##                       iterations = 1:10,
##                       filename = filename.est,
##                       lengthIter = lengthValues(fetchResultsObject(filename.est)@final[[1]]),
##                       nIteration = 10L,
##                       listsAsSingleItems = listsAsSingleItems(),
##                       shift = TRUE,
##                       impute = FALSE)
##     pred <- fetchInner(fetchResultsObject(filename.pred)@model,
##                       nameObject = "model",
##                       where = c("pr", "time"),
##                       iterations = 1:10,
##                       filename = filename.pred,
##                       lengthIter = lengthValues(fetchResultsObject(filename.pred)@final[[1]]),
##                       nIteration = 10L,
##                       listsAsSingleItems = listsAsSingleItems(),
##                       shift = TRUE,
##                       impute = FALSE)
##     ans.expected <- dbind(est, pred, along = "time")
##     ans.expected <- sweepAllMargins(ans.expected)
##     expect_identical(ans.obtained, ans.expected)
##     ## time varying, need to normalize - do not normalize
##     ans.obtained <- fetchBoth(filenameEst = filename.est,
##                               filenamePred = filename.pred,
##                               where = c("mod", "pr", "time"))
##     est <- fetchInner(fetchResultsObject(filename.est)@model,
##                       nameObject = "model",
##                       where = c("pr", "time"),
##                       iterations = 1:10,
##                       filename = filename.est,
##                       lengthIter = lengthValues(fetchResultsObject(filename.est)@final[[1]]),
##                       nIteration = 10L,
##                       listsAsSingleItems = listsAsSingleItems(),
##                       shift = FALSE,
##                       impute = FALSE)
##     pred <- fetchInner(fetchResultsObject(filename.pred)@model,
##                       nameObject = "model",
##                       where = c("pr", "time"),
##                       iterations = 1:10,
##                       filename = filename.pred,
##                       lengthIter = lengthValues(fetchResultsObject(filename.pred)@final[[1]]),
##                       nIteration = 10L,
##                       listsAsSingleItems = listsAsSingleItems(),
##                       shift = FALSE,
##                       impute = FALSE)
##     ans.expected <- dbind(est, pred, along = "time")
##     expect_identical(ans.obtained, ans.expected)
##     ans.expected <- combineEstPred(est = est, pred = pred)
##     expect_identical(ans.obtained, ans.expected)
## })

test_that("fetchFiniteSD works with ResultsModel", {
    y <- Values(array(rnorm(n = 10, mean = 20),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ region, sd = 2)),
                  y = y,
                  nBurnin = 0,
                  nSim = 10,
                  nChain = 4,
                  nThin = 1,
                  filename = filename)
    ans.obtained <- fetchFiniteSD(filename = filename)
    expect_is(ans.obtained, "FiniteSD")
})

test_that("finiteY works with ResultsModel - no missing values", {
    y <- Values(array(rnorm(n = 10, mean = 20),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ region, sd = 2)),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 10, lambda = 5),
                            dim = c(2, 5),
                            dimnames = list(sex = c("f", "m"), region = 1:5)))
    total <- sampled + Counts(array(rpois(n = 10, lambda = 20),
                                         dim = c(2, 5),
                                         dimnames = list(sex = c("f", "m"), region = 1:5)))
    ## standard case
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "mean"))
    varsigma <- fetch(filename, c("model", "likelihood", "sd"))
    ans.expected <- rnorm(n = length(theta),
                          mean = (total - sampled) * theta,
                          sd = varsigma * sqrt(total - sampled))
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Values(ans.expected)
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## iterations supplied
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled,
                            iterations = 3:4)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "mean"), iterations = 3:4)
    varsigma <- fetch(filename, c("model", "likelihood", "sd"))
    ans.expected <- rnorm(n = length(theta),
                          mean = (total - sampled) * theta,
                          sd = varsigma * sqrt(total - sampled))
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 2),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 3:4))
    ans.expected <- Values(ans.expected)
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## no 'sampled' supplied
    expect_error(finiteY(filename, total = total),
                 "argument 'sampled' is missing with no default")
    ## sampled > total
    expect_error(finiteY(filename, total = sampled, sampled = total),
                 "'total' has cells with fewer units than corresponding cells in 'sampled'")
})

test_that("finiteY works with ResultsModel - with missing values", {
    y <- Values(array(rnorm(n = 10, mean = 20),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    y[1] <- NA
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ region, sd = 2)),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 10, lambda = 5),
                            dim = c(2, 5),
                            dimnames = list(sex = c("f", "m"), region = 1:5)))
    total <- sampled + Counts(array(rpois(n = 10, lambda = 20),
                                         dim = c(2, 5),
                                         dimnames = list(sex = c("f", "m"), region = 1:5)))
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "mean"))
    varsigma <- fetch(filename, c("model", "likelihood", "sd"))
    yy <- fetch(filename, "y", impute = TRUE)
    ans.expected <- rnorm(n = length(theta),
                          mean = (total - sampled) * theta,
                          sd = varsigma * sqrt(total - sampled))
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Values(ans.expected)
    ans.expected <- yy + ans.expected
    expect_identical(ans.obtained, ans.expected)
})

test_that("finiteY works with ResultsModelExposure - no missing values", {
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ 1)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 1, nThin = 1, nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 24, lambda = 5),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(time = "Intervals"))
    total <- sampled + Counts(array(rpois(n = 24, lambda = 20),
                                         dim = 2:4,
                                    dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                              dimscales = c(time = "Intervals"))
    ## standard case
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected,
                           dimscales = c(time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## iterations supplied
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled,
                            iterations = 2)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"), iterations = 2)
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 1),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 2))
    ans.expected <- Counts(ans.expected,
                           dimscales = c(time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## no 'sampled' supplied
    sampled <- Counts(array(rpois(n = 24, lambda = 5),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(time = "Intervals"))
    total <- sampled + Counts(array(rpois(n = 24, lambda = 20),
                                         dim = 2:4,
                                    dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                              dimscales = c(time = "Intervals"))
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - exposure,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected,
                           dimscales = c(time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## sampled > total
    expect_error(finiteY(filename, total = sampled, sampled = total),
                 "'total' has cells with fewer units than corresponding cells in 'sampled'")
})

test_that("finiteY works with ResultsModelExposure - no missing values", {
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(age = "Intervals", time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ 1)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 1, nThin = 1, nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 24, lambda = 5),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(age = "Intervals", time = "Intervals"))
    total <- sampled + Counts(array(rpois(n = 24, lambda = 20),
                                         dim = 2:4,
                                    dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                              dimscales = c(age = "Intervals", time = "Intervals"))
    ## standard case
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## iterations supplied
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled,
                            iterations = 2)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"), iterations = 2)
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 1),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## no 'sampled' supplied
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - exposure,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## sampled > total
    expect_error(finiteY(filename, total = sampled, sampled = total),
                 "'total' has cells with fewer units than corresponding cells in 'sampled'")
})

test_that("finiteY works with ResultsModelExposure - no missing values", {
    y <- Counts(array(as.integer(rbinom(n = 24, size = 20, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    exposure <- y + 1
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ 1)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 1, nThin = 1, nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 24, lambda = 5),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(age = "Intervals", time = "Intervals"))
    total <- sampled + Counts(array(rpois(n = 24, lambda = 20),
                                         dim = 2:4,
                                    dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                              dimscales = c(age = "Intervals", time = "Intervals"))
    ## standard case
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## iterations supplied
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled,
                            iterations = 2)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"), iterations = 2)
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 1),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## no 'sampled' supplied
    total <- exposure
    sampled <- Counts(array(rbinom(n = 24, prob = 0.5, size = total),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(age = "Intervals", time = "Intervals"))
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta),
                           size = total - exposure,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- y + ans.expected
    expect_identical(ans.obtained, ans.expected)
    ## sampled > total
    expect_error(finiteY(filename, total = sampled, sampled = total),
                 "'total' has cells with fewer units than corresponding cells in 'sampled'")
})

test_that("finiteY works with ResultsModelExposure - with missing values", {
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(age = "Intervals", time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    y[1] <- NA
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ 1)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 1, nThin = 1, nChain = 2,
                  filename = filename)
    sampled <- Counts(array(rpois(n = 24, lambda = 5),
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                      dimscales = c(age = "Intervals", time = "Intervals"))
    total <- sampled + Counts(array(rpois(n = 24, lambda = 20),
                                         dim = 2:4,
                                    dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                              dimscales = c(age = "Intervals", time = "Intervals"))
    set.seed(1)
    ans.obtained <- finiteY(filename, total = total, sampled = sampled)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    yy <- fetch(filename, "y", impute = TRUE)
    ans.expected <- rbinom(n = length(theta),
                           size = total - sampled,
                           prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2:4, 2),
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003,
                              iteration = 1:2))
    ans.expected <- Counts(ans.expected, dimscales = c(age = "Intervals", time = "Intervals"))
    ans.expected <- yy + ans.expected
    expect_identical(ans.obtained, ans.expected)
})

