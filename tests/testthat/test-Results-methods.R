
context("Results-methods")

test_that("finiteSDObject works with ResultsModel", {
    finiteSDObject <- demest:::finiteSDObject
    fetchResultsObject <- demest:::fetchResultsObject
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
    obj <- fetchResultsObject(filename)
    ans.obtained <- finiteSDObject(object = obj,
                                   filename = filename)
    expect_is(ans.obtained, "FiniteSD")
})


test_that("finiteSDObject works with ResultsCounts", {
    finiteSDObject <- demest:::finiteSDObject
    fetchResultsObject <- demest:::fetchResultsObject
    finiteSDInner <- demest:::finiteSDInner
    lambda <- exp(outer(outer(rnorm(n = 10, mean = seq(from = -1, to = 3, length = 10)),
                              rnorm(2), "+"), rnorm(5), "+"))
    y <- Counts(array(as.integer(rpois(n = length(lambda), lambda = lambda)),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
    d1 <- Counts(array(as.integer(rbinom(n = length(y), size = y, prob = 0.7)),
                       dim = dim(y),
                       dimnames = dimnames(y)))
    d2 <- Counts(array(as.integer(rpois(n = length(y)/ 2, lambda = collapseDimension(y, dim = "sex"))),
                       dim = c(10, 5),
                       dimnames = list(age = 0:9, region = 1:5)))
    d3 <- collapseDimension(y, dim = "region")
    filename <- tempfile()
    estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region, useExpose = FALSE),
                                 jump = 0.3,
                                 age ~ Exch()),
                   y = y,
                   dataModels = list(Model(d1 ~ Binomial(mean ~ 1), jump = 0.03),
                                     Model(d2 ~ Poisson(mean ~ region), jump = 0.2, lower = 0.3),
                                     Model(d3 ~ PoissonBinomial(prob = 0.95))),
                   datasets = list(d1 = d1, d2 = d2, d3 = d3),
                   filename = filename,
                   nBurnin = 5,
                   nSim = 5,
                   nChain = 2)
    obj <- fetchResultsObject(filename)
    ans.obtained <- finiteSDObject(obj, filename = filename)
    ans.expected <- list(model = finiteSDInner(model = obj@final[[1L]]@model,
                                               filename = filename,
                                               where = "model",
                                               probs = c(0.025, 0.5, 0.975),
                                               iterations = NULL),
                         dataModels = list(d1 = NULL,
                                           d2 = finiteSDInner(model = obj@final[[1L]]@dataModels[[2]],
                                                              filename = filename,
                                                              where = c("dataModels", "d2"),
                                                              probs = c(0.025, 0.5, 0.975),
                                                              iterations = NULL),
                                           d3 = NULL))
    expect_identical(ans.obtained, ans.expected)
})

test_that("nIteration works with BinomialVarying", {
    fetchResultsObject <- demest:::fetchResultsObject
    makeAutocorr <- demest:::makeAutocorr
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
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
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- nIteration(object)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
})

test_that("rescaleBetasPred works", {
    rescaleBetasPred <- demest:::rescaleBetasPred
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename.est <- tempfile()
    filename.pred <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex + time),
                        time ~ Exch()),
                  y = y,
                  exposure = exposure,
                  nBurnin = 2,
                  nSim = 5,
                  nThin = 1,
                  nChain = 2,
                  filename = filename.est)
    predictModel(filenameEst = filename.est,
                 filenamePred = filename.pred,
                 n = 5)    
    results.est <- fetchResultsObject(filename.est)
    results.pred <- fetchResultsObject(filename.pred)
    nIteration.est <- results.est@mcmc[["nIteration"]]
    lengthIter.est <- results.est@control$lengthIter
    nIteration.pred <- results.pred@mcmc[["nIteration"]]
    lengthIter.pred <- results.pred@control$lengthIter
    namesBetas <- results.pred@final[[1]]@model@namesBetas
    con <- file(filename.est, open = "rb")
    size.res <- readBin(con, what = "integer", n = 1L)
    size.adj <- readBin(con, what = "integer", n = 1L)
    res <- readBin(con, what = "raw", n = size.res)
    data <- readBin(con, what = "double", n = nIteration.est * lengthIter.est)
    adj.ser <- readBin(con, what = "raw", n = size.adj)
    adjustments <- unserialize(adj.ser)
    close(con)
    betas0 <- lapply(namesBetas,
                     function(x) fetch(filename.pred, c("model", "prior", x)))
    rescaleBetasPred(results = results.pred, 
                     adjustments = adjustments,
                     filename = filename.pred,
                     nIteration = nIteration.pred,
                     lengthIter = lengthIter.pred)
    for (i in seq_along(betas0)) {
        name <- namesBetas[i]
        beta1 <- fetch(filename.pred,
                       where = c("model", "prior", name))
        name.adj <- paste("model.prior", name, sep = ".")
        if (name == "time")
            expect_equal(beta1, betas0[[i]] + adjustments[[name.adj]])
        else
            expect_equal(beta1, betas0[[i]])
    }
})

test_that("rescalePriors works with BinomialVarying", {
    rescalePriors <- demest:::rescalePriors
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
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
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    results <- fetchResultsObject(filename)
    adjustments <- new.env(hash = TRUE)
    nIteration <- results@mcmc[["nIteration"]]
    lengthIter <- results@control$lengthIter
    rescalePriors(results = results,
                  adjustments = adjustments,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
    expect_true(setequal(names(adjustments),
                         c("model.prior.(Intercept)",
                           "model.prior.age",
                           "model.prior.sex")))
})

test_that("rescalePriors works with ResultsCounts", {
    rescalePriors <- demest:::rescalePriors
    fetchResultsObject <- demest:::fetchResultsObject
    lambda <- exp(outer(outer(rnorm(n = 10, mean = seq(from = -1, to = 3, length = 10)),
                              rnorm(2), "+"), rnorm(5), "+"))
    y <- Counts(array(as.integer(rpois(n = length(lambda), lambda = lambda)),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
    d1 <- Counts(array(as.integer(rbinom(n = length(y), size = y, prob = 0.7)),
                       dim = dim(y),
                       dimnames = dimnames(y)))
    d2 <- Counts(array(as.integer(rpois(n = length(y)/ 2, lambda = collapseDimension(y, dim = "sex"))),
                       dim = c(10, 5),
                       dimnames = list(age = 0:9, region = 1:5)))
    d3 <- collapseDimension(y, dim = "region")
    filename <- tempfile()
    estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region, useExpose = FALSE),
                       jump = 0.3,
                       age ~ Exch()),
                   y = y,
                   dataModels = list(Model(d1 ~ Binomial(mean ~ 1), jump = 0.03),
                       Model(d2 ~ Poisson(mean ~ region), jump = 0.2, lower = 0.3),
                       Model(d3 ~ PoissonBinomial(prob = 0.95))),
                   datasets = list(d1 = d1, d2 = d2, d3 = d3),
                   filename = filename,
                   nBurnin = 5,
                   nSim = 5,
                   nChain = 2)
    results <- fetchResultsObject(filename)
    adjustments <- new.env(hash = TRUE)
    nIteration <- results@mcmc[["nIteration"]]
    lengthIter <- results@control$lengthIter
    rescalePriors(results = results,
                  adjustments = adjustments,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
    expect_true(setequal(names(adjustments),
                         c("model.prior.(Intercept)",
                           "model.prior.age",
                           "model.prior.sex",
                           "model.prior.region",
                           "dataModels.d2.prior.(Intercept)",
                           "dataModels.d2.prior.region")))
})



test_that("whereMetropStat works with ResultsModel from BinomialVarying", {
    whereMetropStat <- demest:::whereMetropStat
    whereAcceptance <- demest:::whereAcceptance
    whereAutocorr <- demest:::whereAutocorr
    whereJump <- demest:::whereJump
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
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
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- whereMetropStat(object, FUN = whereAcceptance)
    ans.expected <- list(c("model", "likelihood", "acceptProb"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereAutocorr)
    ans.expected <- list(c("model", "likelihood", "prob"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereJump)
    ans.expected <- list(c("model", "likelihood", "jumpProb"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereMetropStat works with ResultsModel from NormalVarying", {
    set.seed(1)
    whereMetropStat <- demest:::whereMetropStat
    whereAcceptance <- demest:::whereAcceptance
    whereAutocorr <- demest:::whereAutocorr
    whereJump <- demest:::whereJump
    fetchResultsObject <- demest:::fetchResultsObject
    y <- Counts(array(rnorm(n = 24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    ## no constraints
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ age + sex)),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- whereMetropStat(object, FUN = whereAcceptance)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereAutocorr)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereJump)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ## lower = 0
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ age + sex), lower = 0),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    ans.obtained <- whereMetropStat(object, FUN = whereAcceptance)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereAutocorr)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereJump)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
    ## aggregate
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ age + sex),
                        lower = 0,
                        aggregate = AgNormal(value = 0, sd = 0.2)),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- whereMetropStat(object, FUN = whereAcceptance)
    ans.expected <- list(c("model", "likelihood", "acceptMean"),
                         c("model", "aggregate", "accept"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereAutocorr)
    ans.expected <- list(c("model", "likelihood", "mean"),
                         c("model", "aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereJump)
    ans.expected <- list(c("model", "likelihood", "jumpMean"),
                         c("model", "aggregate", "jump"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereMetropStat works with ResultsCounts", {
    whereMetropStat <- demest:::whereMetropStat
    whereAcceptance <- demest:::whereAcceptance
    whereAutocorr <- demest:::whereAutocorr
    whereJump <- demest:::whereJump
    fetchResultsObject <- demest:::fetchResultsObject
    lambda <- exp(outer(outer(rnorm(n = 10, mean = seq(from = -1, to = 3, length = 10)),
                              rnorm(2), "+"), rnorm(5), "+"))
    y <- Counts(array(as.integer(rpois(n = length(lambda), lambda = lambda)),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
    d1 <- Counts(array(as.integer(rbinom(n = length(y), size = y, prob = 0.7)),
                       dim = dim(y),
                       dimnames = dimnames(y)))
    d2 <- Counts(array(as.integer(rpois(n = length(y)/ 2, lambda = collapseDimension(y, dim = "sex"))),
                       dim = c(10, 5),
                       dimnames = list(age = 0:9, region = 1:5)))
    d3 <- collapseDimension(y, dim = "region")
    filename <- tempfile()
    estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region, useExpose = FALSE),
                       jump = 0.3,
                       age ~ Exch()),
                   y = y,
                   dataModels = list(Model(d1 ~ Binomial(mean ~ 1), jump = 0.03),
                       Model(d2 ~ Poisson(mean ~ region), jump = 0.2, lower = 0.3),
                       Model(d3 ~ PoissonBinomial(prob = 0.95))),
                   datasets = list(d1 = d1, d2 = d2, d3 = d3),
                   filename = filename,
                   nBurnin = 5,
                   nSim = 5,
                   nChain = 2)
    object <- fetchResultsObject(filename)
    expect_true(validObject(object))
    ans.obtained <- whereMetropStat(object, FUN = whereAcceptance)
    ans.expected <- list(c("model", "likelihood", "acceptCount"),
                         c("dataModels", "d1", "likelihood", "acceptProb"),
                         c("dataModels", "d2", "likelihood", "acceptRate"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereAutocorr)
    ans.expected <- list(c("model", "likelihood", "count"),
                         c("dataModels", "d1", "likelihood", "prob"),
                         c("dataModels", "d2", "likelihood", "rate"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- whereMetropStat(object, FUN = whereJump)
    ans.expected <- list(c("model", "likelihood", "jumpCount"),
                         c("dataModels", "d1", "likelihood", "jumpProb"),
                         c("dataModels", "d2", "likelihood", "jumpRate"))
    expect_identical(ans.obtained, ans.expected)
})
