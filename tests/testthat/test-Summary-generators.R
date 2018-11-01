
context("Summary-generators")

## summary ###############################################################

test_that("summary works with object of class Model", {
    initialModel <- demest:::initialModel
    y <- Values(array(rnorm(20),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(runif(20),
                            dim = 5:4,
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region),
                  age ~ Exch())
    x <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- summary(x)
    ans.expected <- new("SummaryModel",
                        specification = "y ~ Normal(mean ~ age + region)",
                        dimensions = c("age", "region"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("summary works with object of class SkeletonManyCounts", {
    Skeleton <- demest:::Skeleton
    object <- Counts(array(1:2, dim = 2L, dimnames = list(age = c("0-4", "5-9"))))
    object <- Skeleton(object = object, first = 3L)
    ans.obtained <- summary(object)
    ans.expected <- new("SummarySeries",
                        dimensions = "age",
                        nCell = 2L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("summary works with ResultsModelEst", {
    summaryDataset <- demest:::summaryDataset
    fetchResultsObject <- demest:::fetchResultsObject
    makeParameters <- demest:::makeParameters
    makeMetropolis <- demest:::makeMetropolis
    makeGelmanDiag <- demest:::makeGelmanDiag
    y <- Counts(array(rpois(n = 24, lambda = 1:24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Poisson(mean ~ sex, useExpose = FALSE)),
                  y = y,
                  filename = filename,
                  nBurnin = 2,
                  nSim = 8,
                  nChain = 2L)
    set.seed(1)
    object <- fetchResultsObject(filename)
    ans.obtained <- summary(object, filename = filename, nSample = 10)
    set.seed(1)
    ans.expected <- new("SummaryResultsModelEst",
                        parameters = makeParameters(object = object, filename = filename),
                        gelmanDiag = makeGelmanDiag(object = object,
                                                    filename = filename,
                                                    nSample = 10),
                        metropolis = makeMetropolis(object = object, filename = filename,
                                                    nSample = 10),
                        model = summary(object@final[[1]]@model),
                        y = summaryDataset(y),
                        mcmc = object@mcmc,
                        nSampleMCMC = new("Length", 10L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("summary works with ResultsModelPred", {
    summaryDataset <- demest:::summaryDataset
    fetchResultsObject <- demest:::fetchResultsObject
    makeParameters <- demest:::makeParameters
    y <- Counts(array(rpois(n = 24, lambda = 1:24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename.est <- tempfile()
    filename.pred <- tempfile()
    estimateModel(Model(y ~ Poisson(mean ~ sex, useExpose = FALSE)),
                  y = y,
                  filename = filename.est,
                  nBurnin = 2,
                  nSim = 8,
                  nChain = 2L)
    predictModel(filenameEst = filename.est, filenamePred = filename.pred, n = 3)
    set.seed(1)
    object <- fetchResultsObject(filename.pred)
    expect_is(object, "ResultsModelPred")
    ans.obtained <- summary(object, filename = filename.pred)
    ans.expected <- new("SummaryResultsModelPred",
                        parameters = makeParameters(object = object, filename = filename.pred),
                        model = summary(object@final[[1]]@model),
                        mcmc = object@mcmc)
    expect_identical(ans.obtained, ans.expected)
})

test_that("summary works with object of class ResultsCountsEst", {
    fetchResultsObject <- demest:::fetchResultsObject
    summaryDataset <- demest:::summaryDataset
    makeParameters <- demest:::makeParameters
    makeMetropolis <- demest:::makeMetropolis
    makeGelmanDiag <- demest:::makeGelmanDiag
    lambda <- exp(outer(outer(rnorm(n = 10, mean = seq(from = -1, to = 3, length = 10)),
                              rnorm(2), "+"), rnorm(5), "+"))
    y <- Counts(array(rpois(n = length(lambda), lambda = lambda),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
    d1 <- Counts(array(rbinom(n = length(y), size = y, prob = 0.7),
                       dim = dim(y),
                       dimnames = dimnames(y)))
    d2 <- Counts(array(rpois(n = length(y)/ 2,
                             lambda = collapseDimension(y, dim = "sex")),
                       dim = c(10, 5),
                       dimnames = list(age = 0:9, region = 1:5)))
    d2[1] <- NA
    d3 <- collapseDimension(y, dim = "region")
    model <- Model(y ~ Poisson(mean ~ age + sex + region, useExpose = FALSE),
                   jump = 0.3)
    observation <- list(Model(d1 ~ Binomial(mean ~ 1), jump = 0.03),
                        Model(d2 ~ Poisson(mean ~ region), jump = 0.2),
                        Model(d3 ~ PoissonBinomial(prob = 0.95)))
    filename <- tempfile()
    estimateCounts(model = model,
                   y = y,
                   dataModels = observation,
                   datasets = list(d1 = d1, d2 = d2, d3 = d3),
                   filename = filename,
                   nBurnin = 2,
                   nSim = 20,
                   nChain = 2)
    object <- fetchResultsObject(filename)
    set.seed(1)
    ans.obtained <- summary(object, filename = filename, nSample = 25)
    set.seed(1)
    ans.expected <- new("SummaryResultsCounts",
                        mcmc = object@mcmc,
                        parameters = makeParameters(object, filename),
                        gelmanDiag = makeGelmanDiag(object, filename,
                                                    nSample = 25),
                        metropolis = makeMetropolis(object, filename,
                                                    nSample = 25),
                        model = summary(object@final[[1]]@model),
                        y = summary(object@y),
                        dataModels = list(
                            summary(object@final[[1]]@dataModels[[1]]),
                            summary(object@final[[1]]@dataModels[[2]]),
                            summary(object@final[[1]]@dataModels[[3]])),
                        datasets = list(
                            summaryDataset(d1),
                            summaryDataset(d2),
                            summaryDataset(d3)),
                        namesDatasets = c("d1", "d2", "d3"),
                        nSampleMCMC = new("Length", 25L))
    expect_identical(ans.obtained, ans.expected)
})


## summaryDataset #########################################################

test_that("summaryDataset works with object of class Demographic", {
    summaryDataset <- demest:::summaryDataset
    ## integers, some observed values
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"), age = 0:2)))
    ans.obtained <- summaryDataset(object)
    ans.expected <- new("SummaryDataset",
                        classStr = "Counts",
                        dimensions = c("sex", "age"),
                        nCell = 6L,
                        nMissing = 1L,
                        isIntegers = TRUE,
                        nZero = 0L,
                        median = 3)
    expect_identical(ans.obtained, ans.expected)    
    ## non-integers, some observed values
    object <- Counts(array(c(1:4, 4.5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"), age = 0:2)))
    ans.obtained <- summaryDataset(object)
    ans.expected <- new("SummaryDataset",
                        classStr = "Counts",
                        dimensions = c("sex", "age"),
                        nCell = 6L,
                        nMissing = 1L,
                        isIntegers = FALSE,
                        nZero = as.integer(NA),
                        median = 3)
    expect_identical(ans.obtained, ans.expected)    
    ## no observed values
    object <- Values(array(as.numeric(NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"), age = 0:2)))
    ans.obtained <- summaryDataset(object)
    ans.expected <- new("SummaryDataset",
                        classStr = "Values",
                        dimensions = c("sex", "age"),
                        nCell = 6L,
                        nMissing = 6L,
                        isIntegers = NA,
                        nZero = as.integer(NA),
                        median = as.numeric(NA))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("summaryDataset works with object of class SkeletonMissingData", {
    summaryDataset <- demest:::summaryDataset
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaUnknown")
    model@w <- rep(0.3, 6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object),
                            sd = Skeleton(first = 7L)))
    object <- SkeletonMissingData(object = object,
                                  model = model,
                                  outputModel = outputModel,
                                  exposure = NULL)
    ans.obtained <- summaryDataset(object)
    ans.expected <- summaryDataset(object@data)
    expect_identical(ans.obtained, ans.expected)
})
                        
