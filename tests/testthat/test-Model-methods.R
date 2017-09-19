
context("Model-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## drawYNonSampled #########################################################################

test_that("drawYNonSampled works with Binomial", {
    drawYNonSampled <- demest:::drawYNonSampled
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(rpois(n = 10, lambda = 10),
                             dim = c(2, 5),
                             dimnames = list(sex = c("f", "m"), region = 1:5)))
    y <- Counts(array(rbinom(n = 10, size = exposure, prob = 0.5),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ region)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 2,
                  nSim = 2,
                  nChain = 2,
                  filename = filename)
    res <- fetchResultsObject(filename)
    model <- res@final[[1L]]@model
    nonsampled <- 2 * exposure
    set.seed(1)
    ans.obtained <- drawYNonSampled(filename = filename,
                                    model = model,
                                    nonsampled = nonsampled,
                                    iterations = NULL)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    ans.expected <- rbinom(n = length(theta), size = nonsampled, prob = theta)
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("drawYNonSampled works with Poisson with exposure", {
    drawYNonSampled <- demest:::drawYNonSampled
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(rpois(n = 10, lambda = 10),
                             dim = c(2, 5),
                             dimnames = list(sex = c("f", "m"), region = 1:5)))
    y <- Counts(array(rpois(n = 10, lambda = 2 * exposure),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Poisson(mean ~ region)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 2,
                  nSim = 2,
                  nChain = 2,
                  filename = filename)
    res <- fetchResultsObject(filename)
    model <- res@final[[1L]]@model
    nonsampled <- 2 * exposure
    set.seed(1)
    ans.obtained <- drawYNonSampled(filename = filename,
                                    model = model,
                                    nonsampled = nonsampled,
                                    iterations = NULL)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "rate"))
    ans.expected <- rpois(n = length(theta), lambda = nonsampled * theta)
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("drawYNonSampled throws error with Poisson without exposure", {
    drawYNonSampled <- demest:::drawYNonSampled
    fetchResultsObject <- demest:::fetchResultsObject
    y <- Counts(array(rpois(n = 10, lambda = 20),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Poisson(mean ~ region)),
                         y = y,
                         nBurnin = 0,
                         nSim = 2,
                         nChain = 2,
                         filename = filename)
    res <- fetchResultsObject(filename)
    model <- res@final[[1L]]@model
    nonsampled <- 2 * y
    expect_error(drawYNonSampled(filename = filename,
                                 model = model,
                                 nonsampled = nonsampled,
                                 iterations = NULL),
                 "finite-population estimates not defined for Poisson model without exposure")
})

test_that("drawYNonSampled works with Normal varsigma known", {
    drawYNonSampled <- demest:::drawYNonSampled
    fetchResultsObject <- demest:::fetchResultsObject
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
    res <- fetchResultsObject(filename)
    model <- res@final[[1L]]@model
    nonsampled <- Counts(array(rpois(n = 10, lambda = 10),
                               dim = c(2, 5),
                               dimnames = list(sex = c("f", "m"), region = 1:5)))
    set.seed(1)
    ans.obtained <- drawYNonSampled(filename = filename,
                                    model = model,
                                    nonsampled = nonsampled,
                                    iterations = NULL)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "mean"))
    varsigma <- fetch(filename, c("model", "likelihood", "sd"))
    ans.expected <- rnorm(n = length(theta),
                          mean = nonsampled * theta,
                          sd = varsigma * sqrt(nonsampled))
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("drawYNonSampled works with Normal varsigma unknown", {
    drawYNonSampled <- demest:::drawYNonSampled
    fetchResultsObject <- demest:::fetchResultsObject
    y <- Values(array(rnorm(n = 10, mean = 20),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), region = 1:5)))
    filename <- tempfile()
    estimateModel(Model(y ~ Normal(mean ~ region)),
                  y = y,
                  nBurnin = 0,
                  nSim = 2,
                  nChain = 2,
                  filename = filename)
    res <- fetchResultsObject(filename)
    model <- res@final[[1L]]@model
    nonsampled <- Counts(array(rpois(n = 10, lambda = 10),
                               dim = c(2, 5),
                               dimnames = list(sex = c("f", "m"), region = 1:5)))
    set.seed(1)
    ans.obtained <- drawYNonSampled(filename = filename,
                                    model = model,
                                    nonsampled = nonsampled,
                                    iterations = NULL)
    set.seed(1)
    theta <- fetch(filename, c("model", "likelihood", "mean"))
    varsigma <- as.numeric(fetch(filename, c("model", "likelihood", "sd")))
    varsigma <- rep(varsigma, each = length(y))
    ans.expected <- rnorm(n = length(theta),
                          mean = as.numeric(nonsampled) * as.numeric(theta),
                          sd = varsigma * sqrt(as.numeric(nonsampled)))
    ans.expected <- array(ans.expected,
                          dim = c(2, 5, 4),
                          dimnames = list(sex = c("f", "m"), region = 1:5, iteration = 1:4))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

## getTransform #################################################################

test_that("getTransform works with BinomialVarying", {
    getTransform <- demest:::getTransform
    x <- new("BinomialVarying")
    ans.obtained <- getTransform(x)
    ans.expected <- function(x) log(x / (1 - x))
    expect_equal(ans.obtained, ans.expected)
})

test_that("getTransform works with NormalVarying", {
    getTransform <- demest:::getTransform
    x <- new("NormalVaryingVarsigmaKnown")
    ans.obtained <- getTransform(x)
    ans.expected <- function(x) x
    expect_equal(ans.obtained, ans.expected)
})

test_that("getTransform works with PoissonVarying", {
    getTransform <- demest:::getTransform
    x <- new("PoissonVaryingUseExpAgCertain")
    ans.obtained <- getTransform(x)
    ans.expected <- log
    expect_equal(ans.obtained, ans.expected)
})

          
## logLikelihood ######################################################################

## Calculations tested in tests for logLikelihood_Binomial, logLikelihood_Poisson,
## logLikelihood_PoissonBinomialMixture in 'test-helper.functions.R'. Here just test
## that method dispatch is working correctly.

test_that("R, C-generic, and C-specific versions of logLikelihood give same answer with BinomialVarying", {
    logLikelihood <- demest:::logLikelihood
    initialModel <- demest:::initialModel
    BetaIterator <- demest:::BetaIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 1.5))
        ans.R <- logLikelihood(model = model,
                               count = count,
                               dataset = dataset,
                               i = i,
                               useC = FALSE)
        ans.C.generic <- logLikelihood(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE,
                                       useSpecific = FALSE)
        ans.C.specific <- logLikelihood(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE,
                                        useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

test_that("R, C-generic, and C-specific versions of logLikelihood give same answer with PoissonVaryingUseExp", {
    logLikelihood <- demest:::logLikelihood
    initialModel <- demest:::initialModel
    BetaIterator <- demest:::BetaIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood(model = model,
                               count = count,
                               dataset = dataset,
                               i = i,
                               useC = FALSE)
        ans.C.generic <- logLikelihood(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE,
                                       useSpecific = FALSE)
        ans.C.specific <- logLikelihood(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE,
                                        useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

test_that("logLikelihood gives valid answer with PoissonBinomialMixture", {
    logLikelihood <- demest:::logLikelihood
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood(model = model,
                                      count = count,
                                      dataset = dataset,
                                      i = i)
        ans.expected <- dpoibin1(x = dataset[i], size = count, prob = model@prob, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R, C-generic, and C-specific versions of logLikelihood give same answer with PoissonBinomialMixture", {
    logLikelihood <- demest:::logLikelihood
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood(model = model,
                               count = count,
                               dataset = dataset,
                               i = i,
                               useC = FALSE)
        ans.C.generic <- logLikelihood(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE,
                                       useSpecific = FALSE)
        ans.C.specific <- logLikelihood(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE,
                                        useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

test_that("R, C-generic, and C-specific versions of logLikelihood give same answer with NormalFixedUseExp", {
    logLikelihood <- demest:::logLikelihood
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        mean <- Values(array(runif(10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood(model = model,
                               count = count,
                               dataset = dataset,
                               i = i,
                               useC = FALSE)
        ans.C.generic <- logLikelihood(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE,
                                       useSpecific = FALSE)
        ans.C.specific <- logLikelihood(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE,
                                        useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})




## makeCellInLik ######################################################################

test_that("makeCellInLik works with BinomialVarying", {
    makeCellInLik <- demest:::makeCellInLik
    initialModel <- demest:::initialModel
    ## no missing
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Binomial(mean ~ sex + age))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeCellInLik(model,
                                  y = y)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    ## has missing
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    y[c(1, 15)] <- NA
    spec <- Model(y ~ Binomial(mean ~ sex + age))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeCellInLik(model,
                                  y = y)
    ans.expected <- model
    ans.expected@cellInLik <- c(FALSE, rep(TRUE, 13), FALSE, rep(TRUE, 5))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeCellInLik works with subtotals", {
    makeCellInLik <- demest:::makeCellInLik
    initialModel <- demest:::initialModel
    y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    y[c(1:12, 20)] <- NA
    subtotals <- Counts(array(30L, dim = 1, dimnames = list(age = "0-4")))
    y <- attachSubtotals(y, subtotals = subtotals)
    spec <- Model(y ~ Poisson(mean ~ sex + age))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- makeCellInLik(model,
                                  y = y)
    ans.expected <- model
    ans.expected@cellInLik[20] <- FALSE
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeCellInLik works with aggregate", {
    makeCellInLik <- demest:::makeCellInLik
    initialModel <- demest:::initialModel
    y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    y[c(10, 20)] <- NA
    value <- ValuesOne(2:4, labels = 7:9, name = "age")
    sd <- sqrt(value)
    aggregate <- AgNormal(value = value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- makeCellInLik(model = model,
                                  y = y)
    ans.expected <- model
    ans.expected@cellInLik[10] <- FALSE
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeCellInLik works with AgLif", {
    makeCellInLik <- demest:::makeCellInLik
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
    y[1:2] <- NA
    value <- ValuesOne(4, labels = "f", name = "sex")
    sd <- sqrt(value)
    aggregate <- AgLife(value = value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeCellInLik(model = model,
                                  y = y)
    ans.expected <- model
    ans.expected@cellInLik <- c(TRUE, FALSE, rep(TRUE, 18))
    expect_identical(ans.obtained, ans.expected)
})

## not sure if we will keep the classes

## test_that("makeCellInLik works with predict plus aggregate ", {
##     makeCellInLik <- demest:::makeCellInLik
##     initialModel <- demest:::initialModel
##     initialModelPredict <- demest:::initialModelPredict
##     weights.old <- Counts(array(runif(50),
##                                 dim = c(5, 10),
##                                 dimnames = list(age = 0:4, region = letters[1:10])))
##     y <- Counts(array(rnorm(50),
##                       dim = c(5, 10),
##                       dimnames = list(age = 0:4, region = letters[1:10])))
##     spec <- Model(y ~ Normal(mean ~ age + region, sd = 2.1))
##     x.old <- initialModel(spec, y = y, weights = weights.old)
##     value <- ValuesOne(3, letters[12], "region")
##     weights.ag <- Counts(array(runif(50),
##                                 dim = c(5, 4),
##                                 dimnames = list(age = 0:4, region = letters[11:14])))
##     aggregate <- AgCertain(value = value, weights = weights.ag)
##     x.new <- initialModelPredict(x.old,
##                                  along = 2L,
##                                  labels = letters[11:14],
##                                  n = NULL,
##                                  offsetModel = 1L,
##                                  covariates = NULL,
##                                  aggregate = aggregate,
##                                  lower = NULL,
##                                  upper = NULL)
##     ans.obtained <- makeCellInLik(x.new)
##     ans.expected <- x.new
##     ans.expected@cellInLik <- c(rep(FALSE, 5), rep(TRUE, 5), rep(FALSE, 10))
##     expect_identical(ans.obtained, ans.expected)
## })

## makeOutputAggregate ################################################################

test_that("makeOutputAggregate works with AgCertain", {
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## mean is Values
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgCertain(mean)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    weights <- new("Counts",
                   .Data = prop.table(exposure, margin = 1),
                   metadata = exposure@metadata)
    ans.obtained <- makeOutputAggregate(model = model)
    ans.expected <- list(value = mean,
                         weights = weights)
    expect_identical(ans.obtained, ans.expected)
    ## mean is scalar
    mean <- sum(y)/sum(exposure)
    aggregate <- AgCertain(mean)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    weights <- new("Counts", .Data = prop.table(exposure), metadata = exposure@metadata)
    ans.obtained <- makeOutputAggregate(model = model)
    ans.expected <- list(value = mean,
                         weights = weights)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputAggregate works with AgNormal", {
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    Skeleton <- demest:::Skeleton
    SkeletonAccept <- demest:::SkeletonAccept
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## mean is Values
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(value)
    aggregate <- AgNormal(value = value, sd = sd)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    weights <- new("Counts", .Data = prop.table(exposure, margin = 1), metadata = exposure@metadata)
    ans.obtained <- makeOutputAggregate(model = model, pos = 20L,
                                        nChain = 2L, nIteration = 50L)
    ans.expected <- list(value = Skeleton(metadata = model@metadataAg,
                             first = 20L),
                         jump = aggregate@scaleAg@.Data,
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 22L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 2L, first = 23L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         sd = sd,
                         weights = weights)
    expect_identical(ans.obtained, ans.expected)
    ## mean is scalar
    value <- sum(y)/sum(exposure)
    sd <- 1
    aggregate <- AgNormal(value, sd = sd)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOutputAggregate(model = model, pos = 10L,
                                        nChain = 2L, nIteration = 50L)
    weights <- new("Counts", .Data = prop.table(exposure), metadata = exposure@metadata)
    ans.expected <- list(value = Skeleton(first = 10L),
                         jump = aggregate@scaleAg@.Data,
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 11L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 1L, first = 12L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         sd = sd,
                         weights = weights)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputAggregate works with AgLife", {
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    Skeleton <- demest:::Skeleton
    SkeletonAccept <- demest:::SkeletonAccept
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
    ## mean is Values
    value <- ValuesOne(c(4, 5), labels = c("f", "m"), name = "sex")
    sd <- sqrt(value)
    aggregate <- AgLife(value = value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOutputAggregate(model = model, pos = 20L)
    ans.expected <- list(value = Skeleton(metadata = model@metadataAg,
                             first = 20L),
                         mean = value,
                         sd = sd,
                         mx = Skeleton(metadata = model@metadataMxAg,
                                       first = 22L))
    expect_identical(ans.obtained, ans.expected)
    ## mean is scalar
    value <- 5
    sd <- 1
    aggregate <- AgLife(value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOutputAggregate(model = model, pos = 10L)
    ans.expected <- list(value = Skeleton(first = 10L),
                         mean = value,
                         sd = sd,
                         mx = Skeleton(metadata = model@metadataMxAg,
                                       first = 11L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputAggregate works with AgFun", {
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    Skeleton <- demest:::Skeleton
    SkeletonAccept <- demest:::SkeletonAccept
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## mean is Values
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(value)
    aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                       FUN = function(x, weights) sum(x * weights) / sum(weights))
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    weights <- new("Counts", .Data = prop.table(exposure, margin = 1), metadata = exposure@metadata)
    ans.obtained <- makeOutputAggregate(model = model, pos = 20L,
                                        nChain = 2L, nIteration = 50L)
    ans.expected <- list(value = Skeleton(metadata = model@metadataAg,
                             first = 20L),
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 22L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 2L, first = 23L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         sd = sd)
    expect_identical(ans.obtained, ans.expected)
    ## mean is scalar
    value <- sum(y)/sum(exposure)
    sd <- 1
    aggregate <- AgFun(value = value, sd = sd,
                       FUN = function(x, weights) sum(x * weights) / sum(weights))
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOutputAggregate(model = model, pos = 10L,
                                        nChain = 2L, nIteration = 50L)
    weights <- new("Counts", .Data = prop.table(exposure), metadata = exposure@metadata)
    ans.expected <- list(value = Skeleton(first = 10L),
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 11L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 1L, first = 12L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         sd = sd)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputAggregate works with AgPoisson", {
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    Skeleton <- demest:::Skeleton
    SkeletonAccept <- demest:::SkeletonAccept
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## mean is Values
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgPoisson(value = value)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    weights <- new("Counts", .Data = prop.table(exposure, margin = 1), metadata = exposure@metadata)
    ans.obtained <- makeOutputAggregate(model = model, pos = 20L,
                                        nChain = 2L, nIteration = 50L)
    ans.expected <- list(value = Skeleton(metadata = model@metadataAg,
                             first = 20L),
                         jump = aggregate@scaleAg@.Data,
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 22L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 2L, first = 23L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         exposure = collapseDimension(exposure, dimension = "age"),
                         weights = weights)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## mean is scalar
    value <- sum(y)/sum(exposure)
    aggregate <- AgPoisson(value)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOutputAggregate(model = model, pos = 10L,
                                        nChain = 2L, nIteration = 50L)
    weights <- new("Counts", .Data = prop.table(exposure), metadata = exposure@metadata)
    ans.expected <- list(value = Skeleton(first = 10L),
                         jump = aggregate@scaleAg@.Data,
                         noProposal = SkeletonAccept(nAttempt = 100L, first = 11L,
                             nChain = 2L, nIteration = 50L),
                         accept = SkeletonAccept(nAttempt = 1L, first = 12L,
                             nChain = 2L, nIteration = 50L),
                         mean = value,
                         exposure = sum(exposure),
                         weights = weights)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})



## makeOutputModel ###################################################################

test_that("makeOutputModel works with NormalVaryingVarsigmaKnown - no aggregate", {
    makeOutputModel <- demest:::makeOutputModel
    initialModel <- demest:::initialModel
    SkeletonAccept <- demest:::SkeletonAccept
    SkeletonMu <- demest:::SkeletonMu
    lengthValues <- demest:::lengthValues
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Normal(mean ~ sex + age, sd = 2),
                  age ~ Exch())
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 11L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 11L,
                           last = 30L,
                           metadata = metadata),
                       noProposal = SkeletonAccept(nAttempt = 20L,
                           first = 31L,
                           nChain = 2L,
                           nIteration = 20L),
                       sd = model@varsigma@.Data,
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = list(scaleError = new("SkeletonOneValues",
                                 first = 46L)))
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    ans.expected <- list(likelihood = likelihood, prior = prior, hyper = hyper)
    expect_identical(ans.obtained, ans.expected)
    expect_identical(lengthValues(model) + pos - 1L, 46L)
})

test_that("makeOutputModel works with NormalVaryingVarsigmaKnown - AgCertain", {
    makeOutputModel <- demest:::makeOutputModel
    initialModel <- demest:::initialModel
    makeOutputAggregate <- demest:::makeOutputAggregate
    SkeletonMu <- demest:::SkeletonMu
    lengthValues <- demest:::lengthValues
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- ValuesOne(values = c(0, 0), labels = c("f", "m"), name = "sex")
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Normal(mean ~ sex + age, sd = 2),
                  age ~ Exch(),
                  aggregate = aggregate)    
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 11L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 11L,
                           last = 30L,
                           metadata = metadata),
                       jumpMean = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept",
                           nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)),
                       acceptMean = new("SkeletonNAccept",
                           nAttempt = 20L, first = 32L,
                           iFirstInChain = c(1L, 11L)),
                       sd = model@varsigma@.Data,
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 33L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 33L),
                  sex = new("SkeletonBetaTerm",
                      first = 34L,
                      last = 35L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 36L,
                      last = 45L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 46L)
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                   sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = list(scaleError = new("SkeletonOneValues",
                                 first = 47L)))
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    aggregate <- makeOutputAggregate(model)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
    expect_identical(lengthValues(model) + pos - 1L, 47L)
})

test_that("makeOutputModel works with NormalVaryingVarsigmaKnown - AgNormal", {
    makeOutputModel <- demest:::makeOutputModel
    initialModel <- demest:::initialModel
    makeOutputAggregate <- demest:::makeOutputAggregate
    SkeletonMu <- demest:::SkeletonMu
    lengthValues <- demest:::lengthValues
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- ValuesOne(values = c(0, 0), labels = c("f", "m"), name = "sex")
    sd <- ValuesOne(values = c(1, 1.1), labels = c("f", "m"), name = "sex")
    aggregate <- AgNormal(value = value, sd = sd)
    spec <- Model(y ~ Normal(mean ~ sex + age, sd = 2),
                  age ~ Exch(),
                  aggregate = aggregate)    
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 11L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 11L,
                           last = 30L,
                           metadata = metadata),
                       jumpMean = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept",
                           nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)),
                       acceptMean = new("SkeletonNAccept",
                           nAttempt = 20L, first = 32L,
                           iFirstInChain = c(1L, 11L)),
                       sd = model@varsigma@.Data,
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 33L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 33L),
                  sex = new("SkeletonBetaTerm",
                      first = 34L,
                      last = 35L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 36L,
                      last = 45L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 46L)
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = list(scaleError = new("SkeletonOneValues",
                                 first = 47L)))
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    aggregate <- makeOutputAggregate(model,
                                     pos = 48L,
                                     nChain = 2L,
                                     nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with NormalVaryingVarsigmaUnknown - no aggregate", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    initialModel <- demest:::initialModel
    SkeletonMu <- demest:::SkeletonMu
    makeOutputPrior <- demest:::makeOutputPrior
    lengthValues <- demest:::lengthValues
    y <- Counts(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Normal(mean ~ sex + age))
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 10L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 10L,
                           last = 29L,
                           metadata = metadata),
                       noProposal = new("SkeletonNAccept",
                           nAttempt = 20L, first = 30L,
                           iFirstInChain = c(1L, 11L)),
                       sd = new("SkeletonOneValues",
                           first = 31L),
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 46L))
    ans.expected <- list(likelihood = likelihood, prior = prior, hyper = hyper)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with NormalVaryingVarsigmaUnknown - AgCertain", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    SkeletonMu <- demest:::SkeletonMu
    initialModel <- demest:::initialModel
    makeOutputPrior <- demest:::makeOutputPrior
    lengthValues <- demest:::lengthValues
    y <- Counts(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- ValuesOne(values = c(0, 0), labels = c("f", "m"), name = "sex")
    aggregate <- AgCertain(value)
    spec <- Model(y ~ Normal(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 10L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 10L,
                           last = 29L,
                           metadata = metadata),
                       jumpMean = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept",
                           nAttempt = 20L, first = 30L,
                           iFirstInChain = c(1L, 11L)),
                       acceptMean = new("SkeletonNAccept",
                           nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)),
                       sd = new("SkeletonOneValues",
                           first = 32L),
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 33L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 33L),
                  sex = new("SkeletonBetaTerm",
                      first = 34L,
                      last = 35L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 36L,
                      last = 45L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 46L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 47L))
    aggregate <- makeOutputAggregate(model)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with NormalVaryingVarsigmaUnknown - AgNormal", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    SkeletonMu <- demest:::SkeletonMu
    initialModel <- demest:::initialModel
    makeOutputPrior <- demest:::makeOutputPrior
    lengthValues <- demest:::lengthValues
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- ValuesOne(values = c(0, 0), labels = c("f", "m"), name = "sex")
    sd <- ValuesOne(values = c(1, 1.1), labels = c("f", "m"), name = "sex")
    aggregate <- AgNormal(value = value, sd = sd)
    spec <- Model(y ~ Normal(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, weights = weights)
    metadata <- y@metadata
    pos <- 11L
    mcmc <- c(nChain = 2L, nIteration = 20L)
    ans.obtained <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
    likelihood <- list(mean = new("SkeletonManyValues",
                           first = 11L,
                           last = 30L,
                           metadata = metadata),
                       jumpMean = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept",
                           nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)),
                       acceptMean = new("SkeletonNAccept",
                           nAttempt = 20L, first = 32L,
                           iFirstInChain = c(1L, 11L)),
                       sd = new("SkeletonOneValues",
                           first = 33L),
                       weights = model@w)
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 34L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 34L),
                  sex = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 36L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 37L,
                      last = 46L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 47L)
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = list(scaleError = new("SkeletonOneValues", first = 48L)))
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    aggregate <- makeOutputAggregate(model,
                                     pos = 49L,
                                     nChain = 2L,
                                     nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with BinomialVarying - no aggregate", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    initialModel <- demest:::initialModel
    makeOutputPrior <- demest:::makeOutputPrior
    SkeletonMu <- demest:::SkeletonMu
    lengthValues <- demest:::lengthValues
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Binomial(mean ~ sex + age))
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 10L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(prob = new("SkeletonManyValues",
                           first = 10L,
                           last = 29L,
                           metadata = metadata),
                       jumpProb = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 30L,
                           iFirstInChain = c(1L, 11L)),
                       acceptProb = new("SkeletonNAccept", nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 46L))
    ans.expected <- list(likelihood = likelihood, prior = prior, hyper = hyper)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with BinomialVarying - AgCertain", {
    makeOutputModel <- demest:::makeOutputModel
    initialModel <- demest:::initialModel
    makeOutputPrior <- demest:::makeOutputPrior
    lengthValues <- demest:::lengthValues
    makeOutputAggregate <- demest:::makeOutputAggregate
    SkeletonMu <- demest:::SkeletonMu
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgCertain(value)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 10L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(prob = new("SkeletonManyValues",
                           first = 10L,
                           last = 29L,
                           metadata = metadata),
                       jumpProb = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 30L,
                           iFirstInChain = c(1L, 11L)),
                       acceptProb = new("SkeletonNAccept", nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 46L))
    aggregate <- makeOutputAggregate(model)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with BinomialVarying - AgNormal", {
    makeOutputModel <- demest:::makeOutputModel
    initialModel <- demest:::initialModel
    makeOutputPrior <- demest:::makeOutputPrior
    SkeletonMu <- demest:::SkeletonMu
    lengthValues <- demest:::lengthValues
    makeOutputAggregate <- demest:::makeOutputAggregate
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(value) + 0.1
    aggregate <- AgNormal(value, sd = sd)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 11L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(prob = new("SkeletonManyValues",
                       first = 11L,
                       last = 30L,
                       metadata = metadata),
                       jumpProb = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 31L,
                       iFirstInChain = c(1L, 11L)),
                       acceptProb = new("SkeletonNAccept", nAttempt = 20L, first = 32L,
                       iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 33L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 33L),
                  sex = new("SkeletonBetaTerm",
                      first = 34L,
                      last = 35L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 36L,
                      last = 45L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 46L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 47L))
    aggregate <- makeOutputAggregate(model, pos = hyper$age$scaleError@first + 1L,
                                     nChain = 2L, nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with PoissonVarying - no aggregate", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    initialModel <- demest:::initialModel
    SkeletonMu <- demest:::SkeletonMu
    makeOutputPrior <- demest:::makeOutputPrior
    lengthValues <- demest:::lengthValues
    y <- Counts(array(rpois(20, lambda  = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Poisson(mean ~ sex + age))
    model <- initialModel(spec, y = y, exposure = NULL)
    metadata <- y@metadata
    pos <- 10L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(count = new("SkeletonManyCounts",
                           first = 10L,
                           last = 29L,
                           metadata = metadata),
                       jumpCount = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 30L,
                           iFirstInChain = c(1L, 11L)),
                       acceptCount = new("SkeletonNAccept", nAttempt = 20L, first = 31L,
                           iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 46L))
    ans.expected <- list(likelihood = likelihood, prior = prior, hyper = hyper)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with PoissonVaryingUseExp - AgCertain", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    lengthValues <- demest:::lengthValues
    SkeletonMu <- demest:::SkeletonMu
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgCertain(value)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 10L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(rate = new("SkeletonManyValues",
                       first = 10L,
                       last = 29L,
                       metadata = metadata),
                       jumpRate = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 30L,
                       iFirstInChain = c(1L, 11L)),
                       acceptRate = new("SkeletonNAccept", nAttempt = 20L, first = 31L,
                       iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 32L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 32L),
                  sex = new("SkeletonBetaTerm",
                      first = 33L,
                      last = 34L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 35L,
                      last = 44L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 45L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 46L))
    aggregate <- makeOutputAggregate(model)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with PoissonVaryingUseExp - AgNormal", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    lengthValues <- demest:::lengthValues
    SkeletonMu <- demest:::SkeletonMu
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(value) + 0.1
    aggregate <- AgNormal(value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 21L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(rate = new("SkeletonManyValues",
                       first = 21L,
                       last = 40L,
                       metadata = metadata),
                       jumpRate = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 41L,
                       iFirstInChain = c(1L, 11L)),
                       acceptRate = new("SkeletonNAccept", nAttempt = 20L, first = 42L,
                       iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 43L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 43L),
                  sex = new("SkeletonBetaTerm",
                      first = 44L,
                      last = 45L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 46L,
                      last = 55L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 56L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 57L))
    aggregate <- makeOutputAggregate(model, pos = hyper$age$scaleError@first + 1L,
                                     nChain = 2L, nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with PoissonVaryingUseExp - AgFun", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    lengthValues <- demest:::lengthValues
    SkeletonMu <- demest:::SkeletonMu
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgFun(value = value, sd = sqrt(abs(value)) + 0.1,
                       FUN = function(x, weights) sum(x * weights) / sum(weights))
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 21L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(rate = new("SkeletonManyValues",
                       first = 21L,
                       last = 40L,
                       metadata = metadata),
                       jumpRate = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 41L,
                       iFirstInChain = c(1L, 11L)),
                       acceptRate = new("SkeletonNAccept", nAttempt = 20L, first = 42L,
                       iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 43L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 43L),
                  sex = new("SkeletonBetaTerm",
                      first = 44L,
                      last = 45L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 46L,
                      last = 55L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 56L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 57L))
    aggregate <- makeOutputAggregate(model, pos = hyper$age$scaleError@first + 1L,
                                     nChain = 2L, nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputModel works with PoissonVaryingUseExp - AgPoisson", {
    makeOutputModel <- demest:::makeOutputModel
    makeOutputPrior <- demest:::makeOutputPrior
    makeOutputAggregate <- demest:::makeOutputAggregate
    initialModel <- demest:::initialModel
    lengthValues <- demest:::lengthValues
    SkeletonMu <- demest:::SkeletonMu
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgPoisson(value)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    metadata <- y@metadata
    pos <- 21L
    ans.obtained <- makeOutputModel(model = model, pos = pos,
                                    mcmc = c(nChain = 2L, nIteration = 20L))
    likelihood <- list(rate = new("SkeletonManyValues",
                       first = 21L,
                       last = 40L,
                       metadata = metadata),
                       jumpRate = model@scaleTheta@.Data,
                       noProposal = new("SkeletonNAccept", nAttempt = 20L, first = 41L,
                       iFirstInChain = c(1L, 11L)),
                       acceptRate = new("SkeletonNAccept", nAttempt = 20L, first = 42L,
                       iFirstInChain = c(1L, 11L)))
    mu <- SkeletonMu(betas = model@betas,
                     margins = model@margins,
                     first = 43L,
                     metadata = model@metadataY)
    betas <- list("(Intercept)" = new("SkeletonBetaIntercept",
                      first = 43L),
                  sex = new("SkeletonBetaTerm",
                      first = 44L,
                      last = 45L,
                      metadata = metadata[1]),
                  age = new("SkeletonBetaTerm",
                      first = 46L,
                      last = 55L,
                      metadata = metadata[2]))
    sigma <- new("SkeletonOneValues", first = 56L)
    prior <- c(betas, list(mean = mu), list(sd = sigma))
    hyper <- list("(Intercept)" = list(scaleError = model@priorsBetas[[1]]@tau@.Data),
                  sex = list(scaleError = model@priorsBetas[[2]]@tau@.Data),
                  age = makeOutputPrior(prior = model@priorsBetas[[3]],
                      metadata = model@metadataY[2],
                      pos = 57L))
    aggregate <- makeOutputAggregate(model, pos = hyper$age$scaleError@first + 1L,
                                     nChain = 2L, nIteration = 20L)
    ans.expected <- list(likelihood = likelihood,
                         prior = prior,
                         hyper = hyper,
                         aggregate = aggregate)
    expect_identical(ans.obtained, ans.expected)
})


## Poisson-binomial mixture

test_that("makeOutputModel works with PoissonBinomialMixture", {
    makeOutputModel <- demest:::makeOutputModel
    y <- Counts(array(rpois(20, lambda  = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    metadata <- y@metadata
    pos <- 10L
    model <- new("PoissonBinomialMixture", prob = 0.98)
    ans.obtained <- makeOutputModel(model = model, pos = pos)
    ans.expected <- list(prob = 0.98)
})


## NormalFixed

test_that("makeOutputModel works with NormalFixed", {
    initialModel <- demest:::initialModel
    makeOutputModel <- demest:::makeOutputModel
    y <- Counts(array(rpois(20, lambda  = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y, exposure = NULL)
    metadata <- y@metadata
    pos <- 10L
    ans.obtained <- makeOutputModel(model = model, pos = pos)
    ans.expected <- list(mean = mean, sd = sd)
})




## predictModelNotUseExp ##############################################################

test_that("predictModelNotUseExp gives valid answer with NormalVaryingVarsigmKnownPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(rnorm(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.double(NA),
                              dim = c(10, 4),
                              dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region, sd = 1.1))
        model <- initialModel(spec, y = y.est, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10L,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictModelNotUseExp(model, y = y.pred)
        set.seed(seed + 1)
        ans.expected <- predictPriorsBetas(model)
        ans.expected <- predictBetas(ans.expected)
        ans.expected <- updateTheta_NormalVarying(ans.expected, y = y.pred)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R, generic C, and specific C versions predictModelNotUseExp give same answer with NormalVaryingVarsigmKnownPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(rnorm(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.double(NA),
                              dim = c(10, 4),
                              dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region, sd = 1.1))
        model <- initialModel(spec, y = y.est, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10L,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictModelNotUseExp(model, y = y.pred,
                                       useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- predictModelNotUseExp(model, y = y.pred,
                                                useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- predictModelNotUseExp(model, y = y.pred,
                                               useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("predictModelNotUseExp gives valid answer with NormalVaryingVarsigmUnknownPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(rnorm(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.double(NA),
                              dim = c(10, 4),
                              dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y.est, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10L,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictModelNotUseExp(model, y = y.pred)
        set.seed(seed + 1)
        ans.expected <- predictPriorsBetas(model)
        ans.expected <- predictBetas(ans.expected)
        ans.expected <- updateTheta_NormalVarying(ans.expected, y = y.pred)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R, generic C, and specific C versions predictModelNotUseExp give same answer with NormalVaryingVarsigmUnknownPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(rnorm(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.double(NA),
                              dim = c(10, 4),
                              dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y.est, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10L,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictModelNotUseExp(model, y = y.pred,
                                       useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- predictModelNotUseExp(model, y = y.pred,
                                                useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- predictModelNotUseExp(model, y = y.pred,
                                               useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("predictModelNotUseExp gives valid answer with PoissonVaryingNotUseExpPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    updateTheta_PoissonVaryingNotUseExp <- demest:::updateTheta_PoissonVaryingNotUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = NULL)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictModelNotUseExp(model, y = y.pred)
        set.seed(seed + 1)
        ans.expected <- predictPriorsBetas(model)
        ans.expected <- predictBetas(ans.expected)
        ans.expected <- updateTheta_PoissonVaryingNotUseExp(ans.expected, y = y.pred)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R, C-specific, and C-generic methods for predictModelNotUseExp give same answer with PoissonVaryingNotUseExpPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = NULL)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictModelNotUseExp(model, y = y.pred,
                                    useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- predictModelNotUseExp(model, y = y.pred,
                                             useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- predictModelNotUseExp(model, y = y.pred,
                                            useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("predictModelNotUseExp gives valid answer with NormalFixed", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = NULL)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.obtained <- predictModelNotUseExp(model, y = y.pred)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
})


test_that("R, C-specific, and C-generic methods for predictModelNotUseExp give same answer with NormalFixedPredict", {
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = NULL)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.R <- predictModelNotUseExp(model, y = y.pred,
                                   useC = FALSE)
    ans.C.specific <- predictModelNotUseExp(model, y = y.pred,
                                            useC = TRUE, useSpecific = TRUE)
    ans.C.generic <- predictModelNotUseExp(model, y = y.pred,
                                           useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})




## predictModelUseExp #################################################################

test_that("predictModelUseExp gives valid answer with BinomialVaryingPredict", {
    predictModelUseExp <- demest:::predictModelUseExp
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    updateTheta_BinomialVarying <- demest:::updateTheta_BinomialVarying
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.integer(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred)
        set.seed(seed + 1)
        ans.expected <- predictPriorsBetas(model)
        ans.expected <- predictBetas(ans.expected)
        ans.expected <- updateTheta_BinomialVarying(ans.expected, y = y.pred, exposure = exposure.pred)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R, C-specific, and C-generic methods for predictModelUseExp give same answer with BinomialVaryingPredict", {
    predictModelUseExp <- demest:::predictModelUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.integer(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                    useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                             useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                            useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("predictModelUseExp gives valid answer with PoissonVaryingUseExpPredict", {
    predictModelUseExp <- demest:::predictModelUseExp
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictBetas <- demest:::predictBetas
    updateTheta_PoissonVaryingUseExp <- demest:::updateTheta_PoissonVaryingUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred)
        set.seed(seed + 1)
        ans.expected <- predictPriorsBetas(model)
        ans.expected <- predictBetas(ans.expected)
        ans.expected <- updateTheta_PoissonVaryingUseExp(ans.expected, y = y.pred, exposure = exposure.pred)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R, C-specific, and C-generic methods for predictModelUseExp give same answer with PoissonVaryingUseExpPredict", {
    predictModelUseExp <- demest:::predictModelUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                    useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                             useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                            useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("predictModelUseExp method for PoissonBinomialMixture works", {
    predictModelUseExp <- demest:::predictModelUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = letters[1:4])))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(10, 4),
                                  dimnames = list(age = 5:14, region = letters[1:4])))
    y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(10, 4),
                           dimnames = list(age = 5:14, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 1L,
                                 labels = NULL,
                                 n = 10,
                                 offsetModel = 1L)
    ans <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred)
    expect_identical(ans, model)
})

test_that("R, generic C, and specific C versions predictModelUseExp method for PoissonBinomialMixture give same answer", {
    predictModelUseExp <- demest:::predictModelUseExp
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = letters[1:4])))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(10, 4),
                                  dimnames = list(age = 5:14, region = letters[1:4])))
    y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(10, 4),
                           dimnames = list(age = 5:14, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 1L,
                                 labels = NULL,
                                 n = 10,
                                 offsetModel = 1L)
    ans.R <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                useC = FALSE)
    ans.C.specific <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                         useC = TRUE, useSpecific = TRUE)
    ans.C.generic <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                        useC = TRUE, useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.C.generic, ans.C.specific)
})


test_that("predictModelUseExp gives valid answer with NormalFixed", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    predictModelUseExp <- demest:::predictModelUseExp
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure.est <- Counts(array(rpois(10, lambda  = 10),
                                 dim = c(2, 5),
                                 dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(2, 5),
                                  dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.obtained <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
})


test_that("R, C-specific, and C-generic methods for predictModelUseExp give same answer with NormalFixedPredict", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    predictModelUseExp <- demest:::predictModelUseExp
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure.est <- Counts(array(rpois(10, lambda  = 10),
                                 dim = c(2, 5),
                                 dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(2, 5),
                                  dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.R <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                   useC = FALSE)
    ans.C.specific <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                            useC = TRUE, useSpecific = TRUE)
    ans.C.generic <- predictModelUseExp(model, y = y.pred, exposure = exposure.pred,
                                           useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})



## transferParamModel ################################################################

test_that("transferParamModel gives valid answer with NormalVaryingVarsigmaKnownPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamBetas <- demest:::transferParamBetas
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamSigma <- demest:::transferParamSigma
    set.seed(1)
    weights <- Counts(array(runif(50),
                            dim = c(5, 10),
                            dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rnorm(50),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 2.1))
    x.old <- initialModel(spec, y = y, weights = weights)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.obtained <- transferParamModel(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamBetas(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamPriorsBetas(model = ans.expected,
                                             filename = filename,
                                             lengthIter = lengthIter,
                                             iteration = 1L)
    ans.expected <- transferParamSigma(model = ans.expected,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C-specific, and C-generic versions of transferParamModel give same answer with NormalVaryingVarsigmaKnownPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    set.seed(1)
    weights <- Counts(array(runif(50),
                            dim = c(5, 10),
                            dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rnorm(50),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 2.1))
    x.old <- initialModel(spec, y = y, weights = weights)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.R <- transferParamModel(model = x.new,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L,
                                useC = FALSE)
    ans.C.specific <- transferParamModel(model = x.new,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model = x.new,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.R, ans.C.generic)
})

test_that("transferParamModel gives valid answer with NormalVaryingVarsigmaUnknownPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamBetas <- demest:::transferParamBetas
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamSigma <- demest:::transferParamSigma
    transferParamVarsigma <- demest:::transferParamVarsigma
    set.seed(1)
    weights <- Counts(array(runif(50),
                            dim = c(5, 10),
                            dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rnorm(50),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    x.old <- initialModel(spec, y = y, weights = weights)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.obtained <- transferParamModel(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamBetas(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamPriorsBetas(model = ans.expected,
                                             filename = filename,
                                             lengthIter = lengthIter,
                                             iteration = 1L)
    ans.expected <- transferParamSigma(model = ans.expected,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamVarsigma(model = ans.expected,
                                          filename = filename,
                                          lengthIter = lengthIter,
                                          iteration = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C-specific, and C-generic versions of transferParamModel give same answer with NormalVaryingVarsigmaUnknownPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    set.seed(1)
    weights <- Counts(array(runif(50),
                            dim = c(5, 10),
                            dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rnorm(50),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    x.old <- initialModel(spec, y = y, weights = weights)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.R <- transferParamModel(model = x.new,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L,
                                useC = FALSE)
    ans.C.specific <- transferParamModel(model = x.new,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model = x.new,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.R, ans.C.generic)
})

test_that("transferParamModel gives valid answer with PoissonVaryingNotUseExpPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamBetas <- demest:::transferParamBetas
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamSigma <- demest:::transferParamSigma
    transferParamVarsigma <- demest:::transferParamVarsigma
    set.seed(1)
    y <- Counts(array(as.integer(rpois(50, lambda = 20)),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x.old <- initialModel(spec, y = y, exposure = NULL)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.obtained <- transferParamModel(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamBetas(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamPriorsBetas(model = ans.expected,
                                             filename = filename,
                                             lengthIter = lengthIter,
                                             iteration = 1L)
    ans.expected <- transferParamSigma(model = ans.expected,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C-specific, and C-generic versions of transferParamModel give same answer with PoissonVaryingNotUseExpPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    set.seed(1)
    y <- Counts(array(as.integer(rpois(50, lambda = 20)),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x.old <- initialModel(spec, y = y, exposure = NULL)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.R <- transferParamModel(model = x.new,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L,
                                useC = FALSE)
    ans.C.specific <- transferParamModel(model = x.new,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model = x.new,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.R, ans.C.generic)
})

test_that("transferParamModel gives valid answer with BinomialVaryingPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamBetas <- demest:::transferParamBetas
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamSigma <- demest:::transferParamSigma
    set.seed(1)
    exposure <- Counts(array(rpois(50, lambda = 10),
                             dim = c(5, 10),
                             dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rbinom(50, size = exposure, prob = 0.4),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  age ~ Exch(),
                  region ~ Exch(error = Error(robust = TRUE)))
    x.old <- initialModel(spec, y = y, exposure = exposure)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.obtained <- transferParamModel(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamBetas(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamPriorsBetas(model = ans.expected,
                                             filename = filename,
                                             lengthIter = lengthIter,
                                             iteration = 1L)
    ans.expected <- transferParamSigma(model = ans.expected,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of transferParamModel give same answer with BinomialVaryingPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    set.seed(1)
    exposure <- Counts(array(rpois(50, lambda = 10),
                             dim = c(5, 10),
                             dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(rbinom(50, size = exposure, prob = 0.4),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  age ~ Exch(),
                  region ~ Exch(error = Error(robust = TRUE)))
    x.old <- initialModel(spec, y = y, exposure = exposure)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.R <- transferParamModel(model = x.new,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L,
                                useC = FALSE)
    ans.C.specific <- transferParamModel(model = x.new,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model = x.new,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.R, ans.C.generic)
})

test_that("transferParamModel gives valid answer with PoissonVaryingUseExpPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamBetas <- demest:::transferParamBetas
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamSigma <- demest:::transferParamSigma
    transferParamVarsigma <- demest:::transferParamVarsigma
    set.seed(1)
    exposure <- Counts(array(rpois(50, lambda = 10),
                             dim = c(5, 10),
                             dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(as.integer(rpois(50, lambda = 0.4 * exposure)),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x.old <- initialModel(spec, y = y, exposure = exposure)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.obtained <- transferParamModel(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamBetas(model = x.new,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    ans.expected <- transferParamPriorsBetas(model = ans.expected,
                                             filename = filename,
                                             lengthIter = lengthIter,
                                             iteration = 1L)
    ans.expected <- transferParamSigma(model = ans.expected,
                                       filename = filename,
                                       lengthIter = lengthIter,
                                       iteration = 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C-specific, and C-generic versions of transferParamModel give same answer with PoissonVaryingUseExpPredict", {
    transferParamModel <- demest:::transferParamModel
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    set.seed(1)
    exposure <- Counts(array(rpois(50, lambda = 10),
                             dim = c(5, 10),
                             dimnames = list(age = 0:4, region = letters[1:10])))
    y <- Counts(array(as.integer(rpois(50, lambda = 0.4 * exposure)),
                      dim = c(5, 10),
                      dimnames = list(age = 0:4, region = letters[1:10])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x.old <- initialModel(spec, y = y, exposure = exposure)
    x.new <- initialModelPredict(x.old,
                                 along = 2L,
                                 labels = letters[11:14],
                                 n = NULL,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    values <- extractValues(x.old)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(values, con = con)
    close(con)
    ans.R <- transferParamModel(model = x.new,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L,
                                useC = FALSE)
    ans.C.specific <- transferParamModel(model = x.new,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model = x.new,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.R, ans.C.generic)
})

test_that("transferParamModel method for PoissonBinomialMixturePredict works", {
    transferParamModel <- demest:::transferParamModel
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = letters[1:4])))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(10, 4),
                                  dimnames = list(age = 5:14, region = letters[1:4])))
    y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(10, 4),
                           dimnames = list(age = 5:14, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 1L,
                                 labels = NULL,
                                 n = 10,
                                 offsetModel = 1L)
    ans <- transferParamModel(model,
                              filename = "file",
                              lengthIter = 100L,
                              iteration = 1L)                              
    expect_identical(ans, model)
})

test_that("R, generic C, and specific C versions transferParamModel method for PoissonBinomialMixturePredict give same answer", {
    transferParamModel <- demest:::transferParamModel
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    exposure.est <- Counts(array(as.integer(rpois(n = 20, lambda = 50)),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = letters[1:4])))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(10, 4),
                                  dimnames = list(age = 5:14, region = letters[1:4])))
    y.est <- Counts(array(as.integer(rbinom(n = 20, size = exposure.est, prob = 0.5)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(10, 4),
                           dimnames = list(age = 5:14, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 1L,
                                 labels = NULL,
                                 n = 10,
                                 offsetModel = 1L)
    ans.R <- transferParamModel(model,
                                filename = "file",
                                lengthIter = 100L,
                                iteration = 1L,
                                useC = FALSE)                              
    ans.C.specific <- transferParamModel(model,
                                         filename = "file",
                                         lengthIter = 100L,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model,
                                        filename = "file",
                                        lengthIter = 100L,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("transferParamModel gives valid answer with NormalFixedNotUseExpPredict", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamModel <- demest:::transferParamModel
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = NULL)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans <- transferParamModel(model,
                              filename = "file",
                              lengthIter = 100L,
                              iteration = 1L)                              
    expect_identical(ans, model)
})

test_that("R, C-specific, and C-generic methods for transferParamModel give same answer with NormalFixedNotUseExpPredict", {
    transferParamModel <- demest:::transferParamModel
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = NULL)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.R <- transferParamModel(model,
                                filename = "file",
                                lengthIter = 100L,
                                iteration = 1L,
                                useC = FALSE)                              
    ans.C.specific <- transferParamModel(model,
                                         filename = "file",
                                         lengthIter = 100L,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model,
                                        filename = "file",
                                        lengthIter = 100L,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.C.generic, ans.C.specific)
})


test_that("transferParamModel gives valid answer with NormalFixedUseExpPredict", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamModel <- demest:::transferParamModel
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure.est <- Counts(array(rpois(10, lambda  = 10),
                                 dim = c(2, 5),
                                 dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(2, 5),
                                  dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.obtained <- transferParamModel(model,
                                       filename = "file",
                                       lengthIter = 100L,
                                       iteration = 1L)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
})


test_that("R, C-specific, and C-generic methods for transferParamModel give same answer with NormalFixedUseExpPredict", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    transferParamModel <- demest:::transferParamModel
    y.est <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure.est <- Counts(array(rpois(10, lambda  = 10),
                                 dim = c(2, 5),
                                 dimnames = list(sex = c("f", "m"), age = 0:4)))
    y.pred <- Counts(array(as.integer(NA),
                           dim = c(2, 5),
                           dimnames = list(sex = c("f", "m"), age = 5:9)))
    exposure.pred <- Counts(array(as.integer(NA),
                                  dim = c(2, 5),
                                  dimnames = list(sex = c("f", "m"), age = 5:9)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y.est, exposure = exposure.est)
    model <- initialModelPredict(model,
                                 along = 2L,
                                 labels = NULL,
                                 n = 5,
                                 offsetModel = 1L,
                                 covariates = NULL,
                                 aggregate = NULL,
                                 lower = NULL,
                                 upper = NULL)
    ans.R <- transferParamModel(model,
                                filename = "file",
                                lengthIter = 100L,
                                iteration = 1L,
                                useC = FALSE)                              
    ans.C.specific <- transferParamModel(model,
                                         filename = "file",
                                         lengthIter = 100L,
                                         iteration = 1L,
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamModel(model,
                                        filename = "file",
                                        lengthIter = 100L,
                                        iteration = 1L,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.C.generic, ans.C.specific)
})


## updateModelNotUseExp ##############################################################

## Only test that appropriate slots are updated.  Check that values are correct in
## the tests for the 'updateTheta' etc
test_that("updateModelNotUseExp for NormalVaryingVarsigmaKnown updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(rbeta(n = 20, shape1 = 5, shape2 = 5),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.1))
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_true(all(x1@theta != x0@theta))
        expect_true(x1@sigma != x0@sigma)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("w", "slotsToExtract", "iMethodModel", "namesBetas",
                       "varsigma", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

## tests equal but not identical
test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaKnown give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(rbeta(n = 20, shape1 = 5, shape2 = 5),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.1),
                      age ~ Exch())                      
        x <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.R <- updateModelNotUseExp(x, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C.generic <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

## Only test that appropriate slots are updated.  Check that values are correct in
## the tests for the 'updateTheta' etc
test_that("updateModelNotUseExp for NormalVaryingVarsigmaUnknown updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(rbeta(n = 20, shape1 = 5, shape2 = 5),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region),
                      age ~ Exch())                      
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_true(all(x1@theta != x0@theta))
        expect_true(x1@sigma != x0@sigma)
        expect_true(x1@varsigma != x0@varsigma)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("w", "slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

## tests equal but not identical
test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaUnknown give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(rbeta(n = 20, shape1 = 5, shape2 = 5),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region),
                      age ~ Exch())                      
        x <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.R <- updateModelNotUseExp(x, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C.generic <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
        for (i in 1:3) {
            set.seed(seed + i)
            ans.R <- updateModelNotUseExp(ans.R, y = y, useC = FALSE)
            set.seed(seed + i)
            ans.C.specific <- updateModelNotUseExp(ans.C.specific, y = y, useC = TRUE, useSpecific = TRUE)
            expect_equal(ans.R, ans.C.specific)
        }
    }
})

## Only test that appropriate slots are updated.  Check that values are correct in
## the tests for the 'updateTheta' etc
test_that("updateModelNotUseExp for PoissonVaryingNotUseExp updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region),
                      age ~ Exch())                      
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_identical(sum(x1@theta != x0@theta), x1@nAcceptTheta@.Data)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

## tests equal but not identical
test_that("R, generic C, and specific C versions updateModelNotUseExp method for PoissonVaryingNotUseExp give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region),
                      age ~ Exch())                      
        x <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateModelNotUseExp(x, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C.generic <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateModelNotUseExp(x, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})


## updateModelUseExp for NormalVaryingVarsigmaKnownAgCertain

test_that("updateModelNotUseExp for NormalVaryingVarsigmaKnownAgCertain updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(2, sd = 3), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgCertain(value = value)
        y <- Values(array(rnorm(n = 20, sd = 3),
                          dim = c(2, 20),
                          dimnames = list(sex = c("f", "m"), age = 0:19)))
        weights <- Counts(array(1,
                                dim = c(2, 20),
                                dimnames = list(sex = c("f", "m"), age = 0:19)))
        spec <- Model(y ~ Normal(mean ~ age, sd = 1), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("no theta updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaKnownAgCertain give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(n = 3), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex, sd = 0.5), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for NormalVaryingVarsigmaUnknownAgCertain

test_that("updateModelNotUseExp for NormalVaryingVarsigmaUnknownAgCertain updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(n = 3), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("no theta updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas",  "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaUnknownAgCertain give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(n = 3), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelNotUseExp for PoissonVaryingNotUseExpAgCertain

test_that("updateModelNotUseExp for PoissonVaryingNotUseExpAgCertain updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(10, dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        y <- as.integer(rpois(n = 20, lambda = 5))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for PoissonVaryingNotUseExpAgCertain give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rpois(n = 3, lambda = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        y <- as.integer(rpois(n = 20, lambda = 10))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


## updateModelNotUseExp for NormalVaryingVarsigmaKnownAgNormal

test_that("updateModelNotUseExp for NormalVaryingVarsigmaKnownAgNormal updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(n = 3), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(abs(value)))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex, sd = 0.5), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("'theta' not updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaKnownAgNormal give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex, sd = 0.5), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelNotUseExp for NormalVaryingVarsigmaUnknownAgNormal

test_that("updateModelNotUseExp for NormalVaryingVarsigmaUnknownAgNormal updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("'theta' not updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaUnknownAgNormal give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


## updateModelNotUseExp for NormalVaryingVarsigmaKnownAgFun

test_that("updateModelNotUseExp for NormalVaryingVarsigmaKnownAgFun updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rnorm(n = 3), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex, sd = 0.5), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        expect_is(x0, "NormalVaryingVarsigmaKnownAgFun")
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_is(x0, "NormalVaryingVarsigmaKnownAgFun")
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("'theta' not updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaKnownAgFun give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex, sd = 0.5), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        expect_is(x0, "NormalVaryingVarsigmaKnownAgFun")
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelNotUseExp for NormalVaryingVarsigmaUnknownAgFun

test_that("updateModelNotUseExp for NormalVaryingVarsigmaUnknownAgFun updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        expect_is(x0, "NormalVaryingVarsigmaUnknownAgFun")
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_is(x1, "NormalVaryingVarsigmaUnknownAgFun")
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            stop("'theta' not updated")
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalVaryingVarsigmaUnknownAgFun give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- Values(array(rnorm(n = 20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- Counts(array(1, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        expect_is(x0, "NormalVaryingVarsigmaUnknownAgFun")
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for PoissonVaryingNotUseExpAgNormal

test_that("updateModelNotUseExp for PoissonVaryingNotUseExpAgNormal updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(10, dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- as.integer(rpois(n = 20, lambda = 5))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for PoissonVaryingNotUseExpAgNormal give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rpois(n = 3, lambda = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- as.integer(rpois(n = 20, lambda = 10))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for PoissonVaryingNotUseExpAgFun

test_that("updateModelNotUseExp for PoissonVaryingNotUseExpAgFun updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(10, dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- as.integer(rpois(n = 20, lambda = 5))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        expect_is(x0, "PoissonVaryingNotUseExpAgFun")
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        expect_is(x1, "PoissonVaryingNotUseExpAgFun")
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for PoissonVaryingNotUseExpAgFun give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rpois(n = 3, lambda = 5), dim = 3, dimnames = list(age = 0:2))) + 1
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        y <- as.integer(rpois(n = 20, lambda = 10))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        expect_is(x0, "PoissonVaryingNotUseExpAgFun")
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelNotUseExp for PoissonVaryingNotUseExpAgPoisson

test_that("updateModelNotUseExp for PoissonVaryingNotUseExpAgPoisson updates the correct slots", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(10, dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value)
        y <- as.integer(rpois(n = 20, lambda = 5))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for PoissonVaryingNotUseExpAgPoisson give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rpois(n = 3, lambda = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value)
        y <- as.integer(rpois(n = 20, lambda = 10))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateModelNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelNotUseExp(x0, y = y, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

test_that("updateModelNotUseExp for NormalFixedNotUseExp works", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- updateModelNotUseExp(model, y = y, useC = FALSE)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, generic C, and specific C versions updateModelNotUseExp method for NormalFixedNotUseExp give same answer", {
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(10, lambda  = 10),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:4)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.R <- updateModelNotUseExp(model, y = y, useC = FALSE)
    ans.C.generic <- updateModelNotUseExp(model, y = y, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- updateModelNotUseExp(model, y = y, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})



## updateModelUseExp ##################################################################

## Only test that appropriate slots are updated.  Check that values are correct in
## the tests for the 'updateTheta' etc
test_that("updateModelUseExp for BinomialVarying updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(1, 0.2, 0.8)
        exposure <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = mu)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Binomial(mean ~ region))
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        expect_identical(sum(x1@theta != x0@theta), x1@nAcceptTheta@.Data)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

## tests equal but not identical
test_that("R, generic C, and specific C versions updateModelUseExp method for BinomialVarying give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(1, 0.2, 0.8)
        exposure <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = mu)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Binomial(mean ~ region))
        x <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateModelUseExp(x, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C.generic <- updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

## Only test that appropriate slots are updated.  Check that values are correct in
## the tests for the 'updateTheta' etc
test_that("updateModelUseExp for PoissonVaryingUseExp updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(1, 0.2, 0.8)
        exposure <- Counts(array(runif(n = 20, max = 10),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = mu * exposure)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ region))
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        expect_identical(sum(x1@theta != x0@theta), x1@nAcceptTheta@.Data)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

## tests equal but not identical
test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExp give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(1, 0.2, 0.8)
        exposure <- Counts(array(runif(n = 20, max = 10),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = mu * exposure)),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ region))
        x <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateModelUseExp(x, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C.generic <- updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

test_that("R, C specific and C generic versions of updateModelUseExp for PoissonBinomialMixture returns object unchanged", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    x <- new("PoissonBinomialMixture", prob = 0.98)
    exposure <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
    y <- Counts(array(as.integer(rpois(n = 20, lambda = exposure)),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
    expect_identical(updateModelUseExp(x, y = y, exposure = exposure, useC = FALSE),
                     x)
    expect_identical(updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE),
                     x)
    expect_identical(updateModelUseExp(x, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE),
                     x)
})

## updateModelUseExp for BinomialVaryingAgCertain

test_that("updateModelUseExp for BinomialVaryingAgCertain updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for BinomialVaryingAgCertain (deterministic) give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for PoissonVaryingUseExpAgCertain

test_that("updateModelUseExp for PoissonVaryingUseExpAgCertain updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        expect_false(identical(x1@sigma, x0@sigma))
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExpAgCertain give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


## updateModelUseExp for BinomialVaryingAgNormal

test_that("updateModelUseExp for BinomialVaryingAgNormal updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for BinomialVaryingAgNormal give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for BinomialVaryingAgFun

test_that("updateModelUseExp for BinomialVaryingAgFun updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "BinomialVaryingAgFun")
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        expect_is(x0, "BinomialVaryingAgFun")
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for BinomialVaryingAgFun give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


## updateModelUseExp for PoissonVaryingUseExpAgNormal

test_that("updateModelUseExp for PoissonVaryingUseExpAgNormal updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        expect_false(identical(x1@sigma, x0@sigma))
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExpAgNormal give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})

## updateModelUseExp for PoissonVaryingUseExpAgFun

test_that("updateModelUseExp for PoissonVaryingUseExpAgFun updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(10, dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "PoissonVaryingUseExpAgFun")
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        expect_is(x1, "PoissonVaryingUseExpAgFun")
        if (x1@nAcceptTheta > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExpAgFun give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rpois(n = 3, lambda = 5), dim = 3, dimnames = list(age = 0:2))) + 1
        aggregate <- AgFun(value = value, sd = sqrt(abs(value)),
                           FUN = function(x, weights) sum(x * weights) / sum(weights))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20)) + 1
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "PoissonVaryingUseExpAgFun")
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure,
                                         useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure,
                                          useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


## updateModelUseExp for PoissonVaryingUseExpAgLife

test_that("updateModelUseExp for PoissonVaryingUseExpAgLife updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        exposure <- as.double(rpois(n = 20, lambda = 20)) + 1
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "PoissonVaryingUseExpAgLife")
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        expect_is(x1, "PoissonVaryingUseExpAgLife")
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        expect_false(identical(x1@sigma, x0@sigma))
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExpAgLife give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        exposure <- as.double(rpois(n = 20, lambda = 20)) + 1
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})



## updateModelUseExp for PoissonVaryingUseExpAgPoisson

test_that("updateModelUseExp for PoissonVaryingUseExpAgPoisson updates the correct slots", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        if (x1@nAcceptTheta > 0L || x1@nAcceptBench > 0L)
            expect_false(identical(x1@theta, x0@theta))
        else
            expect_identical(x1@theta, x0@theta)
        expect_false(identical(x1@sigma, x0@sigma))
        for (b in seq_along(x1@betas)) {
            expect_false(identical(x1@betas[[b]], x0@betas[[b]]))
            if (!is(x1@priorsBetas[[b]], "ExchFixed"))
                expect_false(identical(x1@priorsBetas[[b]], x0@priorsBetas[[b]]))
        }
        for (name in c("slotsToExtract", "iMethodModel", "namesBetas",
                       "scaleTheta", "iteratorBetas", "dims"))
            expect_identical(slot(x1, name), slot(x0, name))
    }
})

test_that("R, generic C, and specific C versions updateModelUseExp method for PoissonVaryingUseExpAgPoisson give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateModelUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C.generic <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = FALSE)
        set.seed(seed + 1)
        x.C.specific <- updateModelUseExp(x0, y = y, exposure = exposure, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C.generic)
        else
            expect_equal(x.R, x.C.generic)
        expect_identical(x.C.generic, x.C.specific)
    }
})


test_that("updateModelUseExp for NormalFixedUseExp works", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- updateModelUseExp(model, y = y, exposure = exposure, useC = FALSE)
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, generic C, and specific C versions updateModelUseExp method for NormalFixedUseExp give same answer", {
    updateModelUseExp <- demest:::updateModelUseExp
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(10, lambda  = 10),
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:4)))
    exposure <- Counts(array(rpois(10, lambda  = 10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("f", "m"), age = 0:4)))
    mean <- Values(array(rpois(20, lambda  = 10),
                         dim = c(2, 10),
                         dimnames = list(sex = c("f", "m"), age = 0:9)))
    sd <- Values(array(runif(20),
                       dim = c(2, 10),
                       dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sd))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.R <- updateModelUseExp(model, y = y, exposure = exposure, useC = FALSE)
    ans.C.generic <- updateModelUseExp(model, y = y, exposure = exposure,
                                       useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- updateModelUseExp(model, y = y, exposure = exposure,
                                           useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})




## whereAcceptance ###################################################################

test_that("whereAcceptance works", {
    whereAcceptance <- demest:::whereAcceptance
    x <- new("NormalVaryingVarsigmaUnknown")
    expect_identical(whereAcceptance(x), list(NULL))
    x <- new("NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptMean")))
    x <- new("NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptMean")))
    x <- new("NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptMean"),
                          c("aggregate", "accept")))
    x <- new("NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptMean"),
                          c("aggregate", "accept")))
    x <- new("BinomialVarying")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptProb")))
    x <- new("BinomialVaryingAgCertain")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptProb")))
    x <- new("BinomialVaryingAgNormal")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptProb"),
                          c("aggregate", "accept")))
    x <- new("PoissonVaryingNotUseExp")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptCount")))
    x <- new("PoissonVaryingUseExp")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptRate")))
    x <- new("PoissonVaryingNotUseExpAgCertain")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptCount")))
    x <- new("PoissonVaryingUseExpAgCertain")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptRate")))
    x <- new("PoissonVaryingNotUseExpAgNormal")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptCount"),
                          c("aggregate", "accept")))
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptRate"),
                          c("aggregate", "accept")))
    x <- new("PoissonVaryingNotUseExpAgPoisson")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptCount"),
                          c("aggregate", "accept")))
    x <- new("PoissonVaryingUseExpAgPoisson")
    expect_identical(whereAcceptance(x),
                     list(c("likelihood", "acceptRate"),
                          c("aggregate", "accept")))
    x <- new("PoissonBinomialMixture")
    expect_identical(whereAcceptance(x), list(NULL))
    x <- new("BinomialVaryingPredict")
    expect_identical(whereAcceptance(x),
                     list(NULL))
    x <- new("PoissonVaryingUseExpPredict")
    expect_identical(whereAcceptance(x),
                     list(NULL))
    x <- new("NormalFixedNotUseExp")
    expect_identical(whereAcceptance(x),
                     list(NULL))
})


## whereAutocorr #####################################################################

test_that("whereAutocorr works", {
    whereAutocorr <- demest:::whereAutocorr
    x <- new("NormalVaryingVarsigmaUnknown")
    expect_identical(whereAutocorr(x), list(NULL))
    x <- new("NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(whereAutocorr(x), list(c("likelihood", "mean")))
    x <- new("NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(whereAutocorr(x), list(c("likelihood", "mean")))
    x <- new("NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(whereAutocorr(x),
                     list(c("likelihood", "mean"),
                          c("aggregate", "value")))
    x <- new("NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(whereAutocorr(x),
                     list(c("likelihood", "mean"),
                          c("aggregate", "value")))
    x <- new("BinomialVarying")
    expect_identical(whereAutocorr(x), list(c("likelihood", "prob")))
    x <- new("BinomialVaryingAgCertain")
    expect_identical(whereAutocorr(x), list(c("likelihood", "prob")))
    x <- new("BinomialVaryingAgNormal")
    expect_identical(whereAutocorr(x), list(c("likelihood", "prob"),
                                              c("aggregate", "value")))
    x <- new("PoissonVaryingUseExp")
    expect_identical(whereAutocorr(x), list(c("likelihood", "rate")))
    x <- new("PoissonVaryingNotUseExp")
    expect_identical(whereAutocorr(x), list(c("likelihood", "count")))
    x <- new("PoissonVaryingUseExpAgCertain")
    expect_identical(whereAutocorr(x), list(c("likelihood", "rate")))
    x <- new("PoissonVaryingNotUseExpAgCertain")
    expect_identical(whereAutocorr(x), list(c("likelihood", "count")))
    x <- new("PoissonVaryingNotUseExpAgNormal")
    expect_identical(whereAutocorr(x), list(c("likelihood", "count"),
                                              c("aggregate", "value")))
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(whereAutocorr(x), list(c("likelihood", "rate"),
                                              c("aggregate", "value")))
    x <- new("PoissonVaryingNotUseExpAgPoisson")
    expect_identical(whereAutocorr(x), list(c("likelihood", "count"),
                                              c("aggregate", "value")))
    x <- new("PoissonVaryingUseExpAgPoisson")
    expect_identical(whereAutocorr(x), list(c("likelihood", "rate"),
                                              c("aggregate", "value")))
    x <- new("PoissonBinomialMixture")
    expect_identical(whereAutocorr(x), list(NULL))
    x <- new("BinomialVaryingPredict")
    expect_identical(whereAutocorr(x), list(NULL))
    x <- new("PoissonVaryingUseExpPredict")
    expect_identical(whereAutocorr(x), list(NULL))
        x <- new("NormalFixedNotUseExp")
    expect_identical(whereAutocorr(x),
                     list(NULL))
})


## whereJump #########################################################################

test_that("whereJump works", {
    whereJump <- demest:::whereJump
    x <- new("NormalVaryingVarsigmaKnown")
    expect_identical(whereJump(x), list(NULL))
    x <- new("NormalVaryingVarsigmaUnknown")
    expect_identical(whereJump(x), list(NULL))
    x <- new("NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(whereJump(x),
                     list(c("likelihood", "jumpMean")))
    x <- new("NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(whereJump(x),
                     list(c("likelihood", "jumpMean")))
    x <- new("NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(whereJump(x),
                     list(c("likelihood", "jumpMean"),
                          c("aggregate", "jump")))
    x <- new("NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(whereJump(x),
                     list(c("likelihood", "jumpMean"),
                          c("aggregate", "jump")))
    x <- new("BinomialVarying")
    expect_identical(whereJump(x), list(c("likelihood", "jumpProb")))
    x <- new("BinomialVaryingAgCertain")
    expect_identical(whereJump(x), list(c("likelihood", "jumpProb")))
    x <- new("BinomialVaryingAgNormal")
    expect_identical(whereJump(x), list(c("likelihood", "jumpProb"),
                                        c("aggregate", "jump")))
    x <- new("PoissonVaryingUseExp")
    expect_identical(whereJump(x), list(c("likelihood", "jumpRate")))
    x <- new("PoissonVaryingNotUseExp")
    expect_identical(whereJump(x), list(c("likelihood", "jumpCount")))
    x <- new("PoissonVaryingUseExpAgCertain")
    expect_identical(whereJump(x), list(c("likelihood", "jumpRate")))
    x <- new("PoissonVaryingNotUseExpAgCertain")
    expect_identical(whereJump(x), list(c("likelihood", "jumpCount")))
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(whereJump(x), list(c("likelihood", "jumpRate"),
                                        c("aggregate", "jump")))
    x <- new("PoissonVaryingNotUseExpAgNormal")
    expect_identical(whereJump(x), list(c("likelihood", "jumpCount"),
                                        c("aggregate", "jump")))
    x <- new("PoissonVaryingUseExpAgPoisson")
    expect_identical(whereJump(x), list(c("likelihood", "jumpRate"),
                                        c("aggregate", "jump")))
    x <- new("PoissonVaryingNotUseExpAgPoisson")
    expect_identical(whereJump(x), list(c("likelihood", "jumpCount"),
                                        c("aggregate", "jump")))
    x <- new("PoissonBinomialMixture")
    expect_identical(whereJump(x), list(NULL))
    x <- new("BinomialVaryingPredict")
    expect_identical(whereJump(x), list(NULL))
    x <- new("PoissonVaryingUseExpPredict")
    expect_identical(whereJump(x), list(NULL))
    x <- new("NormalFixedNotUseExp")
    expect_identical(whereJump(x),
                     list(NULL))
})


## whereEstimated ####################################################################

test_that("whereEstimated works with NormalVaryingVarsigmaKnown", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## is not saturated
    spec <- Model(y ~ Normal(mean ~ sex + age, sd = 2),
                  age ~ Exch())
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    ## is saturated
    spec <- Model(y ~ Normal(mean ~ sex * age, sd = 2),
                  age ~ Exch())
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sex:age"),
                         c("hyper", "age", "scaleError"),
                         c("hyper", "sex:age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with NormalVaryingVarsigmaUnknown", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## is not saturated
    spec <- Model(y ~ Normal(mean ~ sex + age),
                  sex ~ Zero(),
                  age ~ DLM())
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("likelihood", "sd"),
                         c("prior", "(Intercept)"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "trend"),
                         c("hyper", "age", "scaleTrend"),
                         c("hyper", "age", "damp"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    ## is saturated
    spec <- Model(y ~ Normal(mean ~ sex * age),
                  sex ~ Zero(),
                  age ~ DLM())
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("likelihood", "sd"),
                         c("prior", "(Intercept)"),
                         c("prior", "age"),
                         c("prior", "sex:age"),                         
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "trend"),
                         c("hyper", "age", "scaleTrend"),
                         c("hyper", "age", "damp"),
                         c("hyper", "age", "scaleError"),
                         c("hyper", "sex:age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with NormalVaryingVarsigmaKnownAgNormal", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    aggregate <- AgNormal(value = 0, sd = 1)
    ## is not saturated
    spec <- Model(y ~ Normal(mean ~ sex + age, sd = 2),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
    ## is saturated
    spec <- Model(y ~ Normal(mean ~ sex * age, sd = 2),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sex:age"),
                         c("hyper", "age", "scaleError"),
                         c("hyper", "sex:age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with NormalVaryingVarsigmaKnownAgNormal", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Values(array(rnorm(20),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    weights <- Counts(array(1,
                            dim = c(2, 10),
                            dimnames = list(sex = c("f", "m"), age = 0:9)))
    aggregate <- AgNormal(value = 0, sd = 1)
    spec <- Model(y ~ Normal(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "mean"),
                         c("likelihood", "sd"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with BinomialVarying", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## is not saturated
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ DLM(trend = NULL))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "damp"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    spec <- Model(y ~ Binomial(mean ~ 1))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sd"))
    expect_identical(ans.obtained, ans.expected)
    ## is saturated
    spec <- Model(y ~ Binomial(mean ~ sex * age),
                  age ~ DLM(trend = NULL))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sex:age"),
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "damp"),
                         c("hyper", "age", "scaleError"),
                         c("hyper", "sex:age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    ## intercept only
    spec <- Model(y ~ Binomial(mean ~ 1))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sd"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with BinomialVaryingAgCertain", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgCertain(mean)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with BinomialVaryingAgNormal", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(mean) + 0.1
    aggregate <- AgNormal(mean, sd = sd)
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "prob"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingNotUseExp", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(20, lambda  = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ## is not saturated
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ DLM(damp = NULL))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "count"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "trend"),
                         c("hyper", "age", "scaleTrend"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    ## is saturated
    spec <- Model(y ~ Poisson(mean ~ sex * age),
                  age ~ DLM(damp = NULL))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "count"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sex:age"),
                         c("hyper", "age", "level"),
                         c("hyper", "age", "scaleLevel"),
                         c("hyper", "age", "trend"),
                         c("hyper", "age", "scaleTrend"),
                         c("hyper", "age", "scaleError"),
                         c("hyper", "sex:age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingUseExpAgCertain", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgCertain(mean)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "rate"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingNotUseExpAgNormal", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    y <- Counts(array(rbinom(20, size = 20, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    value <- collapseDimension(y, dimension = "age") 
    sd <- sqrt(value) + 0.1
    aggregate <- AgNormal(value = value, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "count"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingUseExpAgPoisson", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgPoisson(value = mean)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "rate"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingUseExpAgNormal", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    sd <- sqrt(mean) + 0.1
    aggregate <- AgNormal(value = mean, sd = sd)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "rate"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("whereEstimated works with PoissonVaryingUseExpAgPoisson", {
    whereEstimated <- demest:::whereEstimated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.double(rpois(20, lambda  = 10)),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    mean <- collapseDimension(y, dimension = "age") / collapseDimension(exposure, dimension = "age")
    aggregate <- AgPoisson(value = mean)
    spec <- Model(y ~ Poisson(mean ~ sex + age),
                  age ~ Exch(),
                  aggregate = aggregate)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(c("likelihood", "rate"),
                         c("prior", "(Intercept)"),
                         c("prior", "sex"),
                         c("prior", "age"),
                         c("prior", "sd"),
                         c("hyper", "age", "scaleError"),
                         c("aggregate", "value"))
    expect_identical(ans.obtained, ans.expected)
})


## Poisson-binomial mixture

test_that("whereEstimated works with PoissonBinomialMixture", {
    whereEstimated <- demest:::whereEstimated
    model <- new("PoissonBinomialMixture", prob = 0.98)
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
})

## NormalFixed

test_that("whereEstimated works with NormalFixed", {
    whereEstimated <- demest:::whereEstimated
    model <- new("NormalFixedNotUseExp")
    ans.obtained <- whereEstimated(model)
    ans.expected <- list(NULL)
    expect_identical(ans.obtained, ans.expected)
})


## whereNoProposal ####################################################################

test_that("whereNoProposal works", {
    whereNoProposal <- demest:::whereNoProposal
    x <- new("BinomialVarying")
    x@lower <- -Inf
    x@upper <- Inf
    expect_identical(whereNoProposal(x), list(NULL))
    x <- new("BinomialVarying")
    x@lower <- -Inf
    x@upper <- 100
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("BinomialVaryingAgCertain")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("BinomialVaryingAgNormal")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("PoissonVaryingUseExp")
    x@lower <- -Inf
    x@upper <- Inf
    expect_identical(whereNoProposal(x), list(NULL))
    x <- new("PoissonVaryingUseExp")
    x@lower <- 0
    x@upper <- Inf
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("PoissonVaryingNotUseExp")
    x@lower <- -Inf
    x@upper <- Inf
    expect_identical(whereNoProposal(x), list(NULL))
    x <- new("PoissonVaryingNotUseExp")
    x@lower <- -Inf
    x@upper <- 100
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("PoissonVaryingUseExpAgCertain")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("PoissonVaryingUseExpAgPoisson")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("PoissonVaryingNotUseExpAgCertain")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("PoissonVaryingNotUseExpAgNormal")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("PoissonVaryingNotUseExpAgPoisson")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("NormalVaryingVarsigmaKnown")
    x@lower <- -Inf
    x@upper <- Inf
    expect_identical(whereNoProposal(x), list(NULL))
    x <- new("NormalVaryingVarsigmaKnown")
    x@lower <- -Inf
    x@upper <- 100
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal")))
    x <- new("NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(whereNoProposal(x), list(c("likelihood", "noProposal"),
                                              c("aggregate", "noProposal")))
    x <- new("PoissonBinomialMixture")
    expect_identical(whereNoProposal(x), list(NULL))
    x <- new("NormalFixedNotUseExp")
    expect_identical(whereNoProposal(x),
                     list(NULL))
})


## whereTheta #####################################################################

test_that("whereTheta works", {
    whereTheta <- demest:::whereTheta
    x <- new("NormalVaryingVarsigmaUnknown")
    expect_identical(whereTheta(x), c("likelihood", "mean"))
    x <- new("PoissonVaryingNotUseExp")
    expect_identical(whereTheta(x), c("likelihood", "count"))
    x <- new("PoissonVaryingUseExp")
    expect_identical(whereTheta(x), c("likelihood", "rate"))
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(whereTheta(x), c("likelihood", "rate"))
    x <- new("BinomialVarying")
    expect_identical(whereTheta(x), c("likelihood", "prob"))
    x <- new("BinomialVaryingAgNormal")
    expect_identical(whereTheta(x), c("likelihood", "prob"))
    x <- new("PoissonBinomialMixture")
    expect_error(whereTheta(x), "'object' has class \"PoissonBinomialMixture\"")
    x <- new("NormalFixedNotUseExp")
    expect_error(whereTheta(x), "'object' has class \"NormalFixedNotUseExp\"")
})
