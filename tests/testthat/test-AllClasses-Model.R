
context("AllClasses-Model")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("can create a valid object of class PoissonVaryingNotUseExp", {
    BetaIterator <- demest:::BetaIterator
    ## main effects model
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    expect_true(validObject(x))
    ## version consisting only of intercept
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20) ,
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 3),
             sigmaMax = new("Scale", 11),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5),
             namesBetas = "(Intercept)",
             margins = list(0L),
             priorsBetas = list(new("ExchFixed")),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L)),
             dims = list(0L))
    expect_true(validObject(x))
})

test_that("validity tests for PoissonVaringNotUseExp inherited from MetadataY work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    expect_true(validObject(x))
    ## 'metadataY' does not have any dimensions with dimtype "iteration"
    x.wrong <- x
    x.wrong@metadataY <- new("MetaData",
                             nms = c("age", "iteration"),
                             dimtypes = c("age", "iteration"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                 new("Iterations", dimvalues = 1:4)))
    expect_error(validObject(x.wrong),
                 "dimension with dimtype \"iteration\"")
    ## 'metadataY' does not have any dimensions with dimtype "quantile"
    x.wrong <- x
    x.wrong@metadataY <- new("MetaData",
                             nms = c("age", "quantile"),
                             dimtypes = c("age", "quantile"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                 new("Quantiles", dimvalues = c(0, 0.25, 0.75, 1))))
    expect_error(validObject(x.wrong),
                 "dimension with dimtype \"quantile\"")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from Theta work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'theta' is double
    x.wrong <- x
    x.wrong@theta <- 1:20
    expect_error(validObject(x.wrong),
                 "'theta' does not have type \"double\"")
    ## 'theta' has no missing values
    x.wrong <- x
    x.wrong@theta[1] <- NA
    expect_error(validObject(x.wrong),
                 "'theta' has missing values")
    ## dimensions of 'metadataY' consistent with length of 'theta'
    x.wrong <- x
    x.wrong@theta <- x.wrong@theta[-1]
    expect_error(validObject(x.wrong),
                 "dimensions of 'metadataY' inconsistent with length of 'theta'")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from Poisson work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'theta' has no negative values
    x.wrong <- x
    x.wrong@theta[1] <- -1
    expect_error(validObject(x.wrong),
                 "'theta' has negative values")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from LowerUpper work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'lower' has length 1
    x.wrong <- x
    x.wrong@lower <- c(1, 1)
    expect_error(validObject(x.wrong),
                 "'lower' does not have length 1")
    ## 'upper' is not missing
    x.wrong <- x
    x.wrong@upper <- as.numeric(NA)
    expect_error(validObject(x.wrong),
                 "'upper' is missing")
    ## 'lower' is double
    x.wrong <- x
    x.wrong@lower <- 1L
    expect_error(validObject(x.wrong),
                 "'lower' does not have type \"double\"")
    ## 'tolerance' is double
    x.wrong <- x
    x.wrong@tolerance <- 1L
    expect_error(validObject(x.wrong),
                 "'tolerance' does not have type \"double\"")
    ## 'lower' is less than upper
    x.wrong <- x
    x.wrong@lower <- 2
    x.wrong@upper <- 1
    expect_error(validObject(x.wrong),
                 "'lower' not less than 'upper'")
    ## 'tolerance' is non-negative
    x.wrong <- x
    x.wrong@tolerance <- -0.00001
    expect_error(validObject(x.wrong),
                 "'tolerance' is negative")
    ## 'maxAttempt' has length 1
    x.wrong <- x
    x.wrong@maxAttempt <- c(100L, 100L)
    expect_error(validObject(x.wrong),
                 "'maxAttempt' does not have length 1")
    ## 'maxAttempt' is not missing
    x.wrong <- x
    x.wrong@maxAttempt <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'maxAttempt' is missing")
    ## 'maxAttempt' is positive
    x.wrong <- x
    x.wrong@maxAttempt <- -1L
    expect_error(validObject(x.wrong),
                 "'maxAttempt' is less than 1")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from PoissonVarying work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'theta' greater than or equal to exp(lower)
    x.wrong <- x
    x.wrong@lower <- log(min(x.wrong@theta) + 0.1)
    expect_error(validObject(x.wrong),
                 "'theta' has values that are less than 'lower'")
    ## 'theta' less than or equal to exp(upper)
    x.wrong <- x
    x.wrong@upper <- log(max(x.wrong@theta) - 0.1)
    expect_error(validObject(x.wrong),
                 "'theta' has values that are greater than 'upper'")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from Margins work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## all elements of 'margins' are integer
    x.wrong <- x
    x.wrong@margins[[2]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'margins' has elements not of type \"integer\"")
    ## 'margins' has no missing values
    x.wrong <- x
    x.wrong@margins[[2]] <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'margins' has missing values")
    ## first element of margins is 0L
    x.wrong <- x
    x.wrong@margins[[1]] <- 1L
    expect_error(validObject(x.wrong),
                 "first element of 'margins' is not 0L")
    ## all other elements of margins at least 1
    x.wrong <- x
    x.wrong@margins[[2]] <- 0L
    expect_error(validObject(x.wrong),
                 "'margins' has non-positive elements")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from Betas work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'betas' has at least one element
    x.wrong <- x
    x.wrong@betas <- list()
    expect_error(validObject(x.wrong),
                 "'betas' has length 0")
    ## all elements of 'betas' have type "double"
    x.wrong <- x
    x.wrong@betas[[1]] <- as.integer(x.wrong@betas[[1]])
    expect_error(validObject(x.wrong),
                 "'betas' has elements not of type \"double\"")
    ## 'betas' has no missing values
    x.wrong <- x
    x.wrong@betas[[1]] <- as.double(NA)
    expect_error(validObject(x.wrong),
                 "'betas' has missing values")
    ## 'betas' does not have names
    x.wrong <- x
    names(x.wrong@betas) <- c("a", "b", "c")
    expect_error(validObject(x.wrong),
                 "'betas' has names")
    ## first element of 'betas' has length 1
    x.wrong <- x
    x.wrong@betas[[1]] <- c(1, 2)
    expect_error(validObject(x.wrong),
                 "first element of 'betas' does not have length 1")
    ## 'namesBetas' has no missing values
    x.wrong <- x
    x.wrong@namesBetas[1] <- NA
    expect_error(validObject(x.wrong),
                 "'namesBetas' has missing values")
    ## 'namesBetas' has no zero-length names
    x.wrong <- x
    x.wrong@namesBetas[1] <- ""
    expect_error(validObject(x.wrong),
                 "'namesBetas' has zero-length names")
    ## 'namesBetas' has no duplicated names
    x.wrong <- x
    x.wrong@namesBetas[2] <- "(Intercept)"
    expect_error(validObject(x.wrong),
                 "'namesBetas' has duplicates")
    ## first element of 'namesBetas' is "(Intercept)"
    x.wrong <- x
    x.wrong@namesBetas[1] <- "wrong"
    expect_error(validObject(x.wrong),
                 "first element of 'namesBetas' is not \"\\(Intercept\\)\"")
    ## all elements of 'priorsBetas' have class "Prior"
    x.wrong <- x
    x.wrong@priorsBetas[1] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'priorsBetas' has elements not of class \"Prior\"")
    ## 'priorsBetas' has no names
    x.wrong <- x
    names(x.wrong@priorsBetas) <- c("a", "b", "c")
    expect_error(validObject(x.wrong),
                 "'priorsBetas' has names")
    ## first element of 'priorsBetas' has class "ExchFixed"
    x.wrong <- x
    x.wrong@priorsBetas[[1L]] <- new("ExchNormZero", J = new("Length", 1L),
                                     tauMax = new("Scale", 5))
    expect_error(validObject(x.wrong),
                 "first element of 'priorsBetas' has class \"ExchNormZero\"")
    ## all elements of 'dims' are integer
    x.wrong <- x
    x.wrong@dims[[1L]] <- 0.0
    expect_error(validObject(x.wrong),
                 "'dims' has elements not of type \"integer\"")
    ## 'dims' does not have missing values
    x.wrong <- x
    x.wrong@dims[[1L]] <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'dims' has missing values")
    ## 'dims' is non-negative
    x.wrong <- x
    x.wrong@dims[[2L]] <- -1L
    expect_error(validObject(x.wrong),
                 "'dims' has negative values")
    ## 'dims' does not have names
    x.wrong <- x
    names(x.wrong@dims) <- c("a", "b", "c")
    expect_error(validObject(x.wrong),
                 "'dims' has names")
    ## first element of 'dims' is 0L
    x.wrong <- x
    x.wrong@dims[[1L]] <- 1L
    expect_error(validObject(x.wrong),
                 "first element of 'dims' is not 0")
    ## 'betas' and 'namesBetas' have same length
    x.wrong <- x
    x.wrong@namesBetas <- x.wrong@namesBetas[1:2]
    expect_error(validObject(x.wrong),
                 "'betas' and 'namesBetas' have different lengths")
    ## 'betas' and 'priorsBetas' have same length
    x.wrong <- x
    x.wrong@priorsBetas <- x.wrong@priorsBetas[1:2]
    expect_error(validObject(x.wrong),
                 "'betas' and 'priorsBetas' have different lengths")
    ## 'betas' and 'dims' have same length
    x.wrong <- x
    x.wrong@dims <- x.wrong@dims[1:2]
    expect_error(validObject(x.wrong),
                 "'betas' and 'dims' have different lengths")
    ## all elements of 'betas' other than first have valid
    ## length for corresponding element of 'priorsBetas'
    x.wrong <- x
    x.wrong@betas[[2]] <- rnorm(1)
    expect_error(validObject(x.wrong),
                 paste("\"age\" element of 'betas' has invalid length \\[1\\]",
                       "for corresponding prior of class \"ExchNormZero\""))
    ## length of 'indices' from iterator equals length of 'betas'
    x.wrong <- x
    x.wrong@iteratorBetas <- BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L))
    expect_error(validObject(x.wrong),
                 "length of 'indices' from 'iteratorBetas' not equal to length of 'betas'")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from CellInLikMixin work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 10),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'cellInLik' same length as 'theta'
    x.wrong <- x
    x.wrong@cellInLik <- x.wrong@cellInLik[-1]
    expect_error(validObject(x.wrong),
                 "'cellInLik' and 'theta' have different lengths")
             ## 'cellInLik' has no missing values
    x.wrong <- x
    x.wrong@cellInLik[1] <- NA
    expect_error(validObject(x.wrong),
                 "'cellInLik' has missing values")
})

test_that("validity tests for PoissonVaryingNotUseExp inherited from NAcceptTheta work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingNotUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'NAcceptTheta' no larger than length of 'theta'
    x.wrong <- x
    x.wrong@nAcceptTheta <- new("Counter", 21L)
    expect_error(validObject(x.wrong),
                 "'nAcceptTheta' is larger than the length of 'theta'")
})

test_that("can create a valid object of class PoissonVaryingUseExp", {
    BetaIterator <- demest:::BetaIterator
    x <- new("PoissonVaryingUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## version consisting only of intercept
    x <- new("PoissonVaryingUseExp",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5),
             namesBetas = "(Intercept)",
             margins = list(0L),
             priorsBetas = list(new("ExchFixed")),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L)),
             dims = list(0L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class BinomialVarying", {
    BetaIterator <- demest:::BetaIterator
    x <- new("BinomialVarying",
             theta = rbeta(n = 20, shape1 = 5, shape2 = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## version consisting only of intercept
    x <- new("BinomialVarying",
             theta = rbeta(n = 20, shape1 = 5, shape2 = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5),
             namesBetas = "(Intercept)",
             margins = list(0L),
             priorsBetas = list(new("ExchFixed")),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L)),
             dims = list(0L))
    expect_true(validObject(x))
})

test_that("validity tests for BinomialVarying inherited from Binomial work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("BinomialVarying",
             theta = rbeta(n = 20, shape1 = 5, shape2 = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'theta' is non-negative
    x.wrong <- x
    x.wrong@theta[1] <- -0.1
    expect_error(validObject(x.wrong),
                 "'theta' has negative values")
    ## 'theta' is non-negative
    x.wrong <- x
    x.wrong@theta[1] <- 1.1
    expect_error(validObject(x.wrong),
                 "'theta' has values greater than 1")
})

test_that("validity tests for BinomialVarying inherited from BinomialVarying work", {
    BetaIterator <- demest:::BetaIterator
    logit <- function(p) log(p / (1-p))
    x <- new("BinomialVarying",
             theta = rbeta(n = 20, shape1 = 5, shape2 = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## 'theta' greater than or equal to invlogit(lower)
    x.wrong <- x
    x.wrong@lower <- logit(min(x.wrong@theta) + 0.1)
    expect_error(validObject(x.wrong),
                 "'theta' has values that are less than 'lower'")
    ## 'theta' less than or equal to invlogit(upper)
    x.wrong <- x
    x.wrong@upper <- logit(max(x.wrong@theta) - 0.1)
    expect_error(validObject(x.wrong),
                 "'theta' has values that are greater than 'upper'")
})

test_that("can create a valid object of class NormalVaryingVarsigmaKnown", {
    BetaIterator <- demest:::BetaIterator
    x <- new("NormalVaryingVarsigmaKnown",
             theta = rnorm(n = 20, sd = 1.3),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    expect_true(validObject(x))
    ## version consisting only of intercept
    x <- new("NormalVaryingVarsigmaKnown",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rep(1, 20),
             varsigma = new("Scale", 0.3),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5),
             namesBetas = "(Intercept)",
             margins = list(0L),
             priorsBetas = list(new("ExchFixed")),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L)),
             dims = list(0L))
    expect_true(validObject(x))
})

test_that("validity tests for NormalVaryingVarsigmaKnown inherited from Normal work", {
    BetaIterator <- demest:::BetaIterator
    x <- new("NormalVaryingVarsigmaKnown",
             theta = rnorm(n = 20, sd = 1.3),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    expect_true(validObject(x))
    x@w[1] <- NA
    expect_true(validObject(x))
    ## 'w' is all positive values
    x.wrong <- x
    x.wrong@w[1] <- -1
    expect_error(validObject(x.wrong),
                 "'w' has non-positive values")
    ## 'w' same length as 'theta'
    x.wrong <- x
    x.wrong@w <- x.wrong@w[-1]
    expect_error(validObject(x.wrong),
                 "'w' and 'theta' have different lengths")
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknown", {
    BetaIterator <- demest:::BetaIterator
    x <- new("NormalVaryingVarsigmaUnknown",
             theta = rnorm(n = 20, sd = 1.3),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.4),
             varsigmaMax = new("Scale", 5),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L))
    ## version consisting only of intercept
    x <- new("NormalVaryingVarsigmaUnknown",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rep(1, 20),
             varsigma = new("Scale", 0.3),
             varsigmaMax = new("Scale", 5),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5),
             namesBetas = "(Intercept)",
             margins = list(0L),
             priorsBetas = list(new("ExchFixed")),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L)),
             dims = list(0L))
    expect_true(validObject(x))
})

test_that("can create valid object of class PoissonBinomialMixture", {
    expect_true(validObject(new("PoissonBinomialMixture",
                                prob = 0.98,
                                metadataY = new("MetaData",
                                nms = c("age", "region"),
                                dimtypes = c("age", "state"),
                                DimScales = list(new("Intervals", dimvalues = 0:5),
                                new("Categories", dimvalues = c("a", "b", "c", "d")))))))
})

test_that("validity tests for PoissonBinomialMixture inherited from Prob work", {
    x <- new("PoissonBinomialMixture", prob = 0.98)
    ## 'prob has length 1
    x.wrong <- x
    x.wrong@prob <- c(0.98, 0.98)
    expect_error(validObject(x.wrong),
                 "'prob' does not have length 1")
    ## 'prob' is not missing
    x.wrong <- x
    x.wrong@prob <- as.numeric(NA)
    expect_error(validObject(x.wrong),
                 "'prob' is missing")
    ## 'prob' is double
    x.wrong <- x
    x.wrong@prob <- 1L
    expect_error(validObject(x.wrong),
                 "'prob' does not have type \"double\"")
    ## 'prob' is between 0 and 1
    x.wrong <- x
    x.wrong@prob <- 1.1
    expect_error(validObject(x.wrong),
                 "'prob' is not between 0 and 1")
})

## Binomial - Aggregate

test_that("can create a valid object of class BinomialVaryingAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),             
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
    ## valueAg is a scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- new("ParameterVector", sum(weightAg * theta))
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("BinomialVaryingAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("validity tests for BinomialVaryingAgCertain inherited from WeightAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    set.seed(100)
    x <- new("BinomialVaryingAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    ## 'weightAg' has type "double"
    x.wrong <- x
    x.wrong@weightAg <- as.integer(x.wrong@weightAg)
    expect_error(validObject(x.wrong),
                 "'weightAg' does not have type \"double\"")
    ## all non-missing values in 'weightAg' are non-negative
    x.wrong <- x
    x.wrong@weightAg[1] <- -1
    expect_error(validObject(x.wrong),
                 "'weightAg' has negative values")
    ## 'weightAg' has length implied by 'transformAg'
    x.wrong <- x
    x.wrong@weightAg <- c(x.wrong@weightAg, NA)
    expect_error(validObject(x.wrong),
                 "'weightAg' does not have length implied by 'transformAg'")
    ## elements of 'weightAg' missing iff not used for aggregate value
    x.wrong <- x
    x.wrong@weightAg[2] <- NA
    expect_error(validObject(x.wrong),
                 "element 2 of 'weightAg' is used for aggregate value but is missing")
    x.wrong <- x
    x.wrong@weightAg[20] <- 1.0
    expect_error(validObject(x.wrong),
                 "element 20 of 'weightAg' is not used for aggregate value but is not missing")
    ## ## 'valueAg' consistent with 'theta' and 'weights'
    x.wrong <- x
    x.wrong@valueAg[1] <- x.wrong@valueAg[1] + 0.001
    expect_error(validObject(x.wrong)) ## can't get regexpr to work with numbers
})

test_that("validity tests for BinomialVaryingAgCertain inherited from MuMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    set.seed(100)
    x <- new("BinomialVaryingAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    ## 'mu' has type "double"
    x.wrong <- x
    x.wrong@mu <- as.integer(x.wrong@mu)
    expect_error(validObject(x.wrong),
                 "'mu' does not have type \"double\"")
    ## 'mu' has no missing values
    x.wrong <- x
    x.wrong@mu[1] <- NA
    expect_error(validObject(x.wrong),
                 "'mu' has missing values")
    ## 'mu' has same length as 'theta'
    x.wrong <- x
    x.wrong@mu <- rep(x.wrong@mu, 2)
    expect_error(validObject(x.wrong),
                 "'mu' and 'theta' have different lengths")
})

test_that("validity tests for BinomialVaryingAgCertain inherited from BinomialVaryingAgCertain work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rep(0.6, times = 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    set.seed(100)
    x <- new("BinomialVaryingAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = 0,
             upper = 2,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 1L))
    ## 'valueAg' greater than or equal to invlogit(lower)
    x.wrong <- x
    x.wrong@theta[1:5] <- 0.1
    x.wrong@valueAg[1] <- 0.1
    expect_error(validObject(x.wrong))
    ## 'valueAg' less than or equal to invlogit(upper)
    x.wrong <- x
    x.wrong@theta[1:5] <- 0.9
    x.wrong@valueAg[1] <- 0.9
    expect_error(validObject(x.wrong))
})

test_that("can create a valid object of class BinomialVaryingAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("BinomialVaryingAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})
test_that("validity tests for BinomialVaryingAgNormal inherited from SDAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 1L),
             nFailedPropValueAg = new("Counter", 1L))
    ## 'sdAg' and 'valueAg' have same length
    x.wrong <- x
    x.wrong@sdAg <- new("ScaleVec", c(1, 2, 3, 4))
    expect_error(validObject(x.wrong),
                 "'sdAg' and 'valueAg' have different lengths")
})

test_that("validity tests for BinomialVaryingAgNormal inherited from MeanAgMixin", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rep(0.6, 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rep(0.7, 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = 0,
             upper = 2,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 1L),
             nFailedPropValueAg = new("Counter", 1L))
    ## 'meanAg' greater than or equal to invlogit(lower)
    x.wrong <- x
    x.wrong@meanAg <- new("ParameterVector", rep(0.7, 4))
    expect_error(validObject(x.wrong),
                 "'meanAg' and 'valueAg' have different lengths")
    ## 'meanAg' greater than or equal to invlogit(lower)
    x.wrong <- x
    x.wrong@meanAg[1] <- 0.4
    expect_error(validObject(x.wrong),
                 "'meanAg' less than 'lower'")
    ## 'meanAg' less than or equal to invlogit(upper)
    x.wrong <- x
    x.wrong@meanAg[1] <- 0.9
    expect_error(validObject(x.wrong),
                 "'meanAg' greater than 'upper'")
})

test_that("can create a valid object of class BinomialVaryingAgFun", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- rbeta(n = 20, shape1 = 2, shape2 = 2)
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                                      new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- new("ParameterVector",
                   c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                     funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                     funAg(xArgsAg[[3]], weightsArgsAg[[3]])))
    x <- new("BinomialVaryingAgFun",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             transformAg = transform,
             metadataAg = metadataAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 2, shape2 = 2)
    xArgsAg <- list(Values(array(theta,
                                 dim = 5:4,
                                 dimnames = list(age = 0:4,
                                                 region = c("a", "b", "c", "d"))),
                           dimscales = c(age = "Intervals")))
    weightsArgsAg <- list(Counts(array(runif(20),
                                       dim = 5:4,
                                       dimnames = list(age = 0:4,
                                                       region = c("a", "b", "c", "d"))),
                                 dimscales = c(age = "Intervals")))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), rep(1L, 4)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    valueAg <- new("ParameterVector", funAg(xArgsAg[[1]], weightsArgsAg[[1]]))
    x <- new("BinomialVaryingAgFun",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             metadataAg = NULL,
             transformAg = transform,
             funAg = funAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg)
    expect_true(validObject(x))
})


## Aggregate - Normal

test_that("can create a valid object of class NormalVaryingVarsigmaKnownAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rep(0.6, times = 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaKnownAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknownAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rep(0.6, times = 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaUnknownAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             nAcceptTheta = new("Counter", 0L),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             varsigmaMax = new("Scale", 5),
             lower = -1000,
             upper = 1000,
             scaleTheta = new("Scale", 0.1),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaKnownAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rnorm(n = 20, mean = 0, sd = 2)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rnorm(n = 3, sd = 0.5))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaKnownAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             nAcceptTheta = new("Counter", 0L),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 0L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknownAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rnorm(n = 20, mean = 0, sd = 2)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rnorm(n = 3, sd = 0.5))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaUnknownAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             nAcceptTheta = new("Counter", 0L),
             cellInLik = rep(TRUE, 20),
             w = rgamma(n = 20, shape = 1, rate = 2),
             varsigma = new("Scale", 1.3),
             varsigmaMax = new("Scale", 5),
             lower = -Inf,
             upper = Inf,
             scaleTheta = new("Scale", 0.1),
             nFailedPropTheta = new("Counter", 0L),
             maxAttempt = 100L,
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 0L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaKnownAgFun", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- rnorm(n = 20)
    varsigma <- 0.5
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                                      new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    w <- rep(1, 20)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- new("ParameterVector",
                   c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                     funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                     funAg(xArgsAg[[3]], weightsArgsAg[[3]])))
    x <- new("NormalVaryingVarsigmaKnownAgFun",
             theta = theta,
             w = w,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             varsigma = new("Scale", varsigma),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             transformAg = transform,
             metadataAg = metadataAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## scalar
    theta <- rnorm(n = 20)
    xArgsAg <- list(Values(array(theta,
                                 dim = 5:4,
                                 dimnames = list(age = 0:4,
                                                 region = c("a", "b", "c", "d"))),
                           dimscales = c(age = "Intervals")))
    w <- runif(20)
    weightsArgsAg <- list(Counts(array(w,
                                       dim = 5:4,
                                       dimnames = list(age = 0:4,
                                                       region = c("a", "b", "c", "d"))),
                                 dimscales = c(age = "Intervals")))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), rep(1L, 4)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    valueAg <- new("ParameterVector", funAg(xArgsAg[[1]], weightsArgsAg[[1]]))
    x <- new("NormalVaryingVarsigmaKnownAgFun",
             theta = theta,
             w = w,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             varsigma = new("Scale", varsigma),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             metadataAg = NULL,
             transformAg = transform,
             funAg = funAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg)
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknownAgFun", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- rnorm(n = 20)
    varsigma <- 0.5
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                         new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    w <- rep(1, 20)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(rep(1, 5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- new("ParameterVector",
                   c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                     funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                     funAg(xArgsAg[[3]], weightsArgsAg[[3]])))
    x <- new("NormalVaryingVarsigmaUnknownAgFun",
             theta = theta,
             w = w,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             varsigma = new("Scale", varsigma),
             varsigmaMax = new("Scale", 4),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             transformAg = transform,
             metadataAg = metadataAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## scalar
    theta <- rnorm(n = 20)
    xArgsAg <- list(Values(array(theta,
                                 dim = 5:4,
                                 dimnames = list(age = 0:4,
                                     region = c("a", "b", "c", "d"))),
                           dimscales = c(age = "Intervals")))
    w <- runif(20)
    weightsArgsAg <- list(Counts(array(w,
                                       dim = 5:4,
                                       dimnames = list(age = 0:4,
                                           region = c("a", "b", "c", "d"))),
                                 dimscales = c(age = "Intervals")))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), rep(1L, 4)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    valueAg <- new("ParameterVector", funAg(xArgsAg[[1]], weightsArgsAg[[1]]))
    x <- new("NormalVaryingVarsigmaUnknownAgFun",
             theta = theta,
             w = w,
             metadataY = new("MetaData",
                 nms = c("age", "region"),
                 dimtypes = c("age", "state"),
                 DimScales = list(new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             varsigma = new("Scale", varsigma),
             varsigmaMax = new("Scale", 4),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             metadataAg = NULL,
             transformAg = transform,
             funAg = funAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg)
    expect_true(validObject(x))
})


## Aggregate PoissonVaryingNotUseExp

test_that("can create a valid object of class PoissonVaryingNotUseExpAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingNotUseExpAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
    ## valueAg is a scalar
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingNotUseExpAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingNotUseExpAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingNotUseExpAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingNotUseExpAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingNotUseExpAgPoisson", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    exposureAg <- new("ScaleVec", rep(6, 3))
    x <- new("PoissonVaryingNotUseExpAgPoisson",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             exposureAg = exposureAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingNotUseExpAgPoisson",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             exposureAg = new("ScaleVec", 20),
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})


test_that("validity test for PoissonVaryingNotUseExpAgPoisson inherited from ExposureAgMixin works", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    exposureAg <- new("ScaleVec", rep(6, 3))
    x <- new("PoissonVaryingNotUseExpAgPoisson",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             exposureAg = exposureAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## 'exposureAg' and 'valueAg' have same length
    x.wrong <- x
    x.wrong@exposureAg <- new("ScaleVec", c(3, 2))
    expect_error(validObject(x.wrong),
                 "'exposureAg' and 'valueAg' have different lengths")
})

test_that("can create a valid object of class PoissonVaryingNotUseExpAgFun", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    valueAg <- new("ParameterVector", runif(n = 3))
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                                      new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- new("ParameterVector",
                   c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                     funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                     funAg(xArgsAg[[3]], weightsArgsAg[[3]])))
    x <- new("PoissonVaryingNotUseExpAgFun",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             transformAg = transform,
             metadataAg = metadataAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    xArgsAg <- list(Values(array(runif(20),
                                 dim = 5:4,
                                 dimnames = list(age = 0:4,
                                                 region = c("a", "b", "c", "d"))),
                           dimscales = c(age = "Intervals")))
    weightsArgsAg <- list(Counts(array(runif(20),
                                       dim = 5:4,
                                       dimnames = list(age = 0:4,
                                                       region = c("a", "b", "c", "d"))),
                                 dimscales = c(age = "Intervals")))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), rep(1L, 4)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    valueAg <- new("ParameterVector", funAg(xArgsAg[[1]], weightsArgsAg[[1]]))
    x <- new("PoissonVaryingNotUseExpAgFun",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             metadataAg = NULL,
             transformAg = transform,
             funAg = funAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg)
    expect_true(validObject(x))
})






## Aggregate - PoissonUseExp

test_that("can create a valid object of class PoissonVaryingUseExpAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingUseExpAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
    ## valueAg is a scalar
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingUseExpAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("validity tests for PoissonVaryingUseExpAgCertain inherited from PoissonVaryingUseExpAgCertain work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingUseExpAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = log(0.2),
             upper = log(max(theta) + 1),
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    ## 'valueAg' greater than or equal to exp(lower)
    x.wrong <- x
    x.wrong@theta[1:5] <- 0.1
    x.wrong@valueAg[1] <- 0.1
    expect_error(validObject(x.wrong))
    ## 'valueAg' less than or equal to exp(upper)
    x.wrong <- x
    x.wrong@theta[1:5] <- max(theta) + 2
    x.wrong@valueAg[1] <- max(theta) + 2
    expect_error(validObject(x.wrong))
})

test_that("can create a valid object of class PoissonVaryingUseExpAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingUseExpAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingUseExpAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingUseExpAgPoisson", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    exposureAg <- new("ScaleVec", runif(n = 3, min = 1, max = 5))
    x <- new("PoissonVaryingUseExpAgPoisson",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             exposureAg = exposureAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- as.double(prop.table(1:20))
    valueAg <- sum(weightAg * theta)
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    x <- new("PoissonVaryingUseExpAgPoisson",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             exposureAg = new("ScaleVec", 5),
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = NULL,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingUseExpAgFun", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    valueAg <- new("ParameterVector", runif(n = 3))
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                                      new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- new("ParameterVector",
                   c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                     funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                     funAg(xArgsAg[[3]], weightsArgsAg[[3]])))
    x <- new("PoissonVaryingUseExpAgFun",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             transformAg = transform,
             metadataAg = metadataAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    xArgsAg <- list(Values(array(runif(20),
                                 dim = 5:4,
                                 dimnames = list(age = 0:4,
                                                 region = c("a", "b", "c", "d"))),
                           dimscales = c(age = "Intervals")))
    weightsArgsAg <- list(Counts(array(runif(20),
                                       dim = 5:4,
                                       dimnames = list(age = 0:4,
                                                       region = c("a", "b", "c", "d"))),
                                 dimscales = c(age = "Intervals")))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), rep(1L, 4)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    valueAg <- new("ParameterVector", funAg(xArgsAg[[1]], weightsArgsAg[[1]]))
    x <- new("PoissonVaryingUseExpAgFun",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             metadataAg = NULL,
             transformAg = transform,
             funAg = funAg,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg)
    expect_true(validObject(x))
})

test_that("validity tests for PoissonVaryingUseExpAgFun inherited from AgFun work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    meanAg <- new("ParameterVector", runif(n = 3))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 5), c(1:3, 0L)),
                     dims = c(0L, 1L),
                     dimBefore = 5:4,
                     dimAfter = 3L)
    transform <- makeCollapseTransformExtra(transform)
    metadataY <- new("MetaData",
                     nms = c("age", "region"),
                     dimtypes = c("age", "state"),
                     DimScales = list(new("Intervals", dimvalues = 0:5),
                         new("Categories", dimvalues = c("a", "b", "c", "d"))))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = metadataY,
                                                 transform = transform)
    xArgsAg <- list(new("Values",
                        .Data = array(theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(theta[6:10], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(theta[11:15], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgsAg <- list(new("Counts",
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(runif(5), dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    funAg <- function(x, weights) sum(x * weights^2) + 1
    valueAg <- c(funAg(xArgsAg[[1]], weightsArgsAg[[1]]),
                 funAg(xArgsAg[[2]], weightsArgsAg[[2]]),
                 funAg(xArgsAg[[3]], weightsArgsAg[[3]]))
    valueAg <- new("ParameterVector", valueAg)
    x <- new("PoissonVaryingUseExpAgFun",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                 new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                 new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             metadataAg = metadataAg,
             transformAg = transform,
             xArgsAg = xArgsAg,
             weightsArgsAg = weightsArgsAg,
             funAg = funAg)
    expect_true(validObject(x))
    ## funAg runs without error
    x.wrong <- x
    x.wrong@funAg <- function(x, weights) stop("error")
    expect_error(validObject(x.wrong),
                 "error calculating element 1 of 'valueAg' : error")
    ## 'xArgsAg', 'weightsArgsAg', 'funAg', and 'valueAg' consistent
    x.wrong <- x
    x.wrong@valueAg@.Data[3] <- x.wrong@valueAg@.Data[3] + 0.1
    expect_error(validObject(x.wrong),
                 "element 3 of 'valueAg' not equal to funAg\\(x, weights\\)")
})

test_that("can create a valid object of class PoissonVaryingUseExpAgLife", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    expect_true(validObject(x))
    ## scalar
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    valueAg <- 30
    valueAg <- new("ParameterVector", valueAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), rep(1L, 4)),
                       dims = c(1L, 0L),
                       dimBefore = 5:4,
                       dimAfter = 1L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    mxAg <- rbeta(n = 10, shape1 = 5, shape2 = 5)
    transformThetaToMxAg <- new("CollapseTransform",
                                indices = list(1:10, c(1L, 1L)),
                                dims = c(1L, 0L),
                                dimBefore = c(10L, 2L),
                                dimAfter = 10L)
    transformThetaToMxAg <- makeCollapseTransformExtra(transformThetaToMxAg)
    metadataAg <- new("MetaData",
                      nms = c("age", "region"),
                      dimtypes = c("age", "state"),
                      DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 40, 5), Inf)),
                                       new("Categories", dimvalues = c("a", "b"))))
    metadataMxAg <- new("MetaData",
                        nms = "age", 
                        dimtypes = "age", 
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 40, 5), Inf))))
    axAg <- c(0.1, 1.5, rep(2.5, times = 8))
    nxAg <- c(1, 4, rep(5, 7), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 40, 5), Inf)),
                                              new("Categories", dimvalues = c("a", "b")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = new("ParameterVector", 0.5),
             sdAg = new("ScaleVec", 0.2),
             transformThetaToMxAg = transformThetaToMxAg,
             metadataAg = NULL,
             mxAg = mxAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 10L))
    expect_true(validObject(x))
})

test_that("tests for PoissonVaryingUseExpAgLife inherited from AxAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    ## 'axAg' has no missing values
    x.wrong <- x
    x.wrong@axAg[1] <- NA
    expect_error(validObject(x.wrong),
                 "'axAg' has missing values")
    ## 'axAg' has no negative values
    x.wrong <- x
    x.wrong@axAg[1] <- -1
    expect_error(validObject(x.wrong),
                 "'axAg' has negative values")
})


test_that("tests for PoissonVaryingUseExpAgLife inherited from MetadataMxAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    ## 'metadataMxAg' has dimension with dimtype "age"
    x.wrong <- x
    x.wrong@metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("state", "state"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:20)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    expect_error(validObject(x.wrong),
                 "'metadataMxAg' does not have a dimension with dimtype \"age\"")
    ## age dimension of 'metadataMxAg' has dimscale "Intervals"
    x.wrong <- x
    x.wrong@metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Points", dimvalues = 0:19),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    expect_error(validObject(x.wrong),
                 "dimension of 'metadataMxAg' with dimtype \"age\" does not have dimscale \"Intervals\"")
})


test_that("tests for PoissonVaryingUseExpAgLife inherited from MxAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    ## 'mxAg' has no missing values
    x.wrong <- x
    x.wrong@mxAg[1] <- NA
    expect_error(validObject(x.wrong),
                 "'mxAg' has missing values")
    ## 'mxAg' is non-negative
    x.wrong <- x
    x.wrong@mxAg[1] <- -1
    expect_error(validObject(x.wrong),
                 "'mxAg' has negative values")
})


test_that("tests for PoissonVaryingUseExpAgLife inherited from NxAgMixin work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    ## 'nxAg' has no missing values
    x.wrong <- x
    x.wrong@nxAg[1] <- NA
    expect_error(validObject(x.wrong),
                 "'nxAg' has missing values")
    ## 'nxAg' is non-negative
    x.wrong <- x
    x.wrong@nxAg[1] <- 0
    expect_error(validObject(x.wrong),
                 "'nxAg' has non-positive values")
})

test_that("tests for PoissonVaryingUseExpAgLife inherited from AgLife work", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    theta <- rbeta(n = 4 * 20 * 3, shape1 = 5, shape2 = 5)
    metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                      new("Categories", dimvalues = as.character(1:3))))
    valueAg <- c(4, 3, 5)
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    mxAg <- rbeta(n = 3 * 20, shape1 = 5, shape2 = 5)
    transformAg <- new("CollapseTransform",
                       indices = list(c(1:3, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 3L))
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    metadataMxAg <- new("MetaData",
                        nms = c("age", "region"),
                        dimtypes = c("age", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 90, 5), Inf)),
                                         new("Categories", dimvalues = c("a", "b", "c"))))
    axAg <- rep(c(0.1, 1.5, rep(2.5, times = 18)), times = 3)
    nxAg <- c(1, 4, rep(5, 17), Inf)
    x <- new("PoissonVaryingUseExpAgLife",
             theta = theta,
             metadataY = metadataY,
             cellInLik = rep(TRUE, 240),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             mxAg = mxAg,
             transformThetaToMxAg = transformAg,
             metadataAg = metadataAg,
             metadataMxAg = metadataMxAg,
             axAg = axAg,
             nxAg = nxAg,
             nAgeAg = new("Length", 20L))
    ## 'y' has dimension with dimtype "age"
    x.wrong <- x
    x.wrong@metadataY <- new("MetaData",
                             nms = c("region", "age", "education"),
                             dimtypes = c("state", "state", "state"),
                             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                              new("Categories", dimvalues = as.character(1:20)),
                                                  new("Categories", dimvalues = as.character(1:3))))
    expect_error(validObject(x.wrong),
                 "'y' does not have a dimension with dimtype \"age\"")
    ## age dimension of 'y' has dimscale "Intervals"
    x.wrong <- x
    x.wrong@metadataY <- new("MetaData",
                             nms = c("region", "age", "education"),
                             dimtypes = c("state", "age", "state"),
                             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                              new("Points", dimvalues = 0:19),
                                              new("Categories", dimvalues = as.character(1:3))))
    expect_error(validObject(x.wrong),
                 "dimension of 'y' with dimtype \"age\" does not have dimscale \"Intervals\"")
    ## last interval of age dimension of 'y' is open
    x.wrong <- x
    x.wrong@metadataY <- new("MetaData",
                     nms = c("region", "age", "education"),
                     dimtypes = c("state", "age", "state"),
                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")),
                                      new("Intervals", dimvalues = c(0, 1, seq(5, 95, 5))),
                                      new("Categories", dimvalues = as.character(1:3))))
    expect_error(validObject(x.wrong),
                 "last interval of dimension of 'y' with dimtype \"age\" is closed")
    ## 'metadataAg' does not have dimension with dimtype "age"
    x.wrong <- x
    x.wrong@metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "age",
                      DimScales = list(new("Intervals", dimvalues = 0:3)))
    expect_error(validObject(x.wrong),
                 "'metadataAg' has a dimension with dimtype \"age\"")
    ## 'mxAg' and 'axAg' have same length
    x.wrong <- x
    x.wrong@mxAg <- rbeta(n = 3 * 20 + 1, shape1 = 5, shape2 = 5)
    expect_error(validObject(x.wrong),
                 "'mxAg' and 'axAg' have different lengths")
    ## dimensions of 'metadataAg' consistent with length of 'mx'
    x.wrong <- x
    x.wrong@metadataMxAg <- new("MetaData",
                                nms = c("age", "region"),
                                dimtypes = c("age", "state"),
                                DimScales = list(new("Intervals", dimvalues = c(0, 1, seq(5, 9, 5), Inf)),
                                    new("Categories", dimvalues = c("a", "b", "c", "d"))))
    expect_error(validObject(x.wrong),
                 "dimensions of 'metadataMxAg' inconsistent with length of 'mxAg'")
    ## 'dimBefore' for 'transformThetaToMxAg' consistent with 'theta'
    transform.wrong <- new("CollapseTransform",
                       indices = list(c(1:2, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(3L, 20L, 3L),
                       dimAfter = c(20L, 2L))
    transform.wrong <- makeCollapseTransformExtra(transform.wrong)
    x.wrong <- x
    x.wrong@transformThetaToMxAg <- transform.wrong
    expect_error(validObject(x.wrong),
                 "'dimBefore' from 'transformThetaToMxAg' inconsistent with length of 'theta'")
    ## 'dimAfter' for 'transformThetaToMxAg' consistent with 'axAg'
    transform.wrong <- new("CollapseTransform",
                       indices = list(c(1:2, 0L, 0L), 1:20, c(1L, 1L, 1L)),
                       dims = c(2L, 1L, 0L),
                       dimBefore = c(4L, 20L, 3L),
                       dimAfter = c(20L, 2L))
    transform.wrong <- makeCollapseTransformExtra(transform.wrong)
    x.wrong <- x
    x.wrong@transformThetaToMxAg <- transform.wrong
    expect_error(validObject(x.wrong),
                 "'dimAfter' from 'transformThetaToMxAg' inconsistent with length of 'axAg'")
    ## length of 'mxAg' equal to 'nAge' times length of 'valueAg'
    x.wrong <- x
    x.wrong@nAgeAg@.Data <- 19L
    expect_error(validObject(x.wrong),
                 "'mxAg', 'nAgeAg', and 'valueAg' inconsistent")
    ## length of 'nxAg' equal to 'nAge'
    x.wrong <- x
    x.wrong@nxAg <- x.wrong@nxAg[-1]
    expect_error(validObject(x.wrong),
                 "'nxAg' and 'nAgeAg' inconsistent")
    ## 'axAg' consistent with 'nxAg'
    x.wrong <- x
    x.wrong@axAg[1] <- 100
    expect_error(validObject(x.wrong),
                 "'nxAg' and 'axAg' inconsistent")
})


## Prediction ################################################################

test_that("can create a valid object of class NormalVaryingVarsigmaKnownPredict", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("NormalVaryingVarsigmaKnownPredict",
             theta = rnorm(n = 20),
             w = rbeta(n = 20, shape1 = 1, shape2 = 1),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             varsigma = new("Scale", 1),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknownPredict", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("NormalVaryingVarsigmaUnknownPredict",
             theta = rnorm(n = 20),
             w = rbeta(n = 20, shape1 = 1, shape2 = 1),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             varsigma = new("Scale", 1),
             varsigmaMax = new("Scale", 5),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingNotUseExpPredict", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingNotUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
})

test_that("can create a valid object of class BinomialVaryingPredict", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("BinomialVaryingPredict",
             theta = rbeta(n = 20, shape1 = 1, shape2 = 1),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingUseExpPredict", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
})

test_that("tests for PoissonVaryingUseExpPredict inherited from BetaIsPredicted work", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
    ## 'betaIsPredicted' has no missing values
    x.wrong <- x
    x.wrong@betaIsPredicted[1] <- NA
    expect_error(validObject(x.wrong),
                 "'betaIsPredicted' has missing values")
    ## 'betaIsPredicted' does not have names
    x.wrong <- x
    names(x.wrong@betaIsPredicted) <- c("a", "b", "c")
    expect_error(validObject(x.wrong),
                 "'betaIsPredicted' has names")
    ## 'betaIsPredicted' and 'betas' have same length
    x.wrong <- x
    x.wrong@betaIsPredicted <- c(x.wrong@betaIsPredicted, FALSE)
    expect_error(validObject(x.wrong),
                 "'betaIsPredicted' and 'betas' have different lengths")
    ## first element of 'betaIsPredicted' is FALSE
    x.wrong <- x
    x.wrong@betaIsPredicted[1] <- TRUE
    expect_error(validObject(x.wrong),
                 "first element of 'betaIsPredicted' is TRUE")
})

test_that("tests for PoissonVaryingUseExpPredict inherited from OffsetsBetas work", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
    ## all lements of 'OffsetsBetas' have class "Offsets"
    x.wrong <- x
    x.wrong@offsetsBetas[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'offsetsBetas' has elements not of class \"Offsets\"")
    ## 'offsetsBetas' has same length as 'betas'
    x.wrong <- x
    x.wrong@offsetsBetas <- x.wrong@offsetsBetas[-1]
    expect_error(validObject(x.wrong),
                 "'offsetsBetas' and 'betas' have different lengths")
})

test_that("tests for PoissonVaryingUseExpPredict inherited from OffsetsPriorsBetas work", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
    ## all lements of 'OffsetsPriorsBetas' have class "Offsets" or "NULL"
    x.wrong <- x
    x.wrong@offsetsPriorsBetas[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'offsetsPriorsBetas' has elements not of class \"Offsets\" or \"NULL\"")
    ## 'offsetsPriorsBetas' has same length as 'betas'
    x.wrong <- x
    x.wrong@offsetsPriorsBetas <- x.wrong@offsetsPriorsBetas[-1]
    expect_error(validObject(x.wrong),
                 "'offsetsPriorsBetas' and 'betas' have different lengths")
})

test_that("tests for PoissonVaryingUseExpPredict inherited from OffsetsSigma work", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("PoissonVaryingUseExpPredict",
             theta = rgamma(n = 20, shape = 5, rate = 5),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
    ## if offsetsSigma is non-NULL, first element equals second
    x.wrong <- x
    x.wrong@offsetsSigma[2L] <- 67L
    expect_error(validObject(x.wrong),
                 "first and second elements of 'offsetsSigma' are not equal")
})

test_that("tests for NormalVaryingVarsigmUnknownPredict inherited from OffsetsVarsigma work", {
    BetaIterator <- demest:::BetaIterator
    ## old object had 10 regions
    x <- new("NormalVaryingVarsigmaUnknownPredict",
             theta = rnorm(20),
             w = runif(20),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             varsigma = new("Scale", 2),
             varsigmaMax = new("Scale", 5),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.1),
             nAcceptTheta = new("Counter", 0L),
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsVarsigma = new("Offsets", c(52L, 52L)),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)))
    expect_true(validObject(x))
    ## if offsetsSigma is non-NULL, first element equals second
    x.wrong <- x
    x.wrong@offsetsSigma[2L] <- 67L
    expect_error(validObject(x.wrong),
                 "first and second elements of 'offsetsSigma' are not equal")
})

test_that("can create an object of class PoissonBinomialMixturePredict", {
    x <- new("PoissonBinomialMixturePredict", prob = 0.98)
    expect_true(validObject(x))
})
          

## Aggregate prediction #########################################

test_that("can create a valid object of class NormalVaryingVarsigmaKnownPredictAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <-  rnorm(n = 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaKnownPredictAgCertain",
             theta = theta,
             w = rep(1, 20),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             lower = -Inf,
             upper = Inf,
             nAcceptTheta = new("Counter", 1L),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             varsigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaKnownPredictAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rnorm(20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaKnownPredictAgNormal",
             theta = theta,
             w = rep(1, 20),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             nAcceptTheta = new("Counter", 0L),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             varsigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})


test_that("can create a valid object of class NormalVaryingVarsigmaUnknownPredictAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <-  rnorm(n = 20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaUnknownPredictAgCertain",
             theta = theta,
             w = rep(1, 20),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             lower = -Inf,
             upper = Inf,
             nAcceptTheta = new("Counter", 1L),
             maxAttempt = 100L,
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             varsigma = new("Scale", 1),
             varsigmaMax = new("Scale", 5),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class NormalVaryingVarsigmaUnknownPredictAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rnorm(20)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("NormalVaryingVarsigmaUnknownPredictAgNormal",
             theta = theta,
             w = rep(1, 20),
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             nAcceptTheta = new("Counter", 0L),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             varsigma = new("Scale", 1),
             varsigmaMax = new("Scale", 5),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingNotUseExpPredictAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingNotUseExpPredictAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class PoissonVaryingNotUseExpPredictAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingNotUseExpPredictAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid object of class BinomialVaryingPredictAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## valueAg has dim 3L
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingPredictAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid object of class BinomialVaryingPredictAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", rbeta(n = 3, shape1 = 1, shape2 = 2))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("BinomialVaryingPredictAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})

test_that("can create a valid objects of class PoissonVaryingUseExpPredictAgCertain", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 5 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingUseExpPredictAgCertain",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20))
    expect_true(validObject(x))
})

test_that("can create a valid objects of class PoissonVaryingUseExpPredictAgNormal", {
    BetaIterator <- demest:::BetaIterator
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## dim = 3L
    theta <- 2 * rbeta(n = 20, shape1 = 5, shape2 = 5)
    weightAg <- matrix(c(1:15, rep(NA, 5)), nrow = 5)
    weightAg <- prop.table(weightAg, margin = 2)
    valueAg <- colSums(weightAg * theta)[1:3]
    valueAg <- new("ParameterVector", valueAg)
    meanAg <- new("ParameterVector", rbeta(n = 3, shape1 = 0.5, shape2 = 1))
    sdAg <- new("ScaleVec", sqrt(meanAg))
    weightAg <- as.double(weightAg)
    transformAg <- new("CollapseTransform",
                       indices = list(rep(1L, 5), c(1:3, 0L)),
                       dims = c(0L, 1L),
                       dimBefore = 5:4,
                       dimAfter = 3L)
    transformAg <- makeCollapseTransformExtra(transformAg)
    metadataAg <- new("MetaData",
                      nms = "region",
                      dimtypes = "state",
                      DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    x <- new("PoissonVaryingUseExpPredictAgNormal",
             theta = theta,
             metadataY = new("MetaData",
                             nms = c("age", "region"),
                             dimtypes = c("age", "state"),
                             DimScales = list(new("Intervals", dimvalues = 0:5),
                                              new("Categories", dimvalues = c("a", "b", "c", "d")))),
             cellInLik = rep(TRUE, 20),
             scaleTheta = new("Scale", 0.1),
             scaleThetaMultiplier = new("Scale", 1),
             nAcceptTheta = new("Counter", 0L),
             sigma = new("Scale", 1),
             sigmaMax = new("Scale", 5),
             ASigma = new("Scale", 1),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             nFailedPropTheta = new("Counter", 0L),
             betas = list(5, rnorm(5), rnorm(4)),
             namesBetas = c("(Intercept)", "age", "region"),
             margins = list(0L, 1L, 2L),
             priorsBetas = list(new("ExchFixed"),
                                new("ExchNormZero", J = new("Length", 5L), tauMax = new("Scale", 5)),
                                new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5))),
             iteratorBetas = BetaIterator(dim = c(5L, 4L), margins = list(0L, 1L, 2L)),
             dims = list(0L, 5L, 4L),
             betaIsPredicted = c(FALSE, FALSE, TRUE),
             offsetsBetas = list(new("Offsets", c(53L, 53L)), new("Offsets", c(54L, 58L)),
                                 new("Offsets", c(59L, 62L))),
             offsetsPriorsBetas = list(NULL, new("Offsets", c(67L, 67L)), new("Offsets", c(68L, 68L))),
             offsetsSigma = new("Offsets", c(66L, 66L)),
             valueAg = valueAg,
             meanAg = meanAg,
             sdAg = sdAg,
             scaleAg = new("Scale", 0.1),
             weightAg = weightAg,
             transformAg = transformAg,
             metadataAg = metadataAg,
             mu = rnorm(20),
             nAcceptAg = new("Counter", 5L),
             nFailedPropValueAg = new("Counter", 1L))
    expect_true(validObject(x))
})



## test iMethodModel

test_that("Model classes have correct value for iMethodModel", {
    ## No exposure
    x <- new("NormalVaryingVarsigmaKnown")
    expect_identical(x@iMethodModel, 4L)
    x <- new("NormalVaryingVarsigmaUnknown")
    expect_identical(x@iMethodModel, 5L)
    x <- new("PoissonVaryingNotUseExp")
    expect_identical(x@iMethodModel, 6L)
    ## Exposure
    x <- new("BinomialVarying")
    expect_identical(x@iMethodModel, 9L)
    x <- new("PoissonVaryingUseExp")
    expect_identical(x@iMethodModel, 10L)
    x <- new("PoissonBinomialMixture")
    expect_identical(x@iMethodModel, 11L)
    ## No exposure - aggregate
    x <- new("NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(x@iMethodModel, 12L)
    x <- new("NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(x@iMethodModel, 13L)
    x <- new("NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(x@iMethodModel, 14L)
    x <- new("NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(x@iMethodModel, 15L)
    x <- new("PoissonVaryingNotUseExpAgCertain")
    expect_identical(x@iMethodModel, 16L)
    x <- new("PoissonVaryingNotUseExpAgNormal")
    expect_identical(x@iMethodModel, 17L)
    ## Exposure - aggregate
    x <- new("BinomialVaryingAgCertain")
    expect_identical(x@iMethodModel, 18L)
    x <- new("BinomialVaryingAgNormal")
    expect_identical(x@iMethodModel, 19L)
    x <- new("PoissonVaryingUseExpAgCertain")
    expect_identical(x@iMethodModel, 20L)
    x <- new("PoissonVaryingUseExpAgNormal")
    expect_identical(x@iMethodModel, 21L)
    x <- new("PoissonVaryingNotUseExpAgPoisson")
    expect_identical(x@iMethodModel, 22L)
    x <- new("PoissonVaryingUseExpAgPoisson")
    expect_identical(x@iMethodModel, 23L)
    x <- new("NormalVaryingVarsigmaKnownAgFun")
    expect_identical(x@iMethodModel, 24L)
    x <- new("NormalVaryingVarsigmaUnknownAgFun")
    expect_identical(x@iMethodModel, 25L)
    x <- new("PoissonVaryingNotUseExpAgFun")
    expect_identical(x@iMethodModel, 26L)
    x <- new("BinomialVaryingAgFun")
    expect_identical(x@iMethodModel, 27L)
    x <- new("PoissonVaryingUseExpAgFun")
    expect_identical(x@iMethodModel, 28L)
    x <- new("PoissonVaryingUseExpAgLife")
    expect_identical(x@iMethodModel, 29L)
    ## Predict
    x <- new("NormalVaryingVarsigmaKnownPredict")
    expect_identical(x@iMethodModel, 104L)
    x <- new("NormalVaryingVarsigmaUnknownPredict")
    expect_identical(x@iMethodModel, 105L)
    x <- new("PoissonVaryingNotUseExpPredict")
    expect_identical(x@iMethodModel, 106L)
    x <- new("BinomialVaryingPredict")
    expect_identical(x@iMethodModel, 109L)
    x <- new("PoissonVaryingUseExpPredict")
    expect_identical(x@iMethodModel, 110L)
    ## Predict - aggregate
    x <- new("NormalVaryingVarsigmaKnownPredictAgCertain")
    expect_identical(x@iMethodModel, 112L)
    x <- new("NormalVaryingVarsigmaUnknownPredictAgCertain")
    expect_identical(x@iMethodModel, 113L)
    x <- new("NormalVaryingVarsigmaKnownPredictAgNormal")
    expect_identical(x@iMethodModel, 114L)
    x <- new("NormalVaryingVarsigmaUnknownPredictAgNormal")
    expect_identical(x@iMethodModel, 115L)
    x <- new("PoissonVaryingNotUseExpPredictAgCertain")
    expect_identical(x@iMethodModel, 116L)
    x <- new("PoissonVaryingNotUseExpPredictAgNormal")
    expect_identical(x@iMethodModel, 117L)
    x <- new("BinomialVaryingPredictAgCertain")
    expect_identical(x@iMethodModel, 118L)
    x <- new("BinomialVaryingPredictAgNormal")
    expect_identical(x@iMethodModel, 119L)
    x <- new("PoissonVaryingUseExpPredictAgCertain")
    expect_identical(x@iMethodModel, 120L)
    x <- new("PoissonVaryingUseExpPredictAgNormal")
    expect_identical(x@iMethodModel, 121L)
    x <- new("PoissonVaryingNotUseExpPredictAgPoisson")
    expect_identical(x@iMethodModel, 122L)
    x <- new("PoissonVaryingUseExpPredictAgPoisson")
    expect_identical(x@iMethodModel, 123L)
})

