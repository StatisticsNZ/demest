
context("AllClasses-Combined")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("can create valid object of class CombinedModelBinomial", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    exposure[1] <- NA
    y[1:2] <- NA
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure)
    expect_true(validObject(x))
})

test_that("validity tests for CombinedModelBinomial inherited from HasExposure work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'exposure' is missing only if 'y' is
    x.wrong <- x
    x.wrong@y[1] <- NA
    x.wrong@exposure[1:2] <- NA
    expect_error(validObject(x.wrong),
                 "'exposure' has missing values where 'y' does not")
    ## 'exposure' non-negative
    x.wrong <- x
    x.wrong@exposure[1] <- -1
    expect_error(validObject(x.wrong),
                 "'exposure' has negative values")
    ## 'exposure' and 'y' have identical metadata
    x.wrong <- x
    x.wrong@exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                                     dim = 2:4,
                                     dimnames = list(sex = c("female", "male"), region = letters[1:3],
                                     age = 0:3)))
    expect_error(validObject(x.wrong),
                 "'exposure' and 'y' have different metadata")
    ## y is 0 if exposure is 0
    x.wrong <- x
    x.wrong@exposure[3] <- 0L
    x.wrong@y[3] <- 10L
    expect_error(validObject(x.wrong),
                 "y > 0 but exposure == 0 for some cells")
})

test_that("validity tests for CombinedModelBinomial inherited from Y work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'y' does not have iteration dimension
    x.wrong <- x
    x.wrong@exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                                     dim = 2:4,
                                     dimnames = list(sex = c("f", "m"), region = letters[1:3],
                                     iteration = 1:4)))
    x.wrong@y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                              dim = 2:4,
                              dimnames = list(sex = c("f", "m"), region = letters[1:3],
                              iteration = 1:4)))
    expect_error(validObject(x.wrong),
                 "'y' has dimension with dimtype \"iteration\"")
    ## 'y' does not have quantile dimension
    x.wrong <- x
    x.wrong@exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                                     dim = 2:4,
                                     dimnames = list(sex = c("f", "m"), region = letters[1:3],
                                     quantile = c(0, 0.1, 0.8, 1))))
    x.wrong@y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                              dim = 2:4,
                              dimnames = list(sex = c("f", "m"), region = letters[1:3],
                              quantile = c(0, 0.1, 0.8, 1))))
    expect_error(validObject(x.wrong),
                 "'y' has dimension with dimtype \"quantile\"")
})

test_that("validity tests for CombinedModelBinomial inherited from YCounts work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'y' has class counts
    x.wrong <- x
    x.wrong@y <- as(x.wrong@y, "Values")
    expect_error(validObject(x.wrong),
                 "'y' has class \"Values\"")
    ## 'y' has type "integer"
    x.wrong <- x
    x.wrong@y[1] <- 1.1
    expect_error(validObject(x.wrong),
                 "'y' does not have type \"integer\"")
})

test_that("validity tests for CombinedModelBinomial inherited from YNonNegativeCounts work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 scaleTheta = new("Scale", 0.1),
                 cellInLik = rep(TRUE, 24),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'y' non-negative
    x.wrong <- x
    x.wrong@y[1] <- -1L
    x.wrong@y[2] <- NA
    expect_error(validObject(x.wrong),
                 "'y' has negative values")
})

test_that("validity tests for CombinedModelBinomial inherited from CombinedBinomial work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'model' has class Binomial
    x.wrong <- x
    x.wrong@model <- new("PoissonVaryingUseExp",
                         theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                         thetaTransformed = rnorm(24),
                         metadataY = y@metadata,
                         cellInLik = rep(TRUE, 24),
                         strucZeroArray = Counts(array(1L,
                                                       dim = 2:4,
                                                       dimnames = list(sex = c("f", "m"),
                                                                       region = letters[1:3], age = 0:3))),
                         scaleTheta = new("Scale", 0.1),
                         nAcceptTheta = new("Counter", 0L),
                         lower = -Inf,
                         upper = Inf,
                         maxAttempt = 100L,
                         nFailedPropTheta = new("Counter", 0L),
                         sigma = new("Scale", 1),
                         ASigma = new("Scale", 1),
                         sigmaMax = new("Scale", 5),
                         betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                         meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                         namesBetas = c("(Intercept)", c("sex", "region", "age")),
                         margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                         iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                         dims = list(0L, 2L, 3L, 4L),
                         mu = rnorm(24))
    expect_error(validObject(x.wrong),
                 "'model' has class \"PoissonVaryingUseExp\"")
})

test_that("validity tests for CombinedModelBinomial inherited from CombinedModelBinomial work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.integer(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rbinom(24, size = exposure, prob = 0.9)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("BinomialVarying",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelBinomial",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'exposure' has type "integer"
    x.wrong <- x
    x.wrong@exposure <- toDouble(x.wrong@exposure)
    expect_error(validObject(x.wrong),
                 "'exposure' does not have type \"integer\"")
    ## 'y' and 'theta' have same length
    x.wrong <- x
    x.wrong@y <- Counts(array(0L,
                              dim = c(2L, 4, 4),
                              dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    x.wrong@exposure <- Counts(array(1L,
                                     dim = c(2L, 4, 4),
                                     dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    expect_error(validObject(x.wrong),
                 "'y' and 'theta' have different lengths")
    ## 'y' <= 'exposure'
    x.wrong <- x
    x.wrong@y[1] <- 10L
    x.wrong@exposure[1] <- 9L
    x.wrong@y[2] <- NA
    x.wrong@exposure[2] <- NA
    expect_error(validObject(x.wrong),
                 "y > exposure for some cells")
})

test_that("can create valid object of class CombinedModelNormal", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(rnorm(24, mean = 1, sd = 3),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y[1] <- NA
    model <- new("NormalVaryingVarsigmaKnown",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 w = runif(n = 24),
                 metadataY = y@metadata,
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 varsigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelNormal",
             model = model,
             y = y,
             slotsToExtract = "model")
    expect_true(validObject(x))
})

test_that("validity tests for CombinedModelNormal inherited from CombinedNormal work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(rnorm(24, mean = 1, sd = 3),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("NormalVaryingVarsigmaKnown",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 w = runif(n = 24),
                 cellInLik = rep(TRUE, 24),
                 metadataY = y@metadata,
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 varsigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelNormal",
             model = model,
             y = y,
             slotsToExtract = "model")
    expect_true(validObject(x))
    ## 'model' has class "Normal"
    x.wrong <- x
    x.wrong@model <- new("BinomialVarying",
                         theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                         thetaTransformed = rnorm(24),
                         metadataY = y@metadata,
                         cellInLik = rep(TRUE, 24),
                         scaleTheta = new("Scale", 0.1),
                         nAcceptTheta = new("Counter", 0L),
                         lower = -Inf,
                         upper = Inf,
                         maxAttempt = 100L,
                         nFailedPropTheta = new("Counter", 0L),
                         sigma = new("Scale", 1),
                         sigmaMax = new("Scale", 5),
                         ASigma = new("Scale", 1),
                         betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                         meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                         namesBetas = c("(Intercept)", c("sex", "region", "age")),
                         margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                         iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                         dims = list(0L, 2L, 3L, 4L),
                         mu = rnorm(24))
    expect_error(validObject(x.wrong),
                 "'model' has class \"BinomialVarying\"")
    ## 'w' has missing values only if 'y' does
    x.wrong <- x
    x.wrong@y[1] <- NA
    x.wrong@model@w[1:2] <- NA
    expect_error(validObject(x.wrong),
                 "'w' has missing values where 'y' does not")
    x.ok <- x
    x.ok@y[1] <- NA
    x.ok@model@w[1] <- NA
    expect_true(validObject(x.ok))
})

test_that("validity tests for CombinedModelNormal inherited from CombinedModelNormal work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(rnorm(24, mean = 1, sd = 3),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("NormalVaryingVarsigmaKnown",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 w = runif(n = 24),
                 cellInLik = rep(TRUE, 24),
                 metadataY = y@metadata,
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 varsigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelNormal",
             model = model,
             y = y,
             slotsToExtract = "model")
    expect_true(validObject(x))
    ## 'y' has type "double"
    x.wrong <- x
    x.wrong@y <- toInteger(x.wrong@y, force = TRUE)
    expect_error(validObject(x.wrong),
                 "'y' does not have type \"double\"")
    ## 'y' and 'theta' have same length
    x.wrong <- x
    x.wrong@y <- Counts(array(rnorm(36, mean = 1, sd = 3),
                              dim = c(2L, 4, 4),
                              dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    expect_error(validObject(x.wrong),
                 "'y' and 'theta' have different lengths")
    ## 'w' is missing only if 'y' is
    x.ok <- x
    x.ok@model@w[1] <- NA
    x.ok@y[1] <- NA
    expect_true(validObject(x.ok))
    x.wrong <- x
    x.wrong@model@w[1] <- NA
    expect_error(validObject(x.wrong),
                 "'w' has missing values where 'y' does not")
})

test_that("can create valid object of class CombinedModelPoissonNotHasExp", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y[1:4] <- NA
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonNotHasExp",
             model = model,
             y = y)
    expect_true(validObject(x))
})

test_that("validity tests for CombinedModelPoissonNotHasExp inherited from NotHasExposure work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonNotHasExp",
             model = model,
             y = y,
             slotsToExtract = "model")
    ## 'model' has class "NotUseExposure"
    x.wrong <- x
    x.wrong@model <- new("PoissonVaryingUseExp",
                         theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                         thetaTransformed = rnorm(24),
                         metadataY = y@metadata,
                         strucZeroArray = Counts(array(1L,
                                                       dim = 2:4,
                                                       dimnames = list(sex = c("f", "m"),
                                                                       region = letters[1:3], age = 0:3))),
                         cellInLik = rep(TRUE, 24),
                         scaleTheta = new("Scale", 0.1),
                         nAcceptTheta = new("Counter", 0L),
                         lower = -Inf,
                         upper = Inf,
                         maxAttempt = 100L,
                         nFailedPropTheta = new("Counter", 0L),
                         sigma = new("Scale", 1),
                         sigmaMax = new("Scale", 5),
                         ASigma = new("Scale", 1),
                         betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                         meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                         namesBetas = c("(Intercept)", c("sex", "region", "age")),
                         margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                         iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                         dims = list(0L, 2L, 3L, 4L),
                         mu = rnorm(24))
    expect_error(validObject(x.wrong),
                 "'model' has class \"PoissonVaryingUseExp\"")
})

test_that("validity tests for CombinedModelPoissonNotHasExp inherited from CombinedPoisson work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonNotHasExp",
             model = model,
             y = y)
    ## 'model' has class Poisson
    x.wrong <- x
    x.wrong@model <- new("BinomialVarying",
                         theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                         thetaTransformed = rnorm(24),
                         metadataY = y@metadata,
                         cellInLik = rep(TRUE, 24),
                         scaleTheta = new("Scale", 0.1),
                         nAcceptTheta = new("Counter", 0L),
                         lower = -Inf,
                         upper = Inf,
                         maxAttempt = 100L,
                         nFailedPropTheta = new("Counter", 0L),
                         sigma = new("Scale", 1),
                         sigmaMax = new("Scale", 5),
                         ASigma = new("Scale", 1),
                         betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                         meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                         namesBetas = c("(Intercept)", c("sex", "region", "age")),
                         margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                         iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                         dims = list(0L, 2L, 3L, 4L),
                         mu = rnorm(24))
    expect_error(validObject(x.wrong),
                 "'model' has class \"BinomialVarying\"")
})

test_that("validity tests for CombinedModelPoissonNotHasExp inherited from CombinedModelPoissonNotHasExp work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonNotHasExp",
             model = model,
             y = y,
             slotsToExtract = "model")
    ## 'y' and 'theta' have same length
    x.wrong <- x
    x.wrong@y <- Counts(array(as.integer(rpois(36, lambda = 10)),
                              dim = c(2L, 4, 4),
                              dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    expect_error(validObject(x.wrong),
                 "'y' and 'theta' have different lengths")
})

test_that("can create valid object of class CombinedModelPoissonHasExp", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.double(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rpois(24, lambda = exposure * 0.3)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    exposure[1] <- NA
    y[1:2] <- NA
    model <- new("PoissonVaryingUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 metadataY = y@metadata,
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonHasExp",
             model = model,
             y = y,
             exposure = exposure)
})

test_that("validity tests for CombinedModelPoissonHasExp inherited from HasExposure work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.double(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rpois(24, lambda = exposure * 0.3)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("PoissonVaryingUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonHasExp",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'model' has class "UseExposure"
    x.wrong <- x
    x.wrong@model <- new("PoissonVaryingNotUseExp",
                         theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                         thetaTransformed = rnorm(24),
                         strucZeroArray = Counts(array(1L,
                                                       dim = 2:4,
                                                       dimnames = list(sex = c("f", "m"),
                                                                       region = letters[1:3], age = 0:3))),
                         metadataY = y@metadata,
                         cellInLik = rep(TRUE, 24),
                         scaleTheta = new("Scale", 0.1),
                         nAcceptTheta = new("Counter", 0L),
                         lower = -Inf,
                         upper = Inf,
                         maxAttempt = 100L,
                         nFailedPropTheta = new("Counter", 0L),
                         sigma = new("Scale", 1),
                         sigmaMax = new("Scale", 5),
                         ASigma = new("Scale", 1),
                         betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                         meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                         momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                         namesBetas = c("(Intercept)", c("sex", "region", "age")),
                         margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                         iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                         dims = list(0L, 2L, 3L, 4L),
                         mu = rnorm(24))
    expect_error(validObject(x.wrong),
                 "'model' has class \"PoissonVaryingNotUseExp\"")
})

test_that("validity tests for CombinedModelPoissonHasExp inherited from CombinedModelPoissonHasExp work", {
    BetaIterator <- demest:::BetaIterator
    exposure <- Counts(array(as.double(rpois(24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y <- Counts(array(as.integer(rpois(24, lambda = exposure * 0.3)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    model <- new("PoissonVaryingUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                         priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                            new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 2)),
                                            new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 3)),
                                            new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                                isSaturated = new("LogicalFlag", FALSE),
                                                allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelPoissonHasExp",
             model = model,
             y = y,
             exposure = exposure,
             slotsToExtract = "model")
    ## 'y' and 'theta' have same length
    x.wrong <- x
    exposure <- Counts(array(as.double(rpois(36, lambda = 10)),
                             dim = c(2L, 4, 4),
                             dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    y <- Counts(array(as.integer(rpois(36, lambda = exposure * 0.3)),
                      dim = c(2, 4, 4),
                      dimnames = list(sex = c("f", "m"), region = letters[1:4], age = 0:3)))
    x.wrong@exposure <- exposure
    x.wrong@y <- y
    expect_error(validObject(x.wrong),
                 "'y' and 'theta' have different lengths")
    ## 'exposure' has type "double"
    x.wrong <- x
    x.wrong@exposure <- toInteger(x.wrong@exposure, force = TRUE)
    expect_error(validObject(x.wrong),
                 "'exposure' does not have type \"double\"")
    ## 'y' is 0 if exposure is 0
    x.wrong <- x
    x.wrong@y[1] <- 10L
    x.wrong@exposure[1] <- 0
    expect_error(validObject(x.wrong),
                 "y > 0 but exposure == 0 for some cells")
})

test_that("can create valid object of class CombinedModelCMPNotHasExp", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y[1:4] <- NA
    model <- new("CMPVaryingNotUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 nuCMP = new("ParameterVector", runif(24)),
                 meanLogNuCMP = new("Parameter", rnorm(1)),
                 sdLogNuCMP = new("Scale", runif(1)),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelCMPNotHasExp",
             model = model,
             y = y)
    expect_true(validObject(x))
})


test_that("can create valid object of class CombinedModelCMPHasExp", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = letters[1:3], age = 0:3)))
    y[1:4] <- NA
    exposure <- y + 1
    model <- new("CMPVaryingUseExp",
                 theta = rbeta(n = 24, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(24),
                 nuCMP = new("ParameterVector", runif(24)),
                 meanLogNuCMP = new("Parameter", rnorm(1)),
                 sdLogNuCMP = new("Scale", runif(1)),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 2:4,
                                               dimnames = list(sex = c("f", "m"),
                                                               region = letters[1:3], age = 0:3))),
                 cellInLik = rep(TRUE, 24),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 ASigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 betas = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 betasOld = list(5, rnorm(2), rnorm(3), rnorm(4)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3), rep(0, 4)),
                 betaEqualsMean = rep(FALSE, 4),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE, TRUE),
                 namesBetas = c("(Intercept)", c("sex", "region", "age")),
                 margins = list(0L, 1L, 2L, 3L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3)),
                                    new("ExchNormZero", J = new("Length", 4L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 4))),
                 iteratorBetas = BetaIterator(dim = 2:4, margins = list(0L, 1L, 2L, 3L)),
                 dims = list(0L, 2L, 3L, 4L),
                 mu = rnorm(24))
    x <- new("CombinedModelCMPHasExp",
             model = model,
             y = y,
             exposure = exposure)
    expect_true(validObject(x))
})




## CombinedCounts ##########################################################################

test_that("can create valid object of class CombinedCountsPoissonNotHasExp", {
    BetaIterator <- demest:::BetaIterator
    ## datasets all complete
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 3:2,
                                               dimnames = list(age = 0:2, sex = c("f", "m")))),
                 cellInLik = rep(TRUE, 6),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, 5L),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            metadataY = y[1:2,]@metadata,
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            cellInLik = rep(TRUE, 4),
                            scaleTheta = new("Scale", 0.1),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsPoissonNotHasExp",
             y = y,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
    ## dataset1 has missing values
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = c(3, 2),
                                               dimnames = list(age = 0:2, sex = c("f", "m")))),
                 scaleTheta = new("Scale", 0.1),
                 cellInLik = rep(TRUE, 6),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, NA),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            metadataY = y[1:2,]@metadata,
                            cellInLik = rep(TRUE, 4),
                            scaleTheta = new("Scale", 0.1),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsPoissonNotHasExp",
             y = y,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
})

test_that("validity tests inherited for CombinedCountsPoissonNotHasExp inherited from DataModels work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    model <- new("PoissonVaryingNotUseExp",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 3:2,
                                               dimnames = list(age = 0:2, sex = c("f", "m")))),
                 scaleTheta = new("Scale", 0.1),
                 cellInLik = rep(TRUE, 6),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, 5L),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            metadataY = y[1:2,]@metadata,
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            cellInLik = rep(TRUE, 4),
                            scaleTheta = new("Scale", 0.1),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsPoissonNotHasExp",
             y = y,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
    ## all elements of 'dataModels' have class "Model"
    x.wrong <- x
    x.wrong@dataModels[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'dataModels' has elements not of class \"Model\"")
    ## all elements of 'dataModels' have class "UseExposure"
    x.wrong <- x
    x.wrong@dataModels[[1]] <- new("PoissonVaryingNotUseExp")
    expect_error(validObject(x.wrong),
                 "'dataModels' has elements not of class \"UseExposure\"")
    ## 'dataModels' does not have names
    x.wrong <- x
    names(x.wrong@dataModels)[1] <- "a"
    expect_error(validObject(x.wrong),
                 "'dataModels' has names")
    ## all elements of 'datasets' have class "Counts"
    x.wrong <- x
    x.wrong@datasets[[1]] <- as(x.wrong@datasets[[1]], "Values")
    expect_error(validObject(x.wrong),
                 "'datasets' has elements not of class \"Counts\"")
    ## all elements of 'datasets' have type "integer"
    x.wrong <- x
    x.wrong@datasets[[1]] <- toDouble(x.wrong@datasets[[1]])
    expect_error(validObject(x.wrong),
                 "'datasets' has elements not of type \"integer\"")
    ## 'datasets' does not have names
    x.wrong <- x
    names(x.wrong@datasets)[1] <- "a"
    expect_error(validObject(x.wrong),
                 "'datasets' has names")
    ## 'namesDatasets' has no missing values
    x.wrong <- x
    x.wrong@namesDatasets[1] <- NA
    expect_error(validObject(x.wrong),
                 "'namesDatasets' has missing values")
    ## 'namesDatasets' has no blanks
    x.wrong <- x
    x.wrong@namesDatasets[1] <- ""
    expect_error(validObject(x.wrong),
                 "'namesDatasets' has blanks")
    ## 'namesDatasets' has no duplicates
    x.wrong <- x
    x.wrong@namesDatasets[2] <- x.wrong@namesDatasets[1]
    expect_error(validObject(x.wrong),
                 "'namesDatasets' has duplicates")
    ## all elements of 'transforms' have class "CollapseTransformExtra"
    x.wrong <- x
    x.wrong@transforms[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'transforms' has elements not of class \"CollapseTransformExtra\"")
    ## all elements of 'transforms' have same 'dimBefore'
    x.wrong <- x
    x.wrong@transforms[[1]] <- new("CollapseTransformExtra",
                                   indices = list(1:4, 1:2),
                                   dims = 1:2,
                                   dimBefore = c(4L, 2L),
                                   dimAfter = c(4L, 2L),
                                   multiplierBefore = c(1L, 4L),
                                   multiplierAfter = c(1L, 4L),
                                   invIndices = list(list(1L, 2L, 3L, 4L), list(1L, 2L)))
    expect_error(validObject(x.wrong))
    ## 'transforms' does not have names
    x.wrong <- x
    names(x.wrong@transforms) <- "wrong"
    expect_error(validObject(x.wrong),
                 "'transforms' has names")
    ## 'dataModels' and 'datasets' have same length
    x.wrong <- x
    x.wrong@datasets <- x.wrong@datasets[-1]
    expect_error(validObject(x.wrong))
    ## 'dataModels' and 'namesDatasets' have same length
    x.wrong <- x
    x.wrong@namesDatasets <- x.wrong@namesDatasets[-1]
    expect_error(validObject(x.wrong),
                 "'dataModels' and 'namesDatasets' have different lengths")
    ## 'dataModels' and 'transforms' have same length
    x.wrong <- x
    x.wrong@transforms <- x.wrong@transforms[-1]
    expect_error(validObject(x.wrong),
                 "'dataModels' and 'transforms' have different lengths")
    ## 'transforms' and 'dimAfter' consistent with datasets
    x.wrong <- x
    x.wrong@transforms[[1]] <- new("CollapseTransformExtra",
                                   indices = list(1:3, c(1L, 0L)),
                                   dims = 1:2,
                                   dimBefore = c(3L, 2L),
                                   dimAfter = c(3L, 1L),
                                   multiplierBefore = c(1L, 3L),
                                   multiplierAfter = c(1L, 3L),
                                   invIndices = list(list(1L, 2L, 3L), list(1L)))
    expect_error(validObject(x.wrong),
                 "'dataset' and 'transform' for \"dataset1\" inconsistent")
})

test_that("can create valid object of class CombinedCountsPoissonHasExp", {
    BetaIterator <- demest:::BetaIterator
    ## no missing values
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    exposure <- y + 1
    model <- new("PoissonVaryingUseExp",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 3:2,
                                               dimnames = list(age = 0:2, sex = c("f", "m")))),
                 cellInLik = rep(TRUE, 6),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, 5L),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            metadataY = y[1:2,]@metadata,
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            cellInLik = rep(TRUE, 4),
                            scaleTheta = new("Scale", 0.1),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsPoissonHasExp",
             y = y,
             exposure = exposure,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
    ## has missing values
    y[1] <- NA
    x <- new("CombinedCountsPoissonHasExp",
             y = y,
             exposure = exposure,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
})

test_that("validity tests for CombinedCountsPoissonHasExp inherited from CombinedCountsPoissonHasExp work", {
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    exposure <- y + 1
    model <- new("PoissonVaryingUseExp",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 strucZeroArray = Counts(array(1L,
                                               dim = 3:2,
                                               dimnames = list(age = 0:2, sex = c("f", "m")))),
                 cellInLik = rep(TRUE, 6),
                 scaleTheta = new("Scale", 0.1),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, 5L),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            metadataY = y[1:2,]@metadata,
                            cellInLik = rep(TRUE, 4),
                            scaleTheta = new("Scale", 0.1),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsPoissonHasExp",
             y = y,
             exposure = exposure,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
    ## 'exposure' has type "double"
    x.wrong <- x
    x.wrong@exposure <- toInteger(x.wrong@exposure, force = TRUE)
    expect_error(validObject(x.wrong),
                 "'exposure' does not have type \"double\"")
})

test_that("can create valid object of class CombinedCountsBinomial", {
    BetaIterator <- demest:::BetaIterator
    ## no missing values
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    exposure <- y + 1
    model <- new("BinomialVarying",
                 theta = rbeta(n = 6, shape1 = 5, shape2 = 5),
                 thetaTransformed = rnorm(6),
                 metadataY = y@metadata,
                 scaleTheta = new("Scale", 0.1),
                 cellInLik = rep(TRUE, 6),
                 nAcceptTheta = new("Counter", 0L),
                 lower = -Inf,
                 upper = Inf,
                 maxAttempt = 100L,
                 nFailedPropTheta = new("Counter", 0L),
                 sigma = new("Scale", 1),
                 sigmaMax = new("Scale", 5),
                 ASigma = new("Scale", 1),
                 betas = list(5, rnorm(2), rnorm(3)),
                 betasOld = list(5, rnorm(2), rnorm(3)),
                 meansBetas = list(0, rep(0, 2), rep(0, 3)),
                 variancesBetas = list(0, rep(0, 2), rep(0, 3)),
                 gradientBetas = list(0, rep(0, 2), rep(0, 3)),
                 momentumBetas = list(0, rep(0, 2), rep(0, 3)),
                 betaEqualsMean = rep(FALSE, 3),
                 useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                 namesBetas = c("(Intercept)", "age", "sex"),
                 margins = list(0L, 1L, 2L),
                 priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                    new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 2)),
                                    new("ExchNormZero", J = new("Length", 3L), tauMax = new("Scale", 5),
                                        isSaturated = new("LogicalFlag", FALSE),
                                        allStrucZero = rep(FALSE, 3))),
                 iteratorBetas = BetaIterator(dim = 2:3, margins = list(0L, 1L, 2L)),
                 dims = list(0L, 2L, 3L),
                 mu = rnorm(6))
    datasets <- list(Counts(array(c(1:5, 5L),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(2L,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("f", "m")))))
    data.models <- list(new("PoissonBinomialMixture", prob = 0.9, metadataY = datasets[[1]]@metadata),
                        new("PoissonVaryingUseExp",
                            theta = rbeta(n = 4, shape1 = 5, shape2 = 5),
                            thetaTransformed = rnorm(4),
                            strucZeroArray = Counts(array(1L,
                                                          dim = c(2, 2),
                                                          dimnames = list(age = 0:1, sex = c("f", "m")))),
                            metadataY = y[1:2,]@metadata,
                            scaleTheta = new("Scale", 0.1),
                            cellInLik = rep(TRUE, 4),
                            nAcceptTheta = new("Counter", 0L),
                            lower = -Inf,
                            upper = Inf,
                            maxAttempt = 100L,
                            nFailedPropTheta = new("Counter", 0L),
                            sigma = new("Scale", 1),
                            sigmaMax = new("Scale", 5),
                            ASigma = new("Scale", 1),
                            betas = list(5, rnorm(2), rnorm(2)),
                            betasOld = list(5, rnorm(2), rnorm(2)),
                            meansBetas = list(0, rep(0, 2), rep(0, 2)),
                            variancesBetas = list(0, rep(0, 2), rep(0, 2)),
                            gradientBetas = list(0, rep(0, 2), rep(0, 2)),
                            momentumBetas = list(0, rep(0, 2), rep(0, 2)),
                            betaEqualsMean = rep(FALSE, 3),
                            useHMCToUpdateBeta = c(FALSE, FALSE, TRUE),
                            namesBetas = c("(Intercept)", "age", "sex"),
                            margins = list(0L, 1L, 2L),
                            priorsBetas = list(new("ExchFixed", isSaturated = new("LogicalFlag", FALSE), allStrucZero = FALSE),
                                               new("ExchFixed", J = new("Length", 2L), isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2)),
                                               new("ExchNormZero", J = new("Length", 2L), tauMax = new("Scale", 5),
                                                   isSaturated = new("LogicalFlag", FALSE),
                                                   allStrucZero = rep(FALSE, 2))),
                            iteratorBetas = BetaIterator(dim = c(2L, 2L), margins = list(0L, 1L, 2L)),
                            dims = list(0L, 2L, 2L),
                            mu = rnorm(4)))
    namesDatasets <- c("dataset1", "dataset2")
    transforms <- list(new("CollapseTransformExtra",
                           indices = list(1:3, 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = 3:2,
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 3L),
                           invIndices = list(list(1L, 2L, 3L), list(1L, 2L))),
                       new("CollapseTransformExtra",
                           indices = list(c(1:2, 0L), 1:2),
                           dims = 1:2,
                           dimBefore = 3:2,
                           dimAfter = c(2L, 2L),
                           multiplierBefore = c(1L, 3L),
                           multiplierAfter = c(1L, 2L),
                           invIndices = list(list(1L, 2L), list(1L, 2L))))
    x <- new("CombinedCountsBinomial",
             y = y,
             exposure = exposure,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
    ## has missing values
    y[1] <- NA
    x <- new("CombinedCountsBinomial",
             y = y,
             exposure = exposure,
             model = model,
             dataModels = data.models,
             datasets = datasets,
             namesDatasets = namesDatasets,
             transforms = transforms)
    expect_true(validObject(x))
})



## test iMethodCombined

test_that("Combined classes have correct value for iMethodCombined", {
    x <- new("CombinedModelBinomial")
    expect_identical(x@iMethodCombined, 1L)
    x <- new("CombinedModelNormal")
    expect_identical(x@iMethodCombined, 2L)
    x <- new("CombinedModelPoissonNotHasExp")
    expect_identical(x@iMethodCombined, 3L)
    x <- new("CombinedModelPoissonHasExp")
    expect_identical(x@iMethodCombined, 4L)
})

 
