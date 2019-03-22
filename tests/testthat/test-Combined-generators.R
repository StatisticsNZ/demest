
context("Combined-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

## Assume that lower-level generator functions are working correctly.
## Only check that valid objects are created, and that the error-checking
## works as expected.

## CombinedModel ######################################################################

test_that("initialCombinedModel creates object of class CombinedModelBinomial from valid inputs", {
    initialCombinedModel <- demest:::initialCombinedModel
    spec <- Model(y ~ Binomial(mean ~ sex + time))
    exposure <- Counts(array(as.integer(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    x <- initialCombinedModel(object = spec, y = y, exposure = exposure, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelBinomial")
    expect_is(x@model, "BinomialVarying")
    ## y has NA
    spec <- Model(y ~ Binomial(mean ~ sex + time))
    exposure <- Counts(array(as.integer(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    y[1:3] <- NA
    exposure[1:2] <- NA
    x <- initialCombinedModel(object = spec, y = y, exposure = exposure, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelBinomial")
    expect_is(x@model, "BinomialVarying")
    expect_identical(sum(is.na(x@y)), 3L)
    expect_identical(sum(is.na(x@exposure)), 2L)
})

test_that("initialCombinedModel gives apprioriate warnings and errors with class SpecBinomial", {
    initialCombinedModel <- demest:::initialCombinedModel
    spec <- Model(y ~ Binomial(mean ~ 1))
    exposure <- Counts(array(rpois(n = 12, lambda = 100),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    weights <- Counts(array(rbeta(n = 12, shape1 = 1, shape2 = 1),
                             dim = c(2, 6),
                            dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                      dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 12, size = exposure, prob = 0.8),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    ## weights argument supplied
    expect_warning(initialCombinedModel(object = spec, y = y, exposure = exposure, weights = weights),
                   "'weights' argument ignored when distribution is Binomial")
    ## y is Values
    expect_error(initialCombinedModel(object = spec, y = as(y, "Values"), exposure = exposure,
                                      weights = NULL),
                 "'y' has class \"Values\" : in a Binomial model 'y' must have class \"Counts\"")
    ## no exposure argument supplied
    expect_error(initialCombinedModel(object = spec, y = y, exposure = NULL,
                                      weights = NULL),
                 "a Binomial model requires an 'exposure' argument, but no 'exposure' argument supplied")
    ## y > exposure
    y.wrong <- y
    y.wrong[1] <- exposure[1] + 1
    expect_error(initialCombinedModel(object = spec, y = y.wrong, exposure = exposure,
                                      weights = NULL),
                 "'y' greater than 'exposure'")
})

test_that("initialCombinedModel creates object of class CombinedModelNormal from valid inputs", {
    initialCombinedModel <- demest:::initialCombinedModel
    ## model is NormalVaryingVarsigmaKnown
    spec <- Model(y ~ Normal(mean ~ sex + time, sd = 2.3))
    y <- Counts(array(rnorm(n = 12),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelNormal")
    expect_is(x@model, "NormalVaryingVarsigmaKnown")
    expect_identical(x@model@w,
                     rep(1, times = 12))
    ## y has NA
    spec <- Model(y ~ Normal(mean ~ sex + time, sd = 2.3))
    y <- Counts(array(rnorm(n = 12),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    y[3] <- NA
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelNormal")
    expect_is(x@model, "NormalVaryingVarsigmaKnown")
    expect_identical(x@model@w,
                     rep(1, times = 12))
    expect_identical(sum(is.na(x@y)), 1L)
})

test_that("initialCombinedModel gives apprioriate warning with class SpecNormal", {
    initialCombinedModel <- demest:::initialCombinedModel
    spec <- Model(y ~ Normal(mean ~ 1))
    exposure <- Counts(array(rpois(n = 12, lambda = 100),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    weights <- Counts(array(rbeta(n = 12, shape1 = 1, shape2 = 1),
                            dim = c(2, 6),
                            dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                      dimscales = c(time = "Intervals"))
    y <- Counts(array(as.double(rbinom(n = 12, size = exposure, prob = 0.8)),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    expect_warning(initialCombinedModel(object = spec, y = y, exposure = exposure, weights = weights),
                   "'exposure' argument ignored when distribution is Normal")
})

test_that("initialCombinedModel creates object of class CombinedModelPoisson from valid inputs", {
    initialCombinedModel <- demest:::initialCombinedModel
    spec <- Model(y ~ Poisson(mean ~ sex + time, useExpose = FALSE))
    y <- Counts(array(as.integer(rbinom(n = 12, size = 10, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelPoissonNotHasExp")
    expect_is(x@model, "PoissonVaryingNotUseExp")
    ## y has NA
    spec <- Model(y ~ Poisson(mean ~ sex + time, useExpose = FALSE))
    exposure <- Counts(array(as.numeric(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    y[1:5] <- NA
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelPoissonNotHasExp")
    expect_is(x@model, "PoissonVaryingNotUseExp")
    expect_identical(sum(is.na(x@y)), 5L)
})

test_that("initialCombinedModel gives apprioriate warnings and errors with class SpecPoisson", {
    initialCombinedModel <- demest:::initialCombinedModel
    ## 'weights' supplied
    spec <- Model(y ~ Poisson(mean ~ 1))
    exposure <- Counts(array(as.double(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    weights <- Counts(array(rbeta(n = 12, shape1 = 1, shape2 = 1),
                            dim = c(2, 6),
                            dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                      dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    expect_warning(initialCombinedModel(object = spec, y = y, exposure = exposure, weights = weights),
                   "'weights' argument ignored when distribution is Poisson")
    ## y is Counts
    spec <- Model(y ~ Poisson(mean ~ 1))
    exposure <- Counts(array(as.double(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.double(rbinom(n = 12, size = exposure, prob = 0.8)),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    expect_error(initialCombinedModel(object = spec, y = as(y, "Values"),
                                      exposure = exposure, weights = NULL),
                 "'y' has class \"Values\" : in a Poisson model 'y' must have class \"Counts\"")
})

test_that("initialCombinedModel creates object of class CombinedModelCMP from valid inputs", {
    initialCombinedModel <- demest:::initialCombinedModel
    spec <- Model(y ~ CMP(mean ~ sex + time, useExpose = FALSE))
    y <- Counts(array(as.integer(rbinom(n = 12, size = 10, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelCMPNotHasExp")
    expect_is(x@model, "CMPVaryingNotUseExp")
    ## y has NA
    spec <- Model(y ~ CMP(mean ~ sex + time, useExpose = FALSE))
    exposure <- Counts(array(as.numeric(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                             dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    y[1:5] <- NA
    x <- initialCombinedModel(object = spec, y = y, exposure = NULL, weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelCMPNotHasExp")
    expect_is(x@model, "CMPVaryingNotUseExp")
    expect_identical(sum(is.na(x@y)), 5L)
})

test_that("initialCombinedModel gives apprioriate warnings and errors with class SpecCMP", {
    initialCombinedModel <- demest:::initialCombinedModel
    ## 'weights' supplied
    spec <- Model(y ~ CMP(mean ~ 1))
    exposure <- Counts(array(as.double(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    weights <- Counts(array(rbeta(n = 12, shape1 = 1, shape2 = 1),
                            dim = c(2, 6),
                            dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                      dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 12, size = exposure, prob = 0.8)),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    expect_warning(initialCombinedModel(object = spec, y = y, exposure = exposure, weights = weights),
                   "'weights' argument ignored when distribution is CMP")
    ## y is Counts
    spec <- Model(y ~ CMP(mean ~ 1))
    exposure <- Counts(array(as.double(rpois(n = 12, lambda = 100)),
                             dim = c(2, 6),
                             dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.double(rbinom(n = 12, size = exposure, prob = 0.8)),
                      dim = c(2, 6),
                      dimnames = list(sex = c("f", "m"), time = 2000:2005)),
                dimscales = c(time = "Intervals"))
    expect_error(initialCombinedModel(object = spec, y = as(y, "Values"),
                                      exposure = exposure, weights = NULL),
                 "'y' has class \"Values\" : in a CMP model 'y' must have class \"Counts\"")
})

## CombinedModelPredct ############################################################

test_that("initialCombinedModelSimulate creates object of class CombinedModelBinomial from valid inputs", {
    initialCombinedModelSimulate <- demest:::initialCombinedModelSimulate
    model <- Model(y ~ Binomial(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(error = Error(scale = HalfT(scale = 0.3))),
                   priorSD = HalfT(scale = 0.1))
    exposure <- CountsOne(1:10, labels = letters[1:10], name = "region")
    y <- CountsOne(rbinom(n = 10, size = 1:10, prob = 0.6),
                   labels = letters[1:10],
                   name = "region")
    x <- initialCombinedModelSimulate(object = model,
                                      y = y,
                                      exposure = exposure,
                                      weights = NULL)
    expect_true(validObject(x))
    expect_is(x, "CombinedModelBinomial")
    expect_is(x@model, "BinomialVarying")
    expect_true(all(is.na(x@y@.Data)))
})



## CombinedModelPredct ############################################################

test_that("test that initialCombinedModelPredict works with with CombinedModelNormal", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    y <- Values(array(rnorm(n = 30),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(runif(n = 30),
                            dim = c(2, 3, 5),
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                      dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ age + time))
    combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = weights)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = FALSE)
    expect_is(ans, "CombinedModelNormal")
    expect_is(ans@y, "Values")
    expect_true(all(is.na(ans@y)))
})

test_that("test that initialCombinedModelPredict works with with CombinedModelPoissonNotHasExp", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 20)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time, useExpose = FALSE))
    combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = TRUE)
    expect_is(ans, "CombinedModelPoissonNotHasExp")
    expect_is(ans@y, "Counts")
    expect_true(all(is.na(ans@y)))
})

test_that("test that initialCombinedModelPredict works with with CombinedModelBinomial", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(as.integer(rpois(n = 30, lambda = 10)),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 30, size = exposure, prob = 0.5)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ sex * age + time))
    combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = TRUE)
    expect_is(ans, "CombinedModelBinomial")
    expect_is(ans@y, "Counts")
    expect_true(all(is.na(ans@y)))
})

test_that("test that initialCombinedModelPredict works with with CombinedModelPoissonHasExp", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(runif(30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = TRUE)
    expect_is(ans, "CombinedModelPoissonHasExp")
    expect_is(ans@y, "Counts")
    expect_true(all(is.na(ans@y)))
})


test_that("test that initialCombinedModelPredict works with with CombinedModelCMPNotHasExp", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 20)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ CMP(mean ~ age + time, useExpose = FALSE))
    combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = TRUE)
    expect_is(ans, "CombinedModelCMPNotHasExp")
    expect_is(ans@y, "Counts")
    expect_true(all(is.na(ans@y)))
})


test_that("test that initialCombinedModelPredict works with with CombinedModelCMPHasExp", {
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(runif(30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ CMP(mean ~ sex * age + time))
    combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    ans <- initialCombinedModelPredict(combined = combined,
                                       along = 3L,
                                       labels = NULL,
                                       n = 2,
                                       covariates = NULL,
                                       aggregate = NULL,
                                       lower = NULL,
                                       upper = NULL,
                                       yIsCounts = TRUE)
    expect_is(ans, "CombinedModelCMPHasExp")
    expect_is(ans@y, "Counts")
    expect_true(all(is.na(ans@y)))
})




## CombinedCounts #####################################################################

test_that("initialCombinedCounts creates object of class CombinedCountsPoissonNotHasExp from valid inputs", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## no subtotals
    object <- Model(y ~ Poisson(mean ~ age * sex, useExpose = FALSE))
    y <- Counts(array(c(1:23, NA),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    datasets <- list(Counts(array(c(1:11, NA),
                                      dim = c(2, 3, 2),
                                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x <- initialCombinedCounts(object = object,
                               y = y,
                               exposure = NULL,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedCountsPoissonNotHasExp")
    expect_true(!any(is.na(x@y)))
    ## with subtotals
    object <- Model(y ~ Poisson(mean ~ age * sex, useExpose = FALSE))
    y <- Counts(array(c(1:18, rep(NA, 6)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    subtotals <- Counts(array(5:6,
                              dim = c(2, 1),
                              dimnames = list(sex = c("f", "m"), age = 3)))
    y <- attachSubtotals(y, subtotals = subtotals)
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x <- initialCombinedCounts(object = object,
                               y = y,
                               exposure = NULL,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedCountsPoissonNotHasExp")
    expect_true(!any(is.na(x@y)))
    expect_true(is(x@y, "CountsWithSubtotalsInternal"))
})

test_that("initialCombinedCounts creates object of class CombinedCountsPoissonHasExp from valid inputs", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## no subtotals
    object <- Model(y ~ Poisson(mean ~ age * sex))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x <- initialCombinedCounts(object = object,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedCountsPoissonHasExp")
    expect_true(!any(is.na(x@y)))
    ## with subtotals
    object <- Model(y ~ Poisson(mean ~ age * sex))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    exposure <- y + 2
    y[19:24] <- NA
    subtotals <- Counts(array(5:6,
                              dim = c(2, 1),
                              dimnames = list(sex = c("f", "m"), age = 3)))
    y <- attachSubtotals(y, subtotals = subtotals)
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x <- initialCombinedCounts(object = object,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedCountsPoissonHasExp")
    expect_true(!any(is.na(x@y)))
    expect_true(is(x@y, "CountsWithSubtotalsInternal"))
})

test_that("initialCombinedCounts creates object of class CombinedCountsBinomial from valid inputs", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    object <- Model(y ~ Binomial(mean ~ age * sex))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x <- initialCombinedCounts(object = object,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedCountsBinomial")
    expect_true(!any(is.na(x@y)))
})


test_that("initialCombinedCounts throws appropriate errors with CombinedCountsBinomial", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    object <- Model(y ~ Binomial(mean ~ age * sex))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, age = 0:3)))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, age = 2:3))),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, age = 0:3))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    ## y > exposure
    y.wrong <- y
    y.wrong[1] <- exposure[1] + 1
    expect_error(initialCombinedCounts(object = object,
                                       y = y.wrong,
                                       exposure = exposure,
                                       dataModels = data.models,
                                       datasets = datasets,
                                       namesDatasets = namesDatasets,
                                       transforms = transforms),
                 "'y' greater than 'exposure'")
})

## initialCombinedCountsPredict #############################################################

test_that("initialCombinedCountsPredict creates object of class CombinedCountsPoissonHasExp from valid inputs", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    initialCombinedCountsPredict <- demest:::initialCombinedCountsPredict
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    object <- Model(y ~ Poisson(mean ~ sex * region))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, time = 0:3)),
                dimscales = c(time = "Intervals"))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, time = 2:3)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, time = 0:3)),
                            dimscales = c(time = "Intervals")))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ time + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x.est <- initialCombinedCounts(object = object,
                                   y = y,
                                   exposure = exposure,
                                   dataModels = data.models,
                                   datasets = datasets,
                                   namesDatasets = namesDatasets,
                                   transforms = transforms)
    expect_true(validObject(x.est))
    expect_is(x.est, "CombinedCountsPoissonHasExp")
    exposure.pred <- extrapolate(exposure, labels = c("4", "5"))[,,5:6]
    x.pred <- initialCombinedCountsPredict(x.est,
                                           along = 3L,
                                           labels = NULL,
                                           n = 2L,
                                           exposure = exposure.pred,
                                           covariates = list(),
                                           aggregate = list(),
                                           lower = list(),
                                           upper = list())
    expect_is(x.est, "CombinedCountsPoissonHasExp")
    expect_true(validObject(x.pred))
})

## CombinedAccount ##########################################################################

test_that("initialCombinedAccount creates object of class CombinedAccountMovements from valid inputs", {
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## no age, single dimension
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                dataModels = data.models,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedAccountMovements")
    expect_identical(x@modelUsesExposure, c(FALSE, TRUE, TRUE))
    ## with age and internal
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 5, 2, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:5,
                                                time = c("2001-2005", "2006-2010"),
                                                triangle = c("Lower", "Upper"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 5, 2, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:5,
                                               time = c("2001-2005", "2006-2010"),
                                               triangle = c("Lower", "Upper"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 5, 2, 2),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     reg = 1:5,
                                                     time = c("2001-2005", "2006-2010"),
                                                     triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         entries = list(immigration = immigration),
                         exits = list(deaths = deaths, emigration = emigration),
                         net = list(reclassification = reclassification))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(immigration ~ Poisson(mean ~ age)),
                         Model(deaths ~ Poisson(mean ~ 1)),
                         Model(emigration ~ Poisson(mean ~ age)),
                         Model(reclassification ~ Normal(mean ~ 1)))
    weights.reclass <- Counts(array(1,
                                    dim = c(3, 2, 5, 2, 2),
                                    dimnames = list(age = c("0-4", "5-9", "10+"),
                                                    sex = c("m", "f"),
                                                    reg = 1:5,
                                                    time = c("2001-2005", "2006-2010"),
                                                    triangle = c("Lower", "Upper"))))
    systemWeights <- list(NULL, NULL, NULL, NULL, NULL, NULL, weights.reclass)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    arrivals <- collapseDimension(immigration, dimension = "sex")
    departures <- collapseDimension(emigration, dimension = "sex")
    datasets <- list(census, register, reg.births, address.change, reg.deaths, arrivals, departures)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths", "arrivals", "departures")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"),
                              Model(arrivals ~ PoissonBinomial(prob = 0.95), series = "immigration"),
                              Model(departures ~ PoissonBinomial(prob = 0.95), series = "emigration"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 4L, 3L, 5L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE),
                       makeTransform(x = components(account, "immigration"), y = datasets[[6]], subset = TRUE),
                       makeTransform(x = components(account, "emigration"), y = datasets[[7]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                dataModels = data.models,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedAccountMovementsHasAge")
})



test_that("initialCombinedAccountSimulate creates object of class CombinedAccountMovements from valid inputs", {
    initialCombinedAccount <- demest:::initialCombinedAccount
    initialCombinedAccountSimulate <- demest:::initialCombinedAccountSimulate
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    checkSystemModelsSuitableForSimulation <- demest:::checkSystemModelsSuitableForSimulation
    checkDataModelsSuitableForSimulation <- demest:::checkDataModelsSuitableForSimulation
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 1),
                               time ~ DLM(level = NULL,
                                          trend = Trend(initial = Initial(sd = 0.1),
                                                        scale = HalfT(scale = 0.1)),
                                          damp = NULL,
                                          error = Error(scale = HalfT(scale = 0.1))),
                               priorSD = HalfT(scale = 0.1)),
                         Model(births ~ Poisson(mean ~ time),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 1),
                               time ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                               priorSD = HalfT(scale = 0.1)),
                         Model(deaths ~ Poisson(mean ~ time),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 1),
                               time ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                               priorSD = HalfT(scale = 0.1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ time),
                              `(Intercept)` ~ ExchFixed(mean = -1, sd = 1),
                              time ~ DLM(level = NULL,
                                          trend = Trend(initial = Initial(sd = 0.1),
                                                        scale = HalfT(scale = 0.1)),
                                          damp = NULL,
                                          error = Error(scale = HalfT(scale = 0.1))),
                               priorSD = HalfT(scale = 0.1),
                              series = "deaths"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    checkSystemModelsSuitableForSimulation(systemModels = systemModels,
                                           account = account)
    checkDataModelsSuitableForSimulation(dataModels = data.models,
                                         datasets = datasets,
                                         namesDatasets = namesDatasets)
    x <- initialCombinedAccountSimulate(account = account,
                                        systemModels = systemModels,
                                        systemWeights = systemWeights,
                                        dataModels = data.models,
                                        seriesIndices = seriesIndices,
                                        datasets = datasets,
                                        namesDatasets = namesDatasets,
                                        transforms = transforms)
    expect_true(validObject(x))
    expect_is(x, "CombinedAccountMovements")
    expect_identical(x@systemModelsUseAg@.Data, FALSE)
    expect_identical(x@dataModelsUseAg@.Data, FALSE)
    ## add aggregate to model for births
    systemModels[[2]] <- Model(births ~ Poisson(mean ~ time),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 1),
                               time ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                               priorSD = HalfT(scale = 0.1),
                               aggregate = AgCertain(1))
    x <- initialCombinedAccountSimulate(account = account,
                                        systemModels = systemModels,
                                        systemWeights = systemWeights,
                                        dataModels = data.models,
                                        seriesIndices = seriesIndices,
                                        datasets = datasets,
                                        namesDatasets = namesDatasets,
                                        transforms = transforms)
    expect_identical(x@systemModelsUseAg@.Data, TRUE)
})






