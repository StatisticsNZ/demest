
context("Combined-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## predictCombined - CombinedModel ####################################################

test_that("test that predictCombined gives valid answer with CombinedModelNormal", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Values(array(rnorm(n = 30),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(runif(n = 30),
                            dim = c(2, 3, 5),
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                      dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = weights)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = FALSE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelNotUseExp(object = model, y = combined.pred@y)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelNormal", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Values(array(rnorm(n = 30),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(runif(n = 30),
                            dim = c(2, 3, 5),
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                      dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = weights)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = FALSE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelPoissonNotHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelNotUseExp(object = model, y = combined.pred@y)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelPoissonNotHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelBinomial", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 30, size = exposure, prob = 0.5)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelUseExp(object = model, y = combined.pred@y, exposure = combined.pred@exposure)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelBinomial", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 30, size = exposure, prob = 0.5)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(runif(n = 30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelUseExp(object = model, y = combined.pred@y, exposure = combined.pred@exposure)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(runif(n = 30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})


## updateCombined - CombinedModel #####################################################

## Assume that underlying updating functions work correctly.  Only check that
## appropriate slots are updated.

test_that("updateCombined updates appropriate slots with CombinedModelNormal", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    y <- Counts(array(rnorm(24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelNormal", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    y <- Counts(array(rnorm(24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateCombined updates appropriate slots with CombinedModelPoissonNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    set.seed(1)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelPoissonNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateCombined updates appropriate slots with CombinedModelBinomial", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Binomial(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelBinomial", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                                 dim = 2:4,
                                 dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                           dimscales = c(time = "Intervals"))
        y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                          dim = 2:4,
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                    dimscales = c(time = "Intervals"))
        exposure[1] <- NA
        y[1] <- NA
        spec <- Model(y ~ Binomial(mean ~ sex * age + time))
        x <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
        set.seed(seed + 1)
        ans.R <- updateCombined(x, useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.C.specific, ans.R)
        else
            expect_equal(ans.C.specific, ans.R)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("updateCombined updates appropriate slots with CombinedModelPoissonUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(runif(n = 24, max = 20),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelPoissonUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    exposure <- Counts(array(runif(n = 24, max = 20),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})



## CombinedCounts #####################################################################

test_that("updateCombined method for CombinedCountsPoissonNotHasExp updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = NULL,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "observation")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsPoissonNotHasExp give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = NULL,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})

test_that("updateCombined method for CombinedCountsPoissonHasExp updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- Counts(array(runif(12, max = 20),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "observation")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "exposure", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsPoissonHasExp give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- Counts(array(runif(12, max = 20),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})

test_that("updateCombined method for CombinedCountsBinomial updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- y + y
    model <- Model(y ~ Binomial(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "observation")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "exposure", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsBinomial give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- y + y
    model <- Model(y ~ Binomial(mean ~ age))
    observation <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               observation = observation,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})
