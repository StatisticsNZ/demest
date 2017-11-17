
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
    spec <- Model(y ~ Poisson(mean ~ age + time, useExpose = FALSE))
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
    spec <- Model(y ~ Poisson(mean ~ age + time, useExpose = FALSE))
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
    spec <- Model(y ~ Poisson(mean ~ sex * age + time, useExpose = FALSE))
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
    spec <- Model(y ~ Poisson(mean ~ sex * age + time, useExpose = FALSE))
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
    model <- Model(y ~ Poisson(mean ~ age, useExpose = FALSE))
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "observationModels")) {
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
    model <- Model(y ~ Poisson(mean ~ age, useExpose = FALSE))
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
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
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "observationModels")) {
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
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
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
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (i in 1:5)
        x1 <- updateCombined(x1)
    for (name in c("model", "y", "observationModels")) {
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
    observationModels <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
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
                               observationModels = observationModels,
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


## Accounts ##############################################################################

test_that("diffLogDensAccount works with CombinedAccountMovementsHasAge", {
    diffLogDensAccount <- demest:::diffLogDensAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    set.seed(1)
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensAccount(x1)
            expect_true(is.numeric(ans.obtained))
            expect_true(!is.na(ans.obtained))
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensAccount give same answer with CombinedAccountMovementsHasAge", {
    diffLogDensAccount <- demest:::diffLogDensAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensAccount(x1, useC = FALSE)
            set.seed(seed)
            ans.C.generic <- diffLogDensAccount(x1, useC = TRUE, useSpecific = FALSE)
            set.seed(seed)
            ans.C.specific <- diffLogDensAccount(x1, useC = TRUE, useSpecific = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C.generic)
            else
                expect_equal(ans.R, ans.C.generic)
            expect_identical(ans.C.specific, ans.C.generic)
        }
    }
    if (!updated)
        warning("not updated")
})


## diffLogLikAccount

test_that("diffLogLikAccount works with CombinedAccountMovementsHasAge", {
    diffLogLikAccount <- demest:::diffLogLikAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogLikAccount(x1)
            expect_true(is.numeric(ans.obtained))
            expect_true(!is.na(ans.obtained))
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogLikAccount give same answer with CombinedAccountMovementsHasAge", {
    diffLogLikAccount <- demest:::diffLogLikAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            set.seed(seed)
            ans.R <- diffLogLikAccount(x1, useC = FALSE)
            set.seed(seed)
            ans.C.generic <- diffLogLikAccount(x1, useC = TRUE, useSpecific = FALSE)
            set.seed(seed)
            ans.C.specific <- diffLogLikAccount(x1, useC = TRUE, useSpecific = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C.generic)
            else
                expect_equal(ans.R, ans.C.generic)
            expect_identical(ans.C.specific, ans.C.generic)
        }
    }
    if (!updated)
        warning("not updated")
})

## updateProposalAccount

test_that("updateProposalAccount works with CombinedAccountMovementsHasAge", {
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccount give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccount(x0, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- updateProposalAccount(x0, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- updateProposalAccount(x0, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
    if (!updated)
        warning("not updated")
})

test_that("updateValuesAccount works with CombinedAccountMovements", {
    updateValuesAccount <- demest:::updateValuesAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        while (!x@generatedNewProposal@.Data)
            x <- updateProposalAccount(x)
        ans.obtained <- updateValuesAccount(x)
        ans.expected <- x
        ans.expected <- updateCellMove(x)
        ans.expected <- updateSubsequentPopnMove(ans.expected)
        ans.expected <- updateSubsequentExpMove(ans.expected)
        expect_identical(ans.obtained, ans.expected)
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateValuesAccount give same answer with CombinedAccountMovements", {
    updateValuesAccount <- demest:::updateValuesAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        while (!x@generatedNewProposal@.Data)
            x <- updateProposalAccount(x)
        ans.R <- updateValuesAccount(x, useC = FALSE)
        ans.C.generic <- updateValuesAccount(x, useC = TRUE, useSpecific = FALSE)
        ans.C.specific <- updateValuesAccount(x, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
    if (!updated)
        warning("not updated")
})

