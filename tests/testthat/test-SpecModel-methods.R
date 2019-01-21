
context("SpecModel-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## checkAndTidySimulatedYExposureWeights ##################################################

test_that("checkAndTidySimulatedYExposureWeights works", {
    checkAndTidySimulatedYExposureWeights <- demest:::checkAndTidySimulatedYExposureWeights
    makeCountsY <- demest:::makeCountsY
    model <- Model(y ~ Binomial(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(error = Error(scale = HalfT(scale = 0.3))),
                   priorSD = HalfT(scale = 0.1))
    y <- CountsOne(1:10, labels = letters[1:10], name = "region")
    exposure <- CountsOne(1:10, labels = letters[1:10], name = "region")
    weights <- CountsOne(1:10, labels = letters[1:10], name = "region")
    expect_is(model, "SpecBinomialVarying")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- checkAndTidySimulatedYExposureWeights(model = model,
                                                              y = NULL,
                                                              exposure = exposure,
                                                              weights = NULL)
        set.seed(seed)
        ans.expected <- list(y = makeCountsY(exposure),
                             exposure = exposure)
        expect_identical(ans.obtained, ans.expected)
    }
    expect_warning(checkAndTidySimulatedYExposureWeights(model = model,
                                                         y = y,
                                                         exposure = exposure,
                                                         weights = NULL))
    expect_warning(checkAndTidySimulatedYExposureWeights(model = model,
                                                         y = NULL,
                                                         exposure = exposure,
                                                         weights = weights))
})


## modelUsesWeights  ######################################################################

test_that("modelUsesWeights works", {
    modelUsesWeights <- demest:::modelUsesWeights
    spec <- new("SpecPoissonVarying")
    expect_false(modelUsesWeights(spec))
    spec <- new("SpecNormalVaryingVarsigmaKnown")
    expect_true(modelUsesWeights(spec))
})







