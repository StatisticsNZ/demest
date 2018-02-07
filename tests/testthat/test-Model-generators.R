
context("Model-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## addAg ########################################################################

## SpecAgPlaceholder

test_that("addAg works with BinomialVarying and SpecAgPlaceholder", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAg(model = model,
                          aggregate = new("SpecAgPlaceholder"))
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    expect_is(ans.obtained, "BinomialVarying")
})

test_that("addAg works with NormalVaryingVarsigmaKnown and SpecAgPlaceholder", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.5))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- addAg(model = model,
                          aggregate = new("SpecAgPlaceholder"))
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    expect_is(ans.obtained, "NormalVaryingVarsigmaKnown")
})

test_that("addAg works with NormalVaryingVarsigmaUnknown and SpecAgPlaceholder", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    ans.obtained <- addAg(model = model,
                          aggregate = new("SpecAgPlaceholder"))
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    expect_is(ans.obtained, "NormalVaryingVarsigmaUnknown")
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgPlaceholder", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- addAg(model = model,
                          aggregate = new("SpecAgPlaceholder"))
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    expect_is(ans.obtained, "PoissonVaryingNotUseExp")
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgPlaceholder", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    exposure <- y + 1L
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAg(model = model,
                          aggregate = new("SpecAgPlaceholder"))
    ans.expected <- model
    expect_identical(ans.obtained, ans.expected)
    expect_is(ans.obtained, "PoissonVaryingUseExp")
})

## SpecAgCertain

test_that("addAg works with BinomialVarying and SpecAgCertain", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgCertain(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "BinomialVaryingAgCertain")
    expect_identical(ans.obtained@slotsToExtract,
                     new("BinomialVaryingAgCertain")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("BinomialVaryingAgCertain")@iMethodModel)
})

test_that("addAg works with NormalVaryingVarsigmaKnown and SpecAgCertain", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.5))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgCertain(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaKnownAgCertain")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaKnownAgCertain")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaKnownAgCertain")@iMethodModel)
})

test_that("addAg works with NormalVaryingVarsigmaUnknown and SpecAgCertain", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgCertain(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaUnknownAgCertain")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaUnknownAgCertain")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaUnknownAgCertain")@iMethodModel)
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgCertain", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    defaultWeights <- Counts(array(1,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = NULL)
    aggregate <- AgCertain(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = defaultWeights)
    expect_is(ans.obtained, "PoissonVaryingNotUseExpAgCertain")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingNotUseExpAgCertain")@slotsToExtract)    
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingNotUseExpAgCertain")@iMethodModel)    
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgCertain", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgCertain(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "PoissonVaryingUseExpAgCertain")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingUseExpAgCertain")@slotsToExtract)    
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingUseExpAgCertain")@iMethodModel)    
})

## SpecAgNormal

test_that("addAg works with BinomialVarying and SpecAgNormal", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgNormal(value = 0.3, sd = 0.2)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "BinomialVaryingAgNormal")
    expect_identical(ans.obtained@slotsToExtract,
                     new("BinomialVaryingAgNormal")@slotsToExtract)    
    expect_identical(ans.obtained@iMethodModel,
                     new("BinomialVaryingAgNormal")@iMethodModel)    
})

test_that("addAg works with NormalVaryingVarsigmaKnown and SpecAgNormal", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.5))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgNormal(value = 0.3, sd = 0.2)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaKnownAgNormal")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaKnownAgNormal")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaKnownAgNormal")@iMethodModel)
})

test_that("addAg works with NormalVaryingVarsigmaUnknown and SpecAgNormal", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgNormal(value = 0.3, sd = 0.2)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaUnknownAgNormal")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaUnknownAgNormal")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaUnknownAgNormal")@iMethodModel)
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgNormal", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    defaultWeights <- Counts(array(1,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = NULL)
    aggregate <- AgNormal(value = 0.3, sd = 0.2)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = defaultWeights)
    expect_is(ans.obtained, "PoissonVaryingNotUseExpAgNormal")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingNotUseExpAgNormal")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingNotUseExpAgNormal")@iMethodModel)
})

test_that("addAg works with PoissonVaryingUseExp and SpecAgNormal", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgNormal(value = 0.3, sd = 0.2)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "PoissonVaryingUseExpAgNormal")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingUseExpAgNormal")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingUseExpAgNormal")@iMethodModel)
})


## SpecAgPoisson

test_that("addAg works with BinomialVarying and SpecAgPoisson", {
    addAg <- demest:::addAg
    expect_error(addAg(model = new("BinomialVarying"),
                       aggregate = new("SpecAgPoisson")),
                 "Poisson model for aggregate values can only be used with Poisson likelihood")
})

test_that("addAg works with NormalVaryingVarsigmaKnown and SpecAgPoisson", {
    addAg <- demest:::addAg
    expect_error(addAg(model = new("NormalVaryingVarsigmaKnown"),
                       aggregate = new("SpecAgPoisson")),
                 "Poisson model for aggregate values can only be used with Poisson likelihood")
})

test_that("addAg works with NormalVaryingVarsigmaUnknown and SpecAgPoisson", {
    addAg <- demest:::addAg
    expect_error(addAg(model = new("NormalVaryingVarsigmaUnknown"),
                       aggregate = new("SpecAgPoisson")),
                 "Poisson model for aggregate values can only be used with Poisson likelihood")
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgPoisson", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    defaultWeights <- Counts(array(1,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = NULL)
    aggregate <- AgPoisson(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = defaultWeights)
    expect_is(ans.obtained, "PoissonVaryingNotUseExpAgPoisson")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingNotUseExpAgPoisson")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingNotUseExpAgPoisson")@iMethodModel)
})

test_that("addAg works with PoissonVaryingNotUseExp and SpecAgPoisson", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgPoisson(value = 0.3)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "PoissonVaryingUseExpAgPoisson")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingUseExpAgPoisson")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingUseExpAgPoisson")@iMethodModel)
})


## SpecAgFun

test_that("addAg works with BinomialVarying and SpecAgFun", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgFun(value = 0.3, sd = 0.2, FUN = function(x, weights) 1)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "BinomialVaryingAgFun")
    expect_identical(ans.obtained@slotsToExtract,
                     new("BinomialVaryingAgFun")@slotsToExtract)    
    expect_identical(ans.obtained@iMethodModel,
                     new("BinomialVaryingAgFun")@iMethodModel)    
})

test_that("addAg works with NormalVaryingVarsigmaKnown and SpecAgFun", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.5))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgFun(value = 0.3, sd = 0.2, FUN = function(x, weights) 1)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaKnownAgFun")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaKnownAgFun")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaKnownAgFun")@iMethodModel)
})

test_that("addAg works with NormalVaryingVarsigmaUnknown and SpecAgFun", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, weights = weights)
    aggregate <- AgFun(value = 0.3, sd = 0.2, FUN = function(x, weights) 1)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = weights)
    expect_is(ans.obtained, "NormalVaryingVarsigmaUnknownAgFun")
    expect_identical(ans.obtained@slotsToExtract,
                     new("NormalVaryingVarsigmaUnknownAgFun")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("NormalVaryingVarsigmaUnknownAgFun")@iMethodModel)
})

test_that("addAg works with PoissonVaryingUseExp and SpecAgFun", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    y <- Counts(array(rbinom(n = 20, size = 20, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])),
                dimscales = c(age = "Intervals"))
    defaultWeights <- Counts(array(rbinom(n = 20, size = 20, prob = 0.5),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])),
                             dimscales = c(age = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = NULL)
    aggregate <- AgFun(value = 0.3, sd = 0.2, FUN = function(x, weights) 1)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = defaultWeights)
    expect_is(ans.obtained, "PoissonVaryingNotUseExpAgFun")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingNotUseExpAgFun")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingNotUseExpAgFun")@iMethodModel)
})

test_that("addAg works with PoissonVaryingUseExp and SpecAgFun", {
    addAg <- demest:::addAg
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    model <- initialModel(spec, y = y, exposure = exposure)
    aggregate <- AgFun(value = 0.3, sd = 0.2, FUN = function(x, weights) 1)
    ans.obtained <- addAg(model = model,
                          aggregate = aggregate,
                          defaultWeights = exposure)
    expect_is(ans.obtained, "PoissonVaryingUseExpAgFun")
    expect_identical(ans.obtained@slotsToExtract,
                     new("PoissonVaryingUseExpAgFun")@slotsToExtract)
    expect_identical(ans.obtained@iMethodModel,
                     new("PoissonVaryingUseExpAgFun")@iMethodModel)
})



## initialModel - Varying ##################################################################

test_that("initialModel creates object of class BinomialVarying from valid inputs", {
    initialModel <- demest:::initialModel
    makeLinearBetas <- demest:::makeLinearBetas
    jitterBetas <- demest:::jitterBetas
    makePriors <- demest:::makePriors
    ## main effect
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    m <- sum(y) / sum(exposure)
    nu <- m * (1 - m)
    sigma <- new("Scale", runif(1, 0, nu))
    theta <- rbeta(n = 20,
                   shape1 = m * (nu/sigma@.Data^2 - 1) + y,
                   shape2 = (1-m) * (nu/sigma@.Data^2 - 1)  + exposure - y)
    logit.theta <- array(log(theta / (1 - theta)), dim = dim(y), dimnames = dimnames(y))
    betas <- makeLinearBetas(theta = logit.theta, formula = prob ~ age + region)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_identical(x@sigma, sigma)
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@ASigma@.Data, 1)
    expect_identical(x@nuSigma@.Data, 7)
    expect_identical(x@sigmaMax@.Data, qhalft(p = 0.999, df = 7, scale = 1))
    expect_identical(x@lower, -Inf)
    expect_identical(x@upper, Inf)
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## intercept only
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ 1))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    m <- sum(y) / sum(exposure)
    nu <- m * (1 - m)
    sigma <- runif(1, 0, nu)
    theta <- rbeta(n = 20,
                   shape1 = m * (nu/sigma^2 -1) + y,
                   shape2 = (1-m) * (nu/sigma^2 - 1) + exposure - y)
    sigma <- new("Scale", sigma)
    betas <- list("(Intercept)" = mean(log(theta / (1 - theta))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(betas)
    betas <- jitterBetas(betas, priors)
    expect_identical(x@sigma, sigma)
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## lower and upper specified
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age + region), lower = 0.4, upper = 0.8)
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(all(x@theta >= 0.4))
    expect_true(all(x@theta <= 0.8))
    expect_equal(x@lower, log(0.4 / 0.6))
    expect_equal(x@upper, log(0.8 / 0.2))
    ## missing values
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1:4] <- NA
    exposure[1:3] <- NA
    spec <- Model(y ~ Binomial(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    m <- sum(y[5:20]) / sum(exposure[5:20])
    nu <- m * (1 - m)
    sigma <- runif(1, 0, nu)
    sigma <- new("Scale", sigma)
    y[1:4] <- 0
    exposure[1:4] <- 0
    theta <- rbeta(n = 20,
                   shape1 = m * (nu/sigma@.Data^2 - 1) + y,
                   shape2 = (1-m) * (nu/sigma@.Data^2 - 1)  + exposure - y)
    expect_identical(x@sigma, sigma)
    expect_identical(x@theta, theta)
    ## saturated model
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ age * region),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)    
})

test_that("initialModel methods for Varying model can cope with orig-dest dimtypes", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 25, lambda = 20),
                             dim = c(5, 5),
                             dimnames = list(reg_orig = c("a", "b", "c", "d", "e"),
                             reg_dest = c("a", "b", "c", "d", "e"))))
    y <- Counts(array(rbinom(n = 25, size = exposure, prob = 0.5),
                      dim = c(5, 5),
                      dimnames = list(reg_orig = c("a", "b", "c", "d", "e"),
                      reg_dest = c("a", "b", "c", "d", "e"))))
    spec <- Model(y ~ Binomial(mean ~ reg_orig + reg_dest))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
})


test_that("initialModel creates object of class CMPVaryingUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    makeSigmaInitialPoisson <- demest:::makeSigmaInitialPoisson
    loglm <- MASS:::loglm
    makePriors <- demest:::makePriors
    jitterBetas <- demest:::jitterBetas
    ## no missing values
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 0.05 * mean(exposure) + 0.95 * exposure)
    sigma <- runif(1, max = 1)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- loglm(~ age + region, theta)$param
    theta <- as.numeric(theta)
    sdNu <- runif(1, max = 1)
    meanNu <- rnorm(n = 1, mean = spec@meanMeanLogNuCMP, sd = spec@sdMeanLogNuCMP)
    nuCMP <- rnorm(n = length(theta), mean = meanNu, sd = sdNu)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priorsBetas = priors)
    expect_is(x, "CMPVaryingUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@ASigma, new("Scale", 1))
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@sigmaMax, new("Scale", qhalft(0.999, df = 7, scale = 1)))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    expect_identical(x@meanLogNuCMP, new("Parameter", meanNu))
    expect_identical(x@sdLogNuCMP, new("Scale", sdNu))
    ## intercept only
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ 1))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 0.05 * mean(exposure) + 0.95 * exposure)
    sigma <- runif(1, max = 1)
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## specify upper and lower
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age + region), lower = 0.01, upper = 2)
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(all(x@theta >= 0.01))
    expect_true(all(x@theta <= 2))
    expect_equal(x@lower, log(0.01))
    expect_equal(x@upper, log(2))
    ## has missing values
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1:5] <- NA
    spec <- Model(y ~ CMP(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    mean.y <- mean(y[6:20])
    mean.expose <- mean(exposure[6:20])
    shape <- 0.05 * mean.y + 0.95 * y
    shape[1:5] <- mean.y
    rate <- 0.05 * mean.expose + 0.95 * exposure
    rate[1:5] <- mean.expose
    theta <- rgamma(n = 20, shape = shape, rate = rate)
    sigma <- runif(1)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- loglm(~ age + region, theta)$param
    theta <- as.numeric(theta)
    sdNu <- runif(1, max = 1)
    meanNu <- rnorm(n = 1, mean = spec@meanMeanLogNuCMP, sd = spec@sdMeanLogNuCMP)
    nuCMP <- rnorm(n = length(theta), mean = meanNu, sd = sdNu)
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priorsBetas = priors)
    expect_is(x, "CMPVaryingUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@ASigma, new("Scale", 1))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## saturated model
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age * region),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)
    ## must have exposure
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age * region))
    expect_error(initialModel(spec, y = y, exposure = NULL),
                 "model 'y ~ CMP\\(mean ~ age \\* region\\)' uses exposure, but no 'exposure' argument supplied")
})




test_that("initialModel creates object of class NormalVaryingVarsigmaKnown from valid inputs", {
    initialModel <- demest:::initialModel
    makeLinearBetas <- demest:::makeLinearBetas
    makePriors <- demest:::makePriors
    jitterBetas <- demest:::jitterBetas
    ## y has no missing values
    varsigma <- 1.3
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = varsigma))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    sigma <- runif(n = 1, max = sd(y))
    theta <- rnorm(n = 20, mean = 0.5 * mean(y) + 0.5 * y, sd = sigma)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- makeLinearBetas(theta = theta, formula = mean ~ age + region)
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = sd(y),
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "NormalVaryingVarsigmaKnown")
    expect_identical(x@theta, theta)
    expect_identical(x@varsigma, new("Scale", varsigma))
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@ASigma, new("Scale", sd(y)))
    expect_identical(x@sigmaMax@.Data, qhalft(p = 0.999, df = 7, scale = sd(y)))
    expect_identical(x@lower, -Inf)
    expect_identical(x@upper, Inf)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@tolerance, 1e-5)
    ## intercept only
    varsigma <- 2.1
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ 1, sd = varsigma))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    sigma <- runif(1, max = sd(y))
    theta <- rnorm(n = 20, mean = 0.5 * mean(y) + 0.5 * y, sd = sigma)
    betas <- list(mean(theta))
    betas <- jitterBetas(betas)
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## lower and upper specified
    varsigma <- 1.3
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = varsigma), lower = 0, upper = 3)
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(all(x@theta > 0))
    expect_true(all(x@theta < 3))
    expect_equal(x@lower, 0)
    expect_equal(x@upper, 3)
    ## y has two values
    varsigma <- 2
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    y[-(1:2)] <- NA
    weights[-(1:2)] <- NA
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 2))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    sigma <- runif(1, max = sd(y, na.rm = T))
    y.bar <- mean(y, na.rm = T)
    mean <- rep(y.bar, 20)
    mean[1:2] <- 0.5 * y.bar + 0.5 * y[1:2]
    theta <- rnorm(20, mean = mean, sd = sigma)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- makeLinearBetas(theta = theta, formula = mean ~ age + region)
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = sd(y, na.rm = T),
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "NormalVaryingVarsigmaKnown")
    expect_identical(x@theta, theta)
    expect_identical(x@varsigma, new("Scale", varsigma))
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@ASigma, new("Scale", sd(y, na.rm = T)))
    expect_identical(x@lower, -Inf)
    expect_identical(x@upper, Inf)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@tolerance, 1e-5)
    ## maxSigma specified
    varsigma <- 1.3
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = varsigma),
                  priorSD = HalfT(max = 1000))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    expect_identical(x@sigmaMax@.Data, 1000)
    ## saturated model
    y <- Values(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age * region, sd = 1),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    x <- initialModel(spec, y = y, weights = weights)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)
})

test_that("initialModel creates object of class NormalVaryingVarsigmaUnknown from valid inputs", {
    initialModel <- demest:::initialModel
    rinvchisq1 <- demest:::rinvchisq1
    makeLinearBetas <- demest:::makeLinearBetas
    makePriors <- demest:::makePriors
    jitterBetas <- demest:::jitterBetas
    ## no missing values
    y <- Counts(array(rnorm(n = 20, mean = 0),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region),
                  priorSD = HalfT(max = 20))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    I <- length(y)
    varsigma <- runif(1, max = sd(y))
    sigma <- runif(1, max = sd(y))
    theta <- rnorm(n = 20, mean = 0.5 * mean(y) + 0.5 * y, sd = sigma)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- makeLinearBetas(theta = theta, formula = mean ~ age + region)
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = sd(y),
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "NormalVaryingVarsigmaUnknown")
    expect_identical(x@theta, theta)
    expect_identical(x@varsigma, new("Scale", varsigma))
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@sigmaMax, new("Scale", 20))
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@lower, -Inf)
    expect_identical(x@upper, Inf)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@tolerance, 1e-5)
    ## intercept only
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = varsigma),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ 1))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    I <- length(y)
    varsigma <- runif(1, max = sd(y))
    sigma <- runif(1, max = sd(y))
    theta <- rnorm(n = 20, mean = 0.5 * mean(y) + 0.5 * y, sd = sigma)
    betas <- list(mean(theta))
    betas <- jitterBetas(betas)
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## lower and upper specified
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 1),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region), lower = 0, upper = 3)
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(all(x@theta > 0))
    expect_true(all(x@theta < 3))
    expect_equal(x@lower, 0)
    expect_equal(x@upper, 3)
    ## has missing values
    y <- Counts(array(rnorm(n = 20, mean = 0),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    y[1:10] <- NA
    weights[1:5] <- NA
    spec <- Model(y ~ Normal(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    varsigma <- runif(n = 1, max = sd(y, na.rm = T))
    sigma <- runif(n = 1, max = sd(y, na.rm = T))
    mean <- 0.5 * mean(y[11:20]) + 0.5 * y
    mean[1:10] <- mean(y[11:20])
    theta <- rnorm(n = 20, mean = mean, sd = sigma)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- makeLinearBetas(theta = theta, formula = mean ~ age + region)
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = sd(y, na.rm = T),
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "NormalVaryingVarsigmaUnknown")
    expect_identical(x@theta, theta)
    expect_identical(x@varsigma, new("Scale", varsigma))
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@lower, -Inf)
    expect_identical(x@upper, Inf)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@tolerance, 1e-5)
    ## saturated model
    y <- Values(array(rnorm(n = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age * region),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    x <- initialModel(spec, y = y, weights = weights)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)
})

test_that("initialModel creates object of class PoissonVaryingUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    makeSigmaInitialPoisson <- demest:::makeSigmaInitialPoisson
    loglm <- MASS:::loglm
    makePriors <- demest:::makePriors
    jitterBetas <- demest:::jitterBetas
    ## no missing values
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 0.05 * mean(exposure) + 0.95 * exposure)
    sigma <- runif(1, max = 1)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- loglm(~ age + region, theta)$param
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "PoissonVaryingUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@ASigma, new("Scale", 1))
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@sigmaMax, new("Scale", qhalft(0.999, df = 7, scale = 1)))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## intercept only
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ 1))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 0.05 * mean(exposure) + 0.95 * exposure)
    sigma <- runif(1, max = 1)
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## specify upper and lower
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region), lower = 0.01, upper = 2)
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(all(x@theta >= 0.01))
    expect_true(all(x@theta <= 2))
    expect_equal(x@lower, log(0.01))
    expect_equal(x@upper, log(2))
    ## has missing values
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1:5] <- NA
    spec <- Model(y ~ Poisson(mean ~ age + region))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    mean.y <- mean(y[6:20])
    mean.expose <- mean(exposure[6:20])
    shape <- 0.05 * mean.y + 0.95 * y
    shape[1:5] <- mean.y
    rate <- 0.05 * mean.expose + 0.95 * exposure
    rate[1:5] <- mean.expose
    theta <- rgamma(n = 20, shape = shape, rate = rate)
    sigma <- runif(1)
    theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
    betas <- loglm(~ age + region, theta)$param
    theta <- as.numeric(theta)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    priors <- makePriors(betas = betas,
                         specs = spec@specsPriors,
                         namesSpecs = spec@namesSpecsPriors,
                         margins = list(0L, 1L, 2L),
                         y = y,
                         sY = NULL,
                         strucZeroArray = strucZeroArray)
    betas <- unname(lapply(betas, as.numeric))
    betas <- jitterBetas(betas, priors)
    expect_is(x, "PoissonVaryingUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(x@betas, betas)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@ASigma, new("Scale", 1))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## saturated model
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age * region),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)
    ## must have exposure
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age * region))
    expect_error(initialModel(spec, y = y, exposure = NULL),
                 "model 'y ~ Poisson\\(mean ~ age \\* region\\)' uses exposure, but no 'exposure' argument supplied")
})

test_that("initialModel creates object of class PoissonVaryingNotUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    makeSigmaInitialPoisson <- demest:::makeSigmaInitialPoisson
    ## no missing values
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  priorSD = HalfT(df = 6, max = 10))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = NULL)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 1)
    sigma <- runif(1, max = sd(log(y+1)))
    expect_is(x, "PoissonVaryingNotUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 6))
    expect_identical(x@ASigma, new("Scale", sd(log(y+1))))
    expect_identical(x@sigmaMax, new("Scale", 10))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## intercept only
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ 1, useExpose = FALSE))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = NULL)
    set.seed(1)
    theta <- rgamma(n = 20,
                    shape = 0.05 * mean(y) + 0.95 * y,
                    rate = 1)
    sigma <- runif(1, max = sd(log(y+1)))
    m <- mean(y)
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_identical(sapply(x@priorsBetas, class), "ExchFixed")
    ## has missing values
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1:5] <- NA
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = NULL)
    set.seed(1)
    mean <- mean(y, na.rm = TRUE)
    shape <- 0.05 * mean + 0.95 * y
    shape[1:5] <- mean
    theta <- rgamma(n = 20, shape = shape, rate = 1)
    sigma <- runif(1, max = sd(log(y+1), na.rm = T))
    expect_is(x, "PoissonVaryingNotUseExp")
    expect_identical(x@sigma, new("Scale", sigma))
    expect_identical(x@theta, theta)
    expect_true(all(sapply(x@priorsBetas, is, "Prior")))
    expect_is(x@priorsBetas[[1]], "ExchFixed")
    expect_identical(x@nuSigma, new("DegreesFreedom", 7))
    expect_identical(x@ASigma, new("Scale", sd(log(y+1), na.rm = TRUE)))
    expect_identical(x@tolerance, 1e-5)
    expect_identical(x@maxAttempt, 1000L)
    expect_identical(x@nFailedPropTheta, new("Counter", 0L))
    ## saturated model
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age * region, useExpose = FALSE),
                  age:region ~ Exch(error = Error(scale = HalfT(df = 3, scale = 0.1, max = 0.6))))
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_identical(x@ASigma, x@priorsBetas[[4]]@ATau)
    expect_identical(x@sigmaMax, x@priorsBetas[[4]]@tauMax)
    expect_identical(x@nuSigma, x@priorsBetas[[4]]@nuTau)
    ## must not have exposure
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age * region, useExpose = FALSE))
    expect_error(initialModel(spec, y = y, exposure = exposure),
                 "exposure' argument supplied, but model 'y ~ Poisson\\(mean ~ age \\* region, useExpose = FALSE\\)' does not use exposure")
})


## PoissonBinomialMixture #############################################################

test_that("initialModel creates object of class PoissonBinomialMixture from valid inputs", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_equal(x,
                 new("PoissonBinomialMixture",
                     call = call("Model", formula = y ~ PoissonBinomial(prob = 0.98)),
                     prob = 0.98,
                     metadataY = y@metadata))
})


## Round3 #############################################################

test_that("initialModel creates object of class Round3 from valid inputs", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y <- round3(y)
    spec <- Model(y ~ Round3())
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_equal(x,
                 new("Round3",
                     call = call("Model", formula = y ~ Round3()),
                     metadataY = y@metadata))
    expect_error(initialModel(spec, y = y + 1L, exposure = exposure),
                 "using 'Round3' data model, but data contains values not divisible by 3")
})


## NormalFixed #############################################################

test_that("initialModel creates object of class NormalFixedUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    mean <- Values(array(runif(n = 25),
                         dim = c(5, 5),
                         dimnames = list(age = 0:4, region = letters[1:5])))
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
    ans.obtained <- initialModel(spec, y = y, exposure = exposure)
    ans.expected <- new("NormalFixedUseExp",
                        call = call("Model", formula = y ~ NormalFixed(mean = mean, sd = 0.1)),
                        mean = new("ParameterVector", mean@.Data[1:20]),
                        sd = new("ScaleVec", rep(0.1, 20)),
                        metadataY = y@metadata,
                        meanAll = new("ParameterVector", mean@.Data),
                        sdAll = new("ScaleVec", rep(0.1, 25)),
                        metadataAll = mean@metadata)
    expect_equal(ans.obtained, ans.expected)
})

test_that("initialModel throws appropriate errors with NormalFixedUseExp", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    mean.wrong <- Values(array(runif(n = 25),
                               dim = c(5, 5),
                               dimnames = list(age = 0:4, region = letters[2:6])))
    spec <- Model(y ~ NormalFixed(mean = mean.wrong, sd = 0.1))
    expect_error(initialModel(spec, y = y, exposure = exposure),
                 "'mean' from NormalFixed model not compatible with data :")
    mean <- Values(array(runif(n = 25),
                         dim = c(5, 5),
                         dimnames = list(age = 0:4, region = letters[1:5])))
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
    expect_error(initialModel(spec, y = y, exposure = NULL),
                 "model 'y ~ NormalFixed\\(mean = mean, sd = 0\\.1\\)' uses exposure, but no 'exposure' argument supplied")
})

test_that("initialModel creates object of class NormalFixedNotUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    mean <- Values(array(runif(n = 25),
                         dim = c(5, 5),
                         dimnames = list(age = 0:4, region = letters[1:5])))
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1, useExpose = FALSE))
    ans.obtained <- initialModel(spec, y = y, exposure = NULL)
    ans.expected <- new("NormalFixedNotUseExp",
                        call = call("Model", formula = y ~ NormalFixed(mean = mean, sd = 0.1, useExpose = FALSE)),
                        mean = new("ParameterVector", mean@.Data[1:20]),
                        sd = new("ScaleVec", rep(0.1, 20)),
                        metadataY = y@metadata,
                        meanAll = new("ParameterVector", mean@.Data),
                        sdAll = new("ScaleVec", rep(0.1, 25)),
                        metadataAll = mean@metadata)
    expect_equal(ans.obtained, ans.expected)
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1, useExpose = FALSE))
    expect_error(initialModel(spec, y = y, exposure = exposure),
                 "'exposure' argument supplied, but model 'y ~ NormalFixed\\(mean = mean, sd = 0.1, useExpose = FALSE\\)' does not use exposure")
})



## TFixed #############################################################

test_that("initialModel creates object of class TFixedUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    location <- Values(array(runif(n = 25),
                             dim = c(5, 5),
                             dimnames = list(age = 0:4, region = letters[1:5])))
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = 0.1, df = 8))
    ans.obtained <- initialModel(spec, y = y, exposure = exposure)
    ans.expected <- new("TFixedUseExp",
                        call = call("Model", formula = y ~ TFixed(location = location, scale = 0.1, df = 8)),
                        mean = new("ParameterVector", location@.Data[1:20]),
                        sd = new("ScaleVec", rep(0.1, 20)),
                        metadataY = y@metadata,
                        meanAll = new("ParameterVector", location@.Data),
                        sdAll = new("ScaleVec", rep(0.1, 25)),
                        nu = new("DegreesFreedom", 8),
                        metadataAll = location@metadata)
    expect_equal(ans.obtained, ans.expected)
})

test_that("initialModel throws appropriate errors with TFixedUseExp", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    location.wrong <- Values(array(runif(n = 25),
                                   dim = c(5, 5),
                                   dimnames = list(age = 0:4, region = letters[2:6])))
    spec <- Model(y ~ TFixed(location = location.wrong, scale = 0.1))
    expect_error(initialModel(spec, y = y, exposure = exposure),
                 "'location' from TFixed model not compatible with data :")
    location <- Values(array(runif(n = 25),
                             dim = c(5, 5),
                             dimnames = list(age = 0:4, region = letters[1:5])))
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = 0.1))
    expect_error(initialModel(spec, y = y, exposure = NULL),
                 "model 'y ~ TFixed\\(location = location, scale = 0\\.1\\)' uses exposure, but no 'exposure' argument supplied")
})

test_that("initialModel creates object of class TFixedNotUseExp from valid inputs", {
    initialModel <- demest:::initialModel
    location <- Values(array(runif(n = 25),
                         dim = c(5, 5),
                         dimnames = list(age = 0:4, region = letters[1:5])))
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = 0.1, useExpose = FALSE))
    ans.obtained <- initialModel(spec, y = y, exposure = NULL)
    ans.expected <- new("TFixedNotUseExp",
                        call = call("Model", formula = y ~ TFixed(location = location, scale = 0.1, useExpose = FALSE)),
                        mean = new("ParameterVector", location@.Data[1:20]),
                        sd = new("ScaleVec", rep(0.1, 20)),
                        metadataY = y@metadata,
                        meanAll = new("ParameterVector", location@.Data),
                        sdAll = new("ScaleVec", rep(0.1, 25)),
                        nu = new("DegreesFreedom", 7),
                        metadataAll = location@metadata)
    expect_equal(ans.obtained, ans.expected)
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = 0.1, useExpose = FALSE))
    expect_error(initialModel(spec, y = y, exposure = exposure),
                 "'exposure' argument supplied, but model 'y ~ TFixed\\(location = location, scale = 0.1, useExpose = FALSE\\)' does not use exposure")
})


## Aggregate #########################################################################

test_that("initialModel works with BinomialVarying and AgCertain", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])),
                       dimscales = c(age = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])),
                dimscales = c(age = "Intervals"))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingAgCertain")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4),
                          dim = 3,
                          dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingAgCertain")
})

test_that("initialModel works with BinomialVarying and AgNormal", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.1)
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingAgNormal")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Binomial(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingAgNormal")
})

test_that("initialModel works with NormalVaryingVarsigmaKnown and AgCertain", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 0.3),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.2),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaKnownAgCertain")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4),
                          dim = 3,
                          dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.2),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaKnownAgCertain")
})

test_that("initialModel works with NormalVaryingVarsigmaKnown and AgNormal", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 0.3),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.1)
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.2),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaKnownAgNormal")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Normal(mean ~ age + region, sd = 0.2),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaKnownAgNormal")
})

test_that("initialModel works with NormalVaryingVarsigmaUnknown and AgCertain", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 0.3),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Normal(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownAgCertain")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4),
                          dim = 3,
                          dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Normal(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownAgCertain")
})

test_that("initialModel works with NormalVaryingVarsigmaUnknown and AgNormal", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 0.3),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.1)
    spec <- Model(y ~ Normal(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownAgNormal")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Normal(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, weights = weights)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownAgNormal")
})

test_that("initialModel works with PoissonVaryingNotUseExp and AgCertain", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  upper = 3,
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgCertain")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4),
                          dim = 3,
                          dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgCertain")
})

test_that("initialModel works with PoissonVaryingNotUseExp and AgNormal", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.1)
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgNormal")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgNormal")
})

test_that("initialModel works with PoissonVaryingUseExp and AgCertain", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgCertain")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4),
                          dim = 3,
                          dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgCertain")
})

test_that("initialModel works with PoissonVaryingUseExp and AgNormal", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.1)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgNormal")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgNormal")
})

test_that("initialModel works with PoissonVaryingNotUseExp and AgPoisson", {
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgPoisson(value = 0.4)
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgPoisson")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgPoisson(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpAgPoisson")
})

test_that("initialModel works with PoissonVaryingUseExp and AgPoisson", {
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgPoisson(value = 0.4)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgPoisson")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgPoisson(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgPoisson")
})

test_that("initialModel works with PoissonVaryingUseExp and AgLife", {
    initialModel <- demest:::initialModel
    expose <- Counts(array(rpois(n = 20, lambda = 20),
                           dim = c(5, 4),
                           dimnames = list(age = c(0:3, "4+"), region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * expose),
                      dim = c(5, 4),
                      dimnames = list(age = c(0:3, "4+"), region = letters[1:4])))
    ## scalar
    aggregate <- AgLife(value = 20, sd = 0.1)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = expose)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgLife")
    ## Values
    value <- Values(array(c(0.2, 0.3, 0.4), dim = 3, dimnames = list(region = c("a", "b", "c"))))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = expose)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingUseExpAgNormal")
})


## initialModelPredict ###############################################################

test_that("initialModelPredict works with BinomialVarying - without aggregate", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                             dim = c(5, 4),
                             dimnames = list(time = 2001:2005, region = 1:4)),
                       dimscales = c(time = "Intervals"))
    weights.bench <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                                  dim = c(4, 4),
                                  dimnames = list(time = 2006:2009, region = 1:4)),
                            dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.7),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod.est <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingPredict")
})

test_that("initialModelPredict works with BinomialVaryingPredict - with aggregate", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                                 dim = c(5, 4),
                             dimnames = list(time = 2001:2005, region = 1:4)),
                       dimscales = c(time = "Intervals"))
    weights.bench <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                                  dim = c(4, 4),
                                  dimnames = list(time = 2006:2009, region = 1:4)),
                            dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.7),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    ## AgCertain with weights
    aggregate <- AgCertain(value = 0.5, weights = weights.bench)
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod.est <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingPredictAgCertain")
    ## AgNormal with weights
    aggregate <- AgNormal(value = 0.5, sd = 1, weights = weights.bench)
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod.est <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "BinomialVaryingPredictAgNormal")
})
    
test_that("initialModelPredict works with NormalVaryingVarsigmaUnknown - without aggregate", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 20, mean = 0, sd = 0.3),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(rbeta(n = 20, shape1 = 1, shape2 = 1),
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ age + region))
    x <- initialModel(spec, y = y, weights = weights)
    set.seed(1)
    x <- initialModelPredict(x,
                             along = 2L,
                             labels = c("a", "b", "c", "d"),
                             n = NULL,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = 1,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownPredict")
})

test_that("initialModelPredict works with NormalVaryingVarsigmaUnknownPredict - with aggregate", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(20),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(1,
                            dim = c(5, 4),
                            dimnames = list(time = 2001:2005, region = 1:4)),
                      dimscales = c(time = "Intervals"))
    weights.ag <- Counts(array(1,
                               dim = c(4, 4),
                               dimnames = list(time = 2006:2009, region = 1:4)),
                         dimscales = c(time = "Intervals"))
    ## AgCertain
    aggregate <- AgCertain(value = 0.5, weights = weights.ag)
    spec <- Model(y ~ Normal(mean ~ time + region))
    mod.est <- initialModel(spec, y = y, weights = weights)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownPredictAgCertain")
    ## AgNormal
    aggregate <- AgNormal(value = 0.5, sd = 1, weights = weights.ag)
    spec <- Model(y ~ Normal(mean ~ time + region))
    mod.est <- initialModel(spec, y = y, weights = weights)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "NormalVaryingVarsigmaUnknownPredictAgNormal")
})
          
test_that("initialModelPredict works with PoissonVaryingUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(x,
                             along = 2L,
                             labels = c("a", "b", "c", "d"),
                             n = NULL,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = 100000)
    expect_is(x, "PoissonVaryingUseExpPredict")
})

test_that("initialModelPredict works with PoissonVaryingNotUseExpPredict - with aggregate", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(20, lambda = 100),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    weights.bench <- Counts(array(1,
                                  dim = c(4, 4),
                                  dimnames = list(time = 2006:2009, region = 1:4)),
                            dimscales = c(time = "Intervals"))
    ## AgCertain with no weights
    aggregate <- AgCertain(value = 0.5, weights = weights.bench)
    spec <- Model(y ~ Poisson(mean ~ time + region, useExpose = FALSE))
    mod.est <- initialModel(spec, y = y, exposure = NULL)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpPredictAgCertain")
    expect_true(all(x@weightAg == 1))
    ## AgNormal
    aggregate <- AgNormal(value = 0.5, sd = 1, weights = weights.bench)
    spec <- Model(y ~ Poisson(mean ~ time + region, useExpose = FALSE))
    mod.est <- initialModel(spec, y = y, exposure = NULL)
    x <- initialModelPredict(mod.est,
                             along = 1L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = aggregate,
                             lower = NULL,
                             upper = NULL)
    expect_true(validObject(x))
    expect_is(x, "PoissonVaryingNotUseExpPredictAgNormal")
})


test_that("initialModelPredict works with CMPVaryingUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(x,
                             along = 2L,
                             labels = c("a", "b", "c", "d"),
                             n = NULL,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = 100000)
    expect_is(x, "CMPVaryingUseExpPredict")
})

test_that("initialModelPredict works with CMPVaryingUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.5 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ CMP(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(x,
                             along = 2L,
                             labels = c("a", "b", "c", "d"),
                             n = NULL,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = 100000)
    expect_is(x, "CMPVaryingUseExpPredict")
})


test_that("initialModelPredict works with PoissonBinomial", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ PoissonBinomial(prob = 0.95))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})


test_that("initialModelPredict works with Round3", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y <- round3(y)
    spec <- Model(y ~ Round3())
    model <- initialModel(spec, y = y, exposure = exposure)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})



test_that("initialModelPredict works with NormFixedUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    mean <- Values(array(rnorm(n = 40),
                         dim = c(10, 4),
                         dimnames = list(age = 0:9, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sqrt(abs(mean))))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})

test_that("initialModelPredict works with NormFixedNotUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    mean <- Values(array(rnorm(n = 40),
                         dim = c(10, 4),
                         dimnames = list(age = 0:9, region = letters[1:4])))
    spec <- Model(y ~ NormalFixed(mean = mean, sd = sqrt(abs(mean)), useExpose = FALSE))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})


test_that("initialModelPredict works with NormFixedUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    location <- Values(array(rnorm(n = 40),
                         dim = c(10, 4),
                         dimnames = list(age = 0:9, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = sqrt(abs(location))))
    model <- initialModel(spec, y = y, exposure = exposure)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})

test_that("initialModelPredict works with NormFixedNotUseExp", {
    initialModelPredict <- demest:::initialModelPredict
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    location <- Values(array(rnorm(n = 40),
                         dim = c(10, 4),
                         dimnames = list(age = 0:9, region = letters[1:4])))
    spec <- Model(y ~ TFixed(location = location, scale = sqrt(abs(location)), useExpose = FALSE))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans <- initialModelPredict(model,
                               along = 1L,
                               labels = NULL,
                               n = 5L,
                               offsetModel = 1L)
    expect_true(validObject(ans))
    metadata.expected <- Counts(array(1L,
                                      dim = c(5, 4),
                                      dimnames = list(age = 5:9, region = letters[1:4])))@metadata
    expect_identical(ans@metadataY, metadata.expected)
    expect_identical(ans@iMethodModel, model@iMethodModel + 100L)
})







