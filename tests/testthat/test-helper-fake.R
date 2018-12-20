
context("helper-fake")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("checkPriorInform_prohibited works", {
    checkPriorInform_prohibited <- demest:::checkPriorInform_prohibited
    object <- ExchFixed()
    expect_identical(checkPriorInform_prohibited(object = object,
                                                 nameSlot = "multTau",
                                                 nameArg = "mult",
                                                 nameFun = "ExchFixed"),
                     NULL)
    object <- ExchFixed(mult = 3)
    expect_identical(checkPriorInform_prohibited(object = object,
                                                 nameSlot = "multTau",
                                                 nameArg = "mult",
                                                 nameFun = "ExchFixed"),
                     "value for 'mult' supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_required works", {
    checkPriorInform_required <- demest:::checkPriorInform_required
    object <- ExchFixed(sd = 3)
    expect_identical(checkPriorInform_required(object = object,
                                               nameSlot = "tau",
                                               nameArg = "sd",
                                               nameFun = "ExchFixed"),
                     NULL)
    object <- ExchFixed()
    expect_identical(checkPriorInform_required(object = object,
                                               nameSlot = "tau",
                                               nameArg = "sd",
                                               nameFun = "ExchFixed"),
                     "value for 'sd' not supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_ExchFixed works", {
    checkPriorInform_ExchFixed <- demest:::checkPriorInform_ExchFixed
    object <- ExchFixed(sd = 3)
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     NULL)
    object <- ExchFixed(mult = 3)
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     "value for 'mult' supplied in call to 'ExchFixed'")                 
    object <- ExchFixed()
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     "value for 'sd' not supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_Error works", {
    checkPriorInform_Error <- demest:::checkPriorInform_Error
    object <- Exch(error = Error(scale = HalfT(scale = 0.2)))
    expect_identical(checkPriorInform_Error(object = object),
                     NULL)
    object <- Exch(error = Error(scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Error(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error'")                 
    object <- Exch()
    expect_identical(checkPriorInform_Error(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error'")
})

test_that("checkPriorInform_Covariates works", {
    checkPriorInform_Covariates <- demest:::checkPriorInform_Covariates
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10))
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 10),
                                           coef = TDist(scale = c(0.4, 0.3))))
    expect_identical(checkPriorInform_Covariates(object = object),
                     NULL)
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(),
                                           coef = TDist(scale = 0.4)))
    expect_identical(checkPriorInform_Covariates(object = object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist()))
    expect_identical(checkPriorInform_Covariates(object = object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates'")
})

test_that("checkPriorInform_Level works", {
    checkPriorInform_Level <- demest:::checkPriorInform_Level
    object <- DLM(level = Level(scale = HalfT(scale = 0.02)))
    expect_identical(checkPriorInform_Level(object = object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Level(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level'")
    object <- DLM()
    expect_identical(checkPriorInform_Level(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level'")
})


test_that("checkPriorInform_Trend works", {
    checkPriorInform_Trend <- demest:::checkPriorInform_Trend
    object <- DLM(trend = Trend(initial = Initial(sd = 0.01),
                                scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     NULL)
    object <- DLM(trend = Trend(initial = Initial(mult = 3),
                                scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'mult' supplied in call to 'Initial' when specifying 'trend'")
    object <- DLM(trend = Trend(scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend'")
    object <- DLM(trend = Trend(initial = Initial(sd = 3),
                                scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'trend'")
    object <- DLM(trend = Trend(initial = Initial(sd = 3)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'trend'")
})

test_that("checkPriorInform_Season works", {
    checkPriorInform_Season <- demest:::checkPriorInform_Season
    object <- DLM(season = Season(n = 4, scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Season(object = object),
                     NULL)
    object <- DLM(season = Season(n = 4, scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Season(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season'")
    object <- DLM(season = Season(n = 3))
    expect_identical(checkPriorInform_Season(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season'")
})











test_that("checkPriorsAreInformative works", {
    checkPriorsAreInformative <- demest:::checkPriorsAreInformative
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(error = Error(scale = HalfT(scale = 0.3))))
    expect_is(model, "SpecVarying")
    expect_identical(checkPriorsAreInformative(model),
                     NULL)
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch())
    expect_error(checkPriorsAreInformative(model),
                 "problem with prior for 'region' in model for 'y'")
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1),
                   region ~ Exch())
    expect_error(checkPriorsAreInformative(model),
                 "problem with prior for '\\(Intercept\\)' in model for 'y'")
})
               





## test_that("makeFakeBetas works", {
##     makeFakeBetas <- demest:::makeFakeBetas
##     sweepAllMargins <- demest:::sweepAllMargins
##     ## complicated model
##     y <- Counts(array(rpois(n = 24, lambda = 10),
##                       dim = 2:4,
##                       dimnames = list(sex = c("f", "m"),
##                           region = c("a", "b", "c"), age = 0:3)))
##     formula <- mean ~ sex * age
##     specPriors <- list(Exch(sd = 1), Exch(sd = 0.5), Exch(sd = 0.2))
##     namesSpecPriors <- c("sex", "age", "sex:age")
##     intercept <- 3
##     set.seed(1)
##     ans.obtained <- makeFakeBetas(y = y, formula = formula, specPriors = specPriors,
##                                   namesSpecPriors = namesSpecPriors,
##                                   intercept = intercept)
##     set.seed(1)
##     ans.expected <- vector(mode = "list", length = 4)
##     ans.expected[[1]] <- intercept
##     ans.expected[[2]] <- rnorm(n = 2, sd = 1)
##     ans.expected[[2]] <- ans.expected[[2]] - mean(ans.expected[[2]])
##     ans.expected[[3]] <- rnorm(n = 4, sd = 0.5)
##     ans.expected[[3]] <- ans.expected[[3]] - mean(ans.expected[[3]])
##     ans.expected[[4]] <- rnorm(n = 8, sd = 0.2)
##     ans.expected[[4]] <- as.double(sweepAllMargins(array(ans.expected[[4]], c(2,4))))
##     expect_identical(ans.obtained, ans.expected)
##     ## intercept-only model
##     y <- Counts(array(rpois(n = 24, lambda = 10),
##                       dim = 2:4,
##                       dimnames = list(sex = c("f", "m"), region = c("a", "b", "c"), age = 0:3)))
##     formula <- mean ~ 1
##     specPriors <- list()
##     namesSpecPriors <- character()
##     intercept <- 2
##     set.seed(1)
##     ans.obtained <- makeFakeBetas(y = y, formula = formula, specPriors = specPriors,
##                                   namesSpecPriors = namesSpecPriors, intercept = intercept)
##     set.seed(1)
##     ans.expected <- list(intercept)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakeBetasOutput works", {
##     makeFakeBetasOutput <- demest:::makeFakeBetasOutput
##     betas <- list(3, rnorm(3), rnorm(4), rnorm(12))
##     y <- Counts(array(1, dim = 3:5, dimnames = list(region = 1:3, age = 0:3, class = 1:5)))
##     namesBetas <- c("(Intercept)", "region", "age", "region:age")
##     ans.obtained <- makeFakeBetasOutput(betas = betas, y = y, namesBetas = namesBetas)
##     ans.expected <- list("(Intercept)" = 3,
##                          region = Values(array(betas[[2]], dim = 3, dimnames = list(region = 1:3))),
##                          age = Values(array(betas[[3]], dim = 4, dimnames = list(age = 0:3))),
##                          "region:age" = Values(array(betas[[4]], dim = 3:4,
##                          dimnames = list(region = 1:3, age = 0:3))))
##     expect_identical(ans.obtained, ans.expected)
##     ## intercept only
##     betas <- list(3)
##     y <- Counts(array(1, dim = 3:5, dimnames = list(region = 1:3, age = 0:3, class = 1:5)))
##     namesBetas <- "(Intercept)"
##     ans.obtained <- makeFakeBetasOutput(betas = betas, y = y, namesBetas = namesBetas)
##     ans.expected <- list("(Intercept)" = 3)
##     expect_identical(ans.obtained, ans.expected)
## })


## test_that("makeFakeSigma works", {
##     makeFakeSigma <- demest:::makeFakeSigma
##     rinvchisq1 <- demest:::rinvchisq1
##     set.seed(1)
##     ans.obtained <- makeFakeSigma(dfPriorSigma = 5, scalePriorSigma = 6.3)
##     set.seed(1)
##     ans.expected <- rinvchisq1(df = 5, scale = 6.3)
##     expect_identical(ans.obtained, ans.expected)
##     expect_error(makeFakeSigma(dfPriorSigma = -1, scalePriorSigma = 0),
##                  "'prior.sd' must have an informative prior with function 'fakeData'")
## })


## ## FAKE ######################################################################

## test_that("initialFakeDLMAll works", {
##     initialFakeDLMAll <- demest:::initialFakeDLMAll
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.05)),
##                 trend = Trend(initial = Initial(mean = -0.02, sd = 0.01),
##                               scale = HalfT(scale = 0.02)),
##                 error = Error(scale = HalfT(scale = 0.06)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 2001:2010)))
##     ans.obtained <- initialFakeDLMAll(spec = spec,
##                                       metadata = metadata)
##     expect_identical(ans.obtained$AAlpha, new("Scale", 0.05))
##     expect_identical(ans.obtained$alphaDLM, new("ParameterVector", rep(0, 11)))
##     expect_identical(ans.obtained$ATau, new("Scale", 0.06))
##     expect_identical(ans.obtained$iAlong, 1L)
##     expect_is(ans.obtained$iteratorState, "AlongIterator")
##     expect_is(ans.obtained$iteratorV, "AlongIterator")
##     expect_identical(ans.obtained$J, new("Length", 10L))
##     expect_identical(ans.obtained$K, new("Length", 10L))
##     expect_identical(ans.obtained$L, new("Length", 1L))
##     expect_identical(ans.obtained$minPhi, 0.8)
##     expect_identical(ans.obtained$maxPhi, 1)
##     expect_identical(ans.obtained$nuAlpha, new("DegreesFreedom", 7))
##     expect_identical(ans.obtained$nuTau, new("DegreesFreedom", 7))
##     expect_is(ans.obtained$omegaAlpha, "Scale")
##     expect_is(ans.obtained$omegaAlphaMax, "Scale")
##     expect_is(ans.obtained$phi, "numeric")
##     expect_identical(ans.obtained$shape1Phi, new("Scale", 2))
##     expect_identical(ans.obtained$shape2Phi, new("Scale", 2))
##     expect_is(ans.obtained$tau, "Scale")
##     expect_is(ans.obtained$tauMax, "Scale")
## })

## test_that("initialFakeDLMWithTrend works", {
##     initialFakeDLMWithTrend <- demest:::initialFakeDLMWithTrend
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.05)),
##                 trend = Trend(initial = Initial(mean = -0.02, sd = 0.01),
##                               scale = HalfT(scale = 0.02)),
##                 error = Error(scale = HalfT(scale = 0.06)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 2001:2010)))
##     ans.obtained <- initialFakeDLMWithTrend(spec = spec, metadata = metadata)
##     expect_identical(ans.obtained$ADelta, new("Scale", 0.02))
##     expect_identical(ans.obtained$deltaDLM, new("ParameterVector", rep(0, 11)))
##     expect_identical(ans.obtained$nuDelta, new("DegreesFreedom", 7))
##     expect_is(ans.obtained$omegaDelta, "Scale")
##     expect_is(ans.obtained$omegaDeltaMax, "Scale")
##     expect_identical(ans.obtained$ADelta0, new("Scale", 0.01))
##     expect_identical(ans.obtained$meanDelta0, new("Parameter", -0.02))
##     expect_identical(ans.obtained$hasLevel, new("LogicalFlag", TRUE))
## })

## test_that("makeFakeHyper works", {
##     makeFakeHyper <- demest:::makeFakeHyper
##     makeFakeOutputPrior <- demest:::makeFakeOutputPrior
##     fakePrior <- demest:::fakePrior
##     spec.int <- ExchFixed(mean = -1, sd = 0.1)
##     spec.time <- DLM(level = Level(scale = HalfT(scale = 0.01)),
##                 trend = NULL,
##                 error = Error(scale = HalfT(scale = 0.01)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 1:10)))
##     prior.int <- fakePrior(spec.int,
##                            metadata = NULL,
##                            isSaturated = FALSE)
##     prior.time <- fakePrior(spec.time,
##                             metadata = metadata,
##                             isSaturated = TRUE)
##     priors <- list(prior.int, prior.time)
##     names <- c("(Intercept)", "age")
##     ans.obtained <- makeFakeHyper(priors = priors,
##                                   margins = 0:1,
##                                   metadata = metadata,
##                                   names = names)
##     ans.expected <- list("(Intercept)" = list(mean = -1,
##                                               sd = 0.1),
##                          age = list(level = ValuesOne(prior.time@alphaDLM[-1],
##                                                       labels = 1:10,
##                                                       name = "time",
##                                                       dimscale = "Points"),
##                                     scaleLevel = prior.time@omegaAlpha@.Data,
##                                     damp = prior.time@phi,
##                                     scaleError = prior.time@tau@.Data))
##     expect_equal(ans.obtained, ans.expected)
## })

## test_that("makeFakeMargins works", {
##     makeFakeMargins <- demest:::makeFakeMargins
##     namesSpecs <- c("(Intercept)", "age", "sex", "age:sex")
##     y <- Counts(array(1,
##                       dim = c(3, 2),
##                       dimnames = list(age = c("0-4", "5-9", "10+"),
##                                       sex = c("f", "m"))))
##     call <- call("Model", y ~ Poisson(mean ~ age * sex),
##                  `(Intercept)` ~ ExchFixed(mean = -3, sd = 0.1),
##                  age ~ DLM(level = Level(scale = HalfT(scale = 0.01)),
##                            error = Error(scale = HalfT(scale = 0.1))),
##                  sex ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
##                  age:sex ~ Exch(error = Error(scale = HalfT(scale = 0.001))))
##     ans.obtained <- makeFakeMargins(namesSpecs = namesSpecs,
##                                     y = y,
##                                     call = call)
##     expect_identical(ans.obtained, list(0L, 1L, 2L, 1:2))
##     namesSpecs.wrong <- namesSpecs[-1]
##     expect_error(makeFakeMargins(namesSpecs = namesSpecs.wrong,
##                                  y = y,
##                                  call = call),
##                  "no prior specified for '\\(Intercept\\)' in model")
##     namesSpecs.wrong <- c(namesSpecs, "wrong")
##     expect_error(makeFakeMargins(namesSpecs = namesSpecs.wrong,
##                                  y = y,
##                                  call = call),
##                  "term 'wrong' from formula")
## })

## test_that("makeFakeOutputLevelDLM works", {
##     makeFakeOutputLevelDLM <- demest:::makeFakeOutputLevelDLM
##     fakePrior <- demest:::fakePrior
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.01)),
##                 trend = NULL,
##                 error = Error(scale = HalfT(scale = 0.01)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 1:10)))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     ans.obtained <- makeFakeOutputLevelDLM(prior = prior,
##                                            metadata = metadata)
##     ans.expected <- array(prior@alphaDLM[-1L],
##                           dim = dim(metadata),
##                           dimnames = dimnames(metadata))
##     ans.expected <- new("Values",
##                         .Data = ans.expected,
##                         metadata = metadata)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakeOutputTrendDLM works", {
##     makeFakeOutputTrendDLM <- demest:::makeFakeOutputTrendDLM
##     fakePrior <- demest:::fakePrior
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.01)),
##                 trend = Trend(initial = Initial(sd = 0.01),
##                               scale = HalfT(scale = 0.01)),
##                 error = Error(scale = HalfT(scale = 0.01)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 1:10)))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     ans.obtained <- makeFakeOutputTrendDLM(prior = prior,
##                                            metadata = metadata)
##     ans.expected <- array(prior@deltaDLM[-1L],
##                           dim = dim(metadata),
##                           dimnames = dimnames(metadata))
##     ans.expected <- new("Values",
##                         .Data = ans.expected,
##                         metadata = metadata)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakePhi works", {
##     makeFakePhi <- demest:::makeFakePhi
##     ## phi known
##     ans.obtained <- makeFakePhi(phi = 0.9,
##                                 phiKnown = TRUE,
##                                 min = 0,
##                                 max = 1,
##                                 shape1 = 1,
##                                 shape2 = 1)
##     ans.expected <- 0.9
##     expect_identical(ans.obtained, ans.expected)
##     ## phi unknown
##     set.seed(1)
##     ans.obtained <- makeFakePhi(phi = as.double(NA),
##                                 phiKnown = FALSE,
##                                 min = 0.8,
##                                 max = 0.98,
##                                 shape1 = 3,
##                                 shape2 = 3)
##     set.seed(1)
##     ans.expected <- rbeta(1, 3, 3)
##     ans.expected <- ans.expected * 0.18 + 0.8
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
## })

## test_that("makeFakePriors works", {
##     makeFakePriors <- demest:::makeFakePriors
##     specs <- list(ExchFixed(mean = -3, sd = 0.1),
##                   Exch(error = Error(scale = HalfT(scale = 0.05))),
##                   DLM(level = Level(scale = HalfT(scale = 0.01)),
##                       trend = Trend(initial = Initial(sd = 0.01, mean = 0.02),
##                                     scale = HalfT(scale = 0.01)),
##                       error = Error(scale = HalfT(scale = 0.05))))
##     margins <- c(0L, 1L, 2L)
##     metadata <- new("MetaData",
##                     nms = c("reg", "age"),
##                     dimtypes = c("state", "age"),
##                     DimScales = list(new("Categories", dimvalues = letters[1:10]),
##                                      new("Intervals", dimvalues = seq(0, 50, 5))))
##     isSaturated <- c(FALSE, FALSE, FALSE)
##     ans.obtained <- makeFakePriors(specs = specs,
##                                    margins = margins,
##                                    metadata = metadata,
##                                    isSaturated = isSaturated)
##     expect_identical(sapply(ans.obtained, class),
##                      c("FakeExchFixed", "FakeExchNormZero", "FakeDLMWithTrendNormZeroNoSeason"))
## })

## test_that("makeFakeScale works", {
##     makeFakeScale <- demest:::makeFakeScale
##     rhalftTrunc1 <- demest:::rhalftTrunc1
##     ## valid inputs
##     A <- new("SpecScale", 0.1)
##     nu <- new("DegreesFreedom", 2)
##     scaleMax <- new("SpecScale", 0.3)
##     functionName <- "Error"
##     set.seed(1)
##     ans.obtained <- makeFakeScale(A = A,
##                              nu = nu,
##                              scaleMax = scaleMax,
##                              functionName = functionName)
##     set.seed(1)
##     scale <- new("Scale", rhalftTrunc1(df = 2, scale = 0.1, max = 0.3))
##     scaleMax <- new("Scale", 0.3)
##     A <- new("Scale", 0.1)
##     ans.expected <- list(scale = scale, A = A, scaleMax = scaleMax)
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
##     ## no scale specified
##     expect_error(makeFakeScale(A = new("SpecScale", as.double(NA)),
##                                           nu = nu,
##                                           scaleMax = scaleMax,
##                                           functionName = functionName),
##                  "need to specify scale of half-t distribution for 'scale' in call to function 'Error'")
## })
