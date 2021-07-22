
context("AllClasses-SpecModel")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("can create valid object of class SpecBinomialVarying", {
    ## series supplied; have priors
    x <- new("SpecBinomialVarying",
             call = call("Model", reg ~ Binomial(mean ~ age + sex)),
             nameY = new("Name", "reg"),
             ASigma = new("SpecScale", as.numeric(NA)),
             formulaMu = mean ~ age + sex,
             lower = 0,
             upper = 1,
             specsPriors = list(DLM(), ExchFixed()),
             namesSpecsPriors = c("age", "sex"),
             nuSigma = new("DegreesFreedom", 7),
             series = new("SpecName", "deaths"),
             scaleTheta = new("Scale", 0.2),
             aggregate = new("SpecAgPlaceholder"))
          expect_true(validObject(x))
    ## series NULL; no priors
    x <- new("SpecBinomialVarying",
             call = call("Model", y ~ Binomial(mean ~ age + sex)),
             nameY = new("Name", "y"),
             ASigma = new("SpecScale", 3),
             formulaMu = mean ~ age + sex,
             lower = 0.1,
             upper = 1,
             specsPriors = list(),
             namesSpecsPriors = character(),
             nuSigma = new("DegreesFreedom", 7),
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.1),
             series = new("SpecName", as.character(NA)),
             aggregate = new("SpecAgPlaceholder"))
    expect_true(validObject(x))
})

test_that("validity tests for SpecBinomialVarying inherited from FormulaMu work", {
    x <- new("SpecBinomialVarying",
             call = call("Model", y ~ Binomial(mean ~ age + sex)),
             nameY = new("Name", "y"),
             ASigma = new("SpecScale", as.numeric(NA)),
             formulaMu = mean ~ age + sex,
             lower = 0,
             upper = 1,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             nuSigma = new("DegreesFreedom", 7),
             series = new("SpecName", as.character(NA)),
             scaleTheta = new("Scale", 0.2),
             aggregate = new("SpecAgPlaceholder"))
    ## 'formulaMu' has a response
    x.wrong <- x
    x.wrong@formulaMu <- ~ age + sex
    expect_error(validObject(x.wrong),
                 "formula '~age \\+ sex' does not have a response")
})

test_that("validity tests for SpecBinomialVarying inherited from SpecsPriorsMixin work", {
    x <- new("SpecBinomialVarying",
             call = call("Model", y ~ Binomial(mean ~ age + sex)),
             nameY = new("Name", "y"),
             ASigma = new("SpecScale", as.numeric(NA)),
             formulaMu = mean ~ age + sex,
             lower = 0,
             upper = 1,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             nuSigma = new("DegreesFreedom", 7),
             series = new("SpecName", as.character(NA)),
             scaleTheta = new("Scale", 0.2),
             aggregate = new("SpecAgPlaceholder"))
    ## 'specsPriors' all have class "SpecPrior"
    x.wrong <- x
    x.wrong@specsPriors[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'specsPriors' has elements not of class \"SpecPrior\"")
    ## 'namesSpecsPriors' has no missing values
    x.wrong <- x
    x.wrong@namesSpecsPriors[1] <- NA
    expect_error(validObject(x.wrong),
                 "'namesSpecsPriors' has missing values")
    ## 'namesSpecsPriors' has no blanks
    x.wrong <- x
    x.wrong@namesSpecsPriors[1] <- ""
    expect_error(validObject(x.wrong),
                 "'namesSpecsPriors' has blanks")
    ## 'namesSpecsPriors' has no duplicates
    x.wrong <- x
    x.wrong@namesSpecsPriors[2] <- "age"
    expect_error(validObject(x.wrong),
                 "'namesSpecsPriors' has duplicates")
    ## 'specsPriors' and 'namesSpecsPriors' have same length
    x.wrong <- x
    x.wrong@namesSpecsPriors <- x.wrong@namesSpecsPriors[-1]
    expect_error(validObject(x.wrong),
                 "'specsPriors' and 'namesSpecsPriors' have different lengths")
    ## 'namesSpecsPriors' refers to terms from 'formulaMu'
    x.wrong <- x
    x.wrong@namesSpecsPriors[1] <- "wrong"
    expect_error(validObject(x.wrong),
                 sprintf("%s from 'namesSpecsPriors' is not a term from formula 'mean ~ age \\+ sex'",
                         dQuote("wrong")))
    x.wrong <- x
    x.wrong@namesSpecsPriors[1:2] <- c("wrong1", "wrong2")
    expect_error(validObject(x.wrong),
                 sprintf("%s, %s from 'namesSpecsPriors' are not terms from formula 'mean ~ age \\+ sex'",
                         dQuote("wrong1"), dQuote("wrong2")))
})

test_that("validity tests for SpecBinomialVarying inherited from SpecBinomialVarying work", {
    x <- new("SpecBinomialVarying",
             call = call("Model", y ~ Binomial(mean ~ age + sex)),
             nameY = new("Name", "deaths"),
             formulaMu = mean ~ age * sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = 0,
             upper = 1,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             series = new("SpecName", as.character(NA)),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    ## response from 'formulaMu' is "prob"
    x.wrong <- x
    x.wrong@formulaMu <- wrong ~ age + sex
    expect_error(validObject(x.wrong),
                 "response for formula 'wrong ~ age \\+ sex' is not 'mean'")
    ## 'lower' non-negative
    x.wrong <- x
    x.wrong@lower <- -1
    expect_error(validObject(x.wrong),
                 "'lower' is less than 0")
    ## 'upper' less than or equal to 1
    x.wrong <- x
    x.wrong@upper <- 1.1
    expect_error(validObject(x.wrong),
                 "'upper' is greater than 1")
})

test_that("can create valid object of class SpecNormalVaryingVarsigmaKnown", {
    ## nameY and varsigma supplied; have priors
    x <- new("SpecNormalVaryingVarsigmaKnown",
             call = call("Model", y ~ Normal(mean ~ age + sex)),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             ASigma = new("SpecScale", as.numeric(NA)),
             varsigma = new("Scale", 0.3),
             aggregate = new("SpecAgPlaceholder"))
      expect_true(validObject(x))
    ## no priors
    x <- new("SpecNormalVaryingVarsigmaKnown",
             call = call("Model", y ~ Normal(mean ~ age + sex)),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(),
             namesSpecsPriors = character(),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             varsigma = new("Scale", 1),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
          expect_true(validObject(x))
})

test_that("validity tests for SpecNormalVaryingVarsigmaKnown inherited from SpecNormalVarying work", {
    x <- new("SpecNormalVaryingVarsigmaKnown",
             call = call("Model", y ~ Normal(mean ~ age + sex, sd = 2)),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             varsigma = new("Scale", 2),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    ## response from 'formulaMu' is "mean"
    x.wrong <- x
    x.wrong@formulaMu <- wrong ~ age + sex
    expect_error(validObject(x.wrong),
                 "response for formula 'wrong ~ age \\+ sex' is not 'mean'")
})


test_that("validity tests for SpecNormalVaryingVarsigmaKnown inherited from VarsigmaSetToZeroMixin work", {
    x <- new("SpecNormalVaryingVarsigmaKnown",
             call = call("Model", y ~ Normal(mean ~ age + sex, sd = 0)),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             varsigma = new("Scale", 0),
             varsigmaSetToZero = new("LogicalFlag", TRUE),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    ## if varsigma is 0, lower, upper not specified
    x.wrong <- x
    x.wrong@lower <- 0
    expect_error(validObject(x.wrong),
                 "'varsigma' is 0 but 'lower' is finite")
})


test_that("validity tests for SpecNormalVaryingVarsigmaKnown inherited from SpecNormalVaryingVarsigmaKnown work", {
    x <- new("SpecNormalVaryingVarsigmaKnown",
             call = call("Model", y ~ Normal(mean ~ age + sex, sd = 0)),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             varsigma = new("Scale", 0),
             varsigmaSetToZero = new("LogicalFlag", TRUE),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    ## if varsigma is 0, 'aggregate' not specified
    x.wrong <- x
    x.wrong@aggregate <- AgCertain(1)
    expect_error(validObject(x.wrong),
                 "'varsigma' is 0 but 'aggregate' has class \"SpecAgCertain\"")
})

test_that("can create valid object of class SpecNormalVaryingVarsigmaUnknown", {
    ## nameY and varsigma supplied; have priors
    x <- new("SpecNormalVaryingVarsigmaUnknown",
             call = call("Model", y ~ Normal(mean ~ age + sex, priorSD = HalfT(4, 0.3))),
             nameY = new("Name", "y"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = -Inf,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             ASigma = new("SpecScale", as.numeric(NA)),
             AVarsigma = new("SpecScale", 0.3),
             nuVarsigma = new("DegreesFreedom", 4),
             aggregate = new("SpecAgPlaceholder"))
      expect_true(validObject(x))
})

test_that("can create valid object of class SpecPoissonVarying", {
    ## nameY and series; have priors
    x <- new("SpecPoissonVarying",
             call = call("Model", reg.deaths ~ Poisson(mean ~ age + sex)),
             nameY = new("Name", "reg.deaths"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = 0,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             series = new("SpecName", "deaths"),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    expect_true(validObject(x))
})

test_that("validity tests for SpecPoissonVarying inherited from SpecPoissonVarying work", {
    ## nameY and series; have priors
    x <- new("SpecPoissonVarying",
             call = call("Model", reg.deaths ~ Poisson(mean ~ age + sex)),
             nameY = new("Name", "reg.deaths"),
             formulaMu = mean ~ age + sex,
             specsPriors = list(DLM(), Exch()),
             namesSpecsPriors = c("age", "sex"),
             lower = 0,
             upper = Inf,
             maxAttempt = 100L,
             scaleTheta = new("Scale", 0.2),
             series = new("SpecName", "deaths"),
             ASigma = new("SpecScale", as.numeric(NA)),
             aggregate = new("SpecAgPlaceholder"))
    ## response from 'formulaMu' is "mean"
    x.wrong <- x
    x.wrong@formulaMu <- wrong ~ age + sex
    expect_error(validObject(x.wrong),
                 "response for formula 'wrong ~ age \\+ sex' is not 'mean'")
    ## 'lower' non-negative
    x.wrong <- x
    x.wrong@lower <- -1
    expect_error(validObject(x.wrong),
                 "'lower' is less than 0")
})

test_that("can create valid object of class SpecPoissonBinomialMixture", {
    ## nameY and series supplied
    x <- new("SpecPoissonBinomialMixture",
             call = call("Model", reg.deaths ~ PoissonBinomial(prob = 0.98)),
             nameY = new("Name", "reg.deaths"),
             prob = 0.98,
             series = new("SpecName", "deaths"))
    expect_true(validObject(x))
    ## series NULL
    x <- new("SpecPoissonBinomialMixture",
             call = call("Model", y ~ PoissonBinomial(prob = 0.98)),
             nameY = new("Name", "y"),
             prob = 0.98,
             series = new("SpecName", as.character(NA)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SpecNormalFixed", {
    ## nameY and series supplied
    x <- new("SpecNormalFixed",
             call = call("Model", reg.deaths ~ NormalFixed(mean = mean, sd = sd)),
             nameY = new("Name", "reg.deaths"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", "deaths"))
    expect_true(validObject(x))
    ## series NULL
    x <- new("SpecNormalFixed",
             call = call("Model", y ~ NormalFixed(mean = mean, sd = sd)),
             nameY = new("Name", "y"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", as.character(NA)))
    expect_true(validObject(x))
})

test_that("tests for SpecNormalFixed inherited from MeanSDMixin work", {
    x <- new("SpecNormalFixed",
             call = call("Model", reg.deaths ~ NormalFixed(mean = mean, sd = sd)),
             nameY = new("Name", "reg.deaths"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", "deaths"))
    ## 'mean' and 'sd' have the same length
    x.wrong <- x
    x.wrong@mean@.Data <- x.wrong@mean@.Data[-1]
    expect_error(validObject(x.wrong),
                 "'mean' and 'sd' have different lengths")
})

test_that("tests for SpecNormalFixed inherited from MeanSDMetadataMixin work", {
    x <- new("SpecNormalFixed",
             call = call("Model", reg.deaths ~ NormalFixed(mean = mean, sd = sd)),
             nameY = new("Name", "reg.deaths"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", "deaths"))
    ## 'metadata' does not have any dimensions with dimtype "iteration"
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = "iteration",
                            dimtypes = "iteration",
                            DimScales = list(new("Iterations", dimvalues = 1:10)))
    expect_error(validObject(x.wrong),
                 "dimension with dimtype \"iteration\"")
    ## 'metadata' does not have any dimensions with dimtype "quantile"
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = "quantile",
                            dimtypes = "quantile",
                            DimScales = list(new("Quantiles", dimvalues = seq(0.1, 0.9, length = 10))))
    expect_error(validObject(x.wrong),
                 "dimension with dimtype \"quantile\"")
    ## 'metadata' and 'mean' consistent
    x.wrong <- x
    x.wrong@mean@.Data <- x.wrong@mean@.Data[-1]
    x.wrong@sd@.Data <- x.wrong@sd@.Data[-1]
    expect_error(validObject(x.wrong),
                 "'mean' and 'metadata' inconsistent")    
})

test_that("can create valid object of class SpecRound3", {
    ## nameY and series supplied
    x <- new("SpecRound3",
             call = call("Model", reg.deaths ~ Round3()),
             nameY = new("Name", "reg.deaths"),
             series = new("SpecName", "deaths"))
    expect_true(validObject(x))
    ## series NULL
    x <- new("SpecRound3",
             call = call("Model", y ~ Round3()),
             nameY = new("Name", "y"),
             series = new("SpecName", as.character(NA)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SpecExact", {
    ## nameY and series supplied
    x <- new("SpecExact",
             call = call("Model", reg.deaths ~ Exact()),
             nameY = new("Name", "reg.deaths"),
             series = new("SpecName", "deaths"))
    expect_true(validObject(x))
    ## series NULL
    x <- new("SpecExact",
             call = call("Model", y ~ Exact()),
             nameY = new("Name", "y"),
             series = new("SpecName", as.character(NA)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SpecTFixed", {
    ## nameY and series supplied
    x <- new("SpecTFixed",
             call = call("Model", reg.deaths ~ TFixed(mean = mean, sd = sd)),
             nameY = new("Name", "reg.deaths"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", "deaths"))
    expect_true(validObject(x))
    ## series NULL
    x <- new("SpecTFixed",
             call = call("Model", y ~ TFixed(mean = mean, sd = sd)),
             nameY = new("Name", "y"),
             mean = new("ParameterVector", rnorm(10)),
             sd = new("ScaleVec", runif(10)),
             metadata = new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 0:10))),
             series = new("SpecName", as.character(NA)))
    expect_true(validObject(x))
})
