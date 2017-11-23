
context("SpecModel-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


test_that("Binomial works", {
    ans.obtained <- Binomial(mean ~ age + sex)
    ans.expected <- new("SpecLikelihoodBinomial",
                        formulaMu = mean ~ age + sex)
    expect_identical(ans.obtained, ans.expected)
    expect_error(Binomial(prob ~ age + sex),
                 "formula 'prob ~ age \\+ sex' does not have response 'mean'")
})

test_that("Normal works - varsigma unknown", {
    ans.obtained <- Normal(mean ~ age + sex)
    ans.expected <- new("SpecLikelihoodNormalVarsigmaUnknown",
                        formulaMu = mean ~ age + sex,
                        AVarsigma = new("SpecScale", as.double(NA)),
                        nuVarsigma = new("DegreesFreedom", 7),
                        varsigmaMax = new("SpecScale", as.numeric(NA)))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- Normal(mean ~ age + sex, priorSD = HalfT(df = 5, scale = 2))
    ans.expected <- new("SpecLikelihoodNormalVarsigmaUnknown",
                        formulaMu = mean ~ age + sex,
                        AVarsigma = new("SpecScale", 2),
                        nuVarsigma = new("DegreesFreedom", 5),
                        varsigmaMax = new("SpecScale", qhalft(p = 0.999, df = 5, scale = 2)))
    expect_identical(ans.obtained, ans.expected)
    expect_error(Normal(prob ~ age + sex),
                 "formula 'prob ~ age \\+ sex' does not have response 'mean'")
})

test_that("Normal works - varsigma known", {
    ans.obtained <- Normal(mean ~ age + sex, sd = 0.5)
    ans.expected <- new("SpecLikelihoodNormalVarsigmaKnown",
                        formulaMu = mean ~ age + sex,
                        varsigma = new("Scale", 0.5))
    expect_identical(ans.obtained, ans.expected)
    expect_error(Normal(prob ~ age + sex),
                 "formula 'prob ~ age \\+ sex' does not have response 'mean'")
})

test_that("Poisson works", {
    ans.obtained <- Poisson(mean ~ age + sex)
    ans.expected <- new("SpecLikelihoodPoisson",
                        formulaMu = mean ~ age + sex)
    expect_identical(ans.obtained, ans.expected)
    expect_error(Poisson(prob ~ age + sex),
                 "formula 'prob ~ age \\+ sex' does not have response 'mean'")
    expect_error(Poisson(mean ~ age + age:region),
                 paste("term 'region' is marginal to term 'age\\:region' but is not included",
                       "in formula 'mean \\~ age \\+ age\\:region'"))
})

test_that("PoissonBinomial works", {
    x.obtained <- PoissonBinomial(prob = 0.98)
    x.expected <- new("SpecLikelihoodPoissonBinomialMixture",
                      prob = 0.98)
    expect_identical(x.obtained, x.expected)
})

test_that("Model works", {
    ## binomial
    ans.obtained <- Model(death.reg ~ Binomial(mean ~ age * region + sex),
                          age ~ DLM(trend = NULL),
                          series = "deaths")
    call <- call("Model",
                 formula = death.reg ~ Binomial(mean ~ age * region + sex),
                 quote(age ~ DLM(trend = NULL)),
                 series = "deaths")
    ans.expected <- new("SpecBinomialVarying",
                        call = call,
                        nameY = new("Name", "death.reg"),
                        ASigma = new("SpecScale", 1),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age * region + sex,
                        lower = 0,
                        upper = 1,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM(trend = NULL)),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", "deaths"),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", qhalft(0.999, 7, 1)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    expect_error(Model(~ Binomial(prob ~ age)),
                 "'~Binomial\\(prob ~ age\\)' is not a valid formula for the likelihood")
    expect_error(Model(y ~ Wrong(prob ~ age)),
                 "'Wrong' is not a valid distribution")
    ## Poisson, standard 'y'
    ans.obtained <- Model(y ~ Poisson(mean ~ age * region + sex),
                          age ~ DLM(trend = NULL))
    call <- call("Model",
                 formula = y ~ Poisson(mean ~ age * region + sex),
                 quote(age ~ DLM(trend = NULL)))
    ans.expected <- new("SpecPoissonVarying",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", NA),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age * region + sex,
                        lower = 0,
                        upper = Inf,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM(trend = NULL)),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", NA),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", NA))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## Poisson,  'y' is 'death.reg'
    ans.obtained <- Model(death.reg ~ Poisson(mean ~ age * region + sex),
                          age ~ DLM(trend = NULL))
    call <- call("Model",
                 formula = death.reg ~ Poisson(mean ~ age * region + sex),
                 quote(age ~ DLM(trend = NULL)))
    ans.expected <- new("SpecPoissonVarying",
                        call = call,
                        nameY = new("Name", "death.reg"),
                        ASigma = new("SpecScale", 1),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age * region + sex,
                        lower = 0,
                        upper = Inf,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM(trend = NULL)),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", "y"),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", qhalft(0.999, 7, 1)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("NormalFixed works", {
    mean <- Values(array(1:3, dim = 3, dimnames = list(age = 0:2)))
    sd <- sqrt(mean)
    x <- NormalFixed(mean = mean, sd = sd)
    expect_is(x, "SpecLikelihoodNormalFixed")
    expect_true(validObject(x))
    x <- NormalFixed(mean = mean, sd = 1)
    expect_is(x, "SpecLikelihoodNormalFixed")
    expect_true(validObject(x))
    ## 'mean' is "Values"
    expect_error(NormalFixed(mean = "wrong", sd = sd),
                 "'mean' has class \"character\"")
    ## 'metadata' does not have any dimensions with dimtype "iteration"
    mean.wrong <- Values(array(1:3, dim = 3, dimnames = list(iteration = 1:3)))
    expect_error(NormalFixed(mean = mean.wrong, sd = sd),
                 "'mean' has dimension with dimtype \"iteration\"")
    ## 'metadata' does not have any dimensions with dimtype "quantile"
    mean.wrong <- Values(array(1:3, dim = 3, dimnames = list(quantile = c(0.1, 0.5, 0.9))))
    expect_error(NormalFixed(mean = mean.wrong, sd = sd),
                 "'mean' has dimension with dimtype \"quantile\"")
    ## 'mean' has no missing values
    mean.wrong <- mean
    mean.wrong[1] <- NA
    expect_error(NormalFixed(mean = mean.wrong, sd = sd),
                 "'mean' has missing values")
    ## 'sd' is compatible with 'mean'
    expect_error(NormalFixed(mean = mean, sd = sd[1:2]),
                 "'sd' and 'mean' not compatible :")
    ## 'sd' has no missing values
    sd.wrong <- sd
    sd.wrong[1] <- NA
    expect_error(NormalFixed(mean = mean, sd = sd.wrong),
                 "'sd' has missing values")
    ## 'sd' has no negative values
    sd.wrong <- sd
    sd.wrong[1] <- -1
    expect_error(NormalFixed(mean = mean, sd = sd.wrong),
                 "'sd' has negative values")
    ## 'sd' has length 1
    expect_error(NormalFixed(mean = mean, sd = 1:2),
                 "'sd' is numeric but does not have length 1")
    ## 'sd' is not missing
    expect_error(NormalFixed(mean = mean, sd = as.numeric(NA)),
                 "'sd' is missing")
    ## 'sd' is non-negative
    expect_error(NormalFixed(mean = mean, sd = -1),
                 "'sd' is negative")
    ## 'sd' is Values or numeric
    expect_error(NormalFixed(mean = mean, sd = "wrong"),
                 "'sd' has class \"character\"")
})

test_that("SpecModel works with SpecLikelihoodBinomial", {
    SpecModel <- demest:::SpecModel
    spec.inner <- Binomial(mean ~ age + sex)
    call <- call("Model",
                 quote(y ~ Binomial(mean ~ age + sex)),
                 quote(age ~ Exch()))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ Exch()),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecBinomialVarying",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", 1),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age + sex,
                        lower = 0,
                        upper = 1,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(Exch()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", as.character(NA)),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", qhalft(0.999, 7, 1)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Binomial(mean ~ age + sex)
    call <- call("Model",
                 quote(y ~ Binomial(mean ~ age + sex)),
                 quote(age ~ DLM()),
                 aggregate = quote(AgCertain(value = 5)))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ DLM()),
                              lower = 0.1,
                              upper = 0.9,
                              priorSD = HalfT(df = 5, scale = 10, max = 15),
                              jump = 0.2,
                              series = "deaths",
                              aggregate = AgCertain(value = 5))
    ans.expected <- new("SpecBinomialVarying",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", 10),
                        aggregate = AgCertain(value = 5),
                        formulaMu = mean ~ age + sex,
                        lower = 0.1,
                        upper = 0.9,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 5),
                        series = new("SpecName", "deaths"),
                        scaleTheta = new("Scale", 0.2),
                        sigmaMax = new("SpecScale", 15))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Binomial(mean ~ age + sex)
    expect_error(SpecModel(specInner = spec.inner,
                             call = call,
                             nameY = new("Name", "y"),
                             dots = list(age ~ Exch()),
                             lower = NULL,
                             upper = NULL,
                             priorSD = NULL,
                             jump = 0.2,
                             series = NULL,
                             aggregate = AgPoisson(1)),
                 "Poisson model for accuracy of aggregate values cannot be combined with binomial likelihood")
})

test_that("SpecModel works with SpecLikelihoodNormalVarsigmaKnown", {
    SpecModel <- demest:::SpecModel
    spec.inner <- Normal(mean ~ age + sex, sd = 1.2)
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex, sd = 1.2)),
                 quote(age ~ Exch()))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ Exch()),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecNormalVaryingVarsigmaKnown",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", NA),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age + sex,
                        lower = -Inf,
                        upper = Inf,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(Exch()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", as.character(NA)),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", NA),
                        varsigma = new("Scale", 1.2))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Normal(mean ~ age + sex, sd = 0.3)
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex, sd = 0.3)),
                 quote(age ~ DLM()),
                 aggregate = quote(AgCertain(value = 5)))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ DLM()),
                              lower = 0.1,
                              upper = 10,
                              priorSD = HalfT(df = 5, scale = 10, max = 20),
                              jump = 0.2,
                              series = "deaths",
                              aggregate = AgCertain(value = 5))
    ans.expected <- new("SpecNormalVaryingVarsigmaKnown",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", 10),
                        aggregate = AgCertain(value = 5),
                        formulaMu = mean ~ age + sex,
                        lower = 0.1,
                        upper = 10,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 5),
                        series = new("SpecName", "deaths"),
                        scaleTheta = new("Scale", 0.2),
                        sigmaMax = new("SpecScale", 20),
                        varsigma = new("Scale", 0.3))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Normal(mean ~ age + sex, sd = 1.2)
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex, sd = 1.2)),
                 quote(age ~ Exch()))
    expect_warning(SpecModel(specInner = spec.inner,
                             call = call,
                             nameY = new("Name", "y"),
                             dots = list(age ~ Exch()),
                             lower = NULL,
                             upper = NULL,
                             priorSD = NULL,
                             jump = 0.2,
                             series = NULL,
                             aggregate = NULL),
                   "'jump' is ignored in Normal model when 'aggregate' is NULL")
    spec.inner <- Normal(mean ~ age + sex)
    expect_error(SpecModel(specInner = spec.inner,
                             call = call,
                             nameY = new("Name", "y"),
                             dots = list(age ~ Exch()),
                             lower = NULL,
                             upper = NULL,
                             priorSD = NULL,
                             jump = 0.2,
                             series = NULL,
                             aggregate = AgPoisson(1)),
                 "Poisson model for accuracy of aggregate values cannot be combined with normal likelihood")
})

test_that("SpecModel works with SpecLikelihoodNormalVarsigmaUnknown", {
    SpecModel <- demest:::SpecModel
    spec.inner <- Normal(mean ~ age + sex)
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex)),
                 quote(age ~ Exch()))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ Exch()),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecNormalVaryingVarsigmaUnknown",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", NA),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age + sex,
                        lower = -Inf,
                        upper = Inf,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(Exch()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", as.character(NA)),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", NA),
                        AVarsigma = new("SpecScale", as.numeric(NA)),
                        nuVarsigma = new("DegreesFreedom", 7),
                        varsigmaMax = new("SpecScale", as.numeric(NA)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Normal(mean ~ age + sex, priorSD = HalfT(scale = 5))
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex, priorSD = HalfT(scale = 5))),
                 quote(age ~ DLM()),
                 aggregate = quote(AgCertain(value = 5)))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ DLM()),
                              lower = 0.1,
                              upper = 10,
                              priorSD = NULL,
                              jump = 0.2,
                              series = "deaths",
                              aggregate = AgCertain(value = 5))
    ans.expected <- new("SpecNormalVaryingVarsigmaUnknown",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", as.numeric(NA)),
                        aggregate = AgCertain(value = 5),
                        formulaMu = mean ~ age + sex,
                        lower = 0.1,
                        upper = 10,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", "deaths"),
                        scaleTheta = new("Scale", 0.2),
                        sigmaMax = new("SpecScale", NA),
                        AVarsigma = new("SpecScale", as.numeric(5)),
                        nuVarsigma = new("DegreesFreedom", 7),
                        varsigmaMax = new("SpecScale", qhalft(0.999, 7, 5)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Normal(mean ~ age + sex, sd = 1.2)
    call <- call("Model",
                 quote(y ~ Normal(mean ~ age + sex)),
                 quote(age ~ Exch()))
    expect_warning(SpecModel(specInner = spec.inner,
                             call = call,
                             nameY = new("Name", "y"),
                             dots = list(age ~ Exch()),
                             lower = NULL,
                             upper = NULL,
                             priorSD = NULL,
                             jump = 0.2,
                             series = NULL,
                             aggregate = NULL),
                   "'jump' is ignored in Normal model when 'aggregate' is NULL")
    spec.inner <- Normal(mean ~ age + sex)
    expect_error(SpecModel(specInner = spec.inner,
                           call = call,
                           nameY = new("Name", "y"),
                           dots = list(age ~ Exch()),
                           lower = NULL,
                           upper = NULL,
                           priorSD = NULL,
                           jump = 0.2,
                           series = NULL,
                           aggregate = AgPoisson(1)),
                 "Poisson model for accuracy of aggregate values cannot be combined with normal likelihood")
})

test_that("SpecModel works with SpecLikelihoodPoisson", {
    SpecModel <- demest:::SpecModel
    spec.inner <- Poisson(mean ~ age + sex)
    call <- call("Model",
                 quote(y ~ Poisson(mean ~ age + sex)),
                 quote(age ~ Exch()))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ Exch()),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecPoissonVarying",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", NA),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age + sex,
                        lower = 0,
                        upper = Inf,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(Exch()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", as.character(NA)),
                        scaleTheta = new("Scale", 0.1),
                        sigmaMax = new("SpecScale", NA))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Poisson(mean ~ age + sex)
    call <- call("Model",
                 quote(y ~ Poisson(mean ~ age + sex)),
                 quote(age ~ DLM()),
                 aggregate = quote(AgCertain(value = 5)))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(age ~ DLM()),
                              lower = 0.1,
                              upper = 10,
                              priorSD = HalfT(df = 5, scale = 10, max = 15),
                              jump = 0.2,
                              series = "deaths",
                              aggregate = AgCertain(value = 5))
    ans.expected <- new("SpecPoissonVarying",
                        call = call,
                        nameY = new("Name", "y"),
                        ASigma = new("SpecScale", 10),
                        aggregate = AgCertain(value = 5),
                        formulaMu = mean ~ age + sex,
                        lower = 0.1,
                        upper = 10,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 5),
                        series = new("SpecName", "deaths"),
                        scaleTheta = new("Scale", 0.2),
                        sigmaMax = new("SpecScale", 15))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- Poisson(mean ~ age + sex)
    call <- call("Model",
                 quote(reg.birth ~ Poisson(mean ~ age + sex)),
                 quote(age ~ DLM()))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "reg.birth"),
                              dots = list(age ~ DLM()),
                              lower = 0.1,
                              upper = 10,
                              priorSD = HalfT(),
                              jump = 0.2,
                              series = "y",
                              aggregate = NULL)
    ans.expected <- new("SpecPoissonVarying",
                        call = call,
                        nameY = new("Name", "reg.birth"),
                        ASigma = new("SpecScale", 1),
                        aggregate = new("SpecAgPlaceholder"),
                        formulaMu = mean ~ age + sex,
                        lower = 0.1,
                        upper = 10,
                        tolerance = 1e-5,
                        maxAttempt = 100L,
                        specsPriors = list(DLM()),
                        namesSpecsPriors = "age",
                        nuSigma = new("DegreesFreedom", 7),
                        series = new("SpecName", "y"),
                        scaleTheta = new("Scale", 0.2),
                        sigmaMax = new("SpecScale", qhalft(0.999, 7, 1)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("SpecModel works with SpecPoissonBinomialMixture", {
    SpecModel <- demest:::SpecModel
    spec.inner <- PoissonBinomial(prob = 0.99)
    call <- call("Model",
                 formula = y ~ PoissonBinomial(prob = 0.99))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecPoissonBinomialMixture",
                        call = call,
                        nameY = new("Name", "y"),
                        series = new("SpecName", as.character(NA)),
                        prob = 0.99)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- PoissonBinomial(prob = 0.99)
    call <- call("Model",
                 formula = deaths.reg ~ PoissonBinomial(prob = 0.99),
                 series = "deaths")
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "deaths.reg"),
                              dots = list(),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = "deaths",
                              aggregate = NULL)
    ans.expected <- new("SpecPoissonBinomialMixture",
                        call = call,
                        nameY = new("Name", "deaths.reg"),
                        series = new("SpecName", "deaths"),
                        prob = 0.99)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("SpecModel works with SpecNormalFixed", {
    SpecModel <- demest:::SpecModel
    mean <- Values(array(1:4,
                         dim = c(2, 2),
                         dimnames = list(age = c("0-39", "40"),
                                         sex = c("Female", "Male"))))
    spec.inner <- NormalFixed(mean = mean, sd = 1)
    call <- call("Model",
                 formula = y ~ NormalFixed(mean = mean, sd = 1))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    ans.expected <- new("SpecNormalFixed",
                        call = call,
                        nameY = new("Name", "y"),
                        series = new("SpecName", as.character(NA)),
                        mean = new("ParameterVector", 1 * 1:4),
                        sd = new("ScaleVec", rep(1, 4)),
                        metadata = mean@metadata)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    spec.inner <- NormalFixed(mean = mean, sd = 1)
    call <- call("Model",
                 formula = deaths.reg ~ NormalFixed(mean = mean, sd = 1),
                 series = "deaths")
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "deaths.reg"),
                              dots = list(),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = "deaths",
                              aggregate = NULL)
    ans.expected <- new("SpecNormalFixed",
                        call = call,
                        nameY = new("Name", "deaths.reg"),
                        series = new("SpecName", "deaths"),
                        mean = new("ParameterVector", 1 * 1:4),
                        sd = new("ScaleVec", rep(1, 4)),
                        metadata = mean@metadata)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})



## Aggregate ###################################################################

test_that("AgCertain works", {
    Concordance <- classconc::Concordance
    ans.obtained <- AgCertain(value = 4, weights = NULL)
    ans.expected <- new("SpecAgCertain",
                        metadataAg = NULL,
                        valueAg = new("ParameterVector", 4),
                        weightAg = NULL)
    expect_identical(ans.obtained, ans.expected)
    value <- Counts(array(1:3,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    weights <- Counts(array(1,
                            dim = c(4, 3),
                            dimnames = list(region = 1:4,
                                            time = c(2000, 2005, 2010))))
    concordances <- list(region = Concordance(data.frame(old = 1:12, new = rep(1:4, each = 3))))
    ans.obtained <- AgCertain(value = value, weights = weights, concordance = concordances)
    ans.expected <- new("SpecAgCertain",
                        metadataAg = value@metadata,
                        valueAg = new("ParameterVector", as.double(value)),
                        weightAg = weights,
                        concordancesAg = concordances)
    expect_identical(ans.obtained, ans.expected)
})

test_that("AgNormal works", {
    Concordance <- classconc::Concordance
    ans.obtained <- AgNormal(value = 4, sd = 3, weights = NULL)
    ans.expected <- new("SpecAgNormal",
                        metadataAg = NULL,
                        scaleAg = new("Scale", 0.1),
                        sdAg = new("ScaleVec", 3),
                        valueAg = new("ParameterVector", 4),
                        weightAg = NULL)
    expect_identical(ans.obtained, ans.expected)
    value <- Counts(array(1:3,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    sd <- Counts(array(3:1,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    weights <- Counts(array(1,
                            dim = c(4, 3),
                            dimnames = list(region = 1:4,
                                time = c(2000, 2005, 2010))))
    concordances <- list(region = Concordance(data.frame(old = 1:12, new = rep(1:4, each = 3))))
    ans.obtained <- AgNormal(value = value, sd = sd, weights = weights,
                             concordances = concordances, jump = 0.3)
    ans.expected <- new("SpecAgNormal",
                        metadataAg = value@metadata,
                        scaleAg = new("Scale", 0.3),
                        sdAg = new("ScaleVec", as.double(3:1)),
                        valueAg = new("ParameterVector", as.double(1:3)),
                        weightAg = weights,
                        concordancesAg = concordances)
    expect_identical(ans.obtained, ans.expected)
})

test_that("AgFun works", {
    Concordance <- classconc::Concordance
    FUN <- function(x, weights) sum(x)
    ans.obtained <- AgFun(value = 4, sd = 3, FUN = FUN)
    ans.expected <- new("SpecAgFun",
                        metadataAg = NULL,
                        sdAg = new("ScaleVec", 3),
                        valueAg = new("ParameterVector", 4),
                        weightAg = NULL,
                        funAg = FUN)
    expect_identical(ans.obtained, ans.expected)
    value <- Counts(array(1:3,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    sd <- Counts(array(3:1,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    weights <- Counts(array(1,
                            dim = c(4, 3),
                            dimnames = list(region = 1:4,
                                time = c(2000, 2005, 2010))))
    concordances <- list(region = Concordance(data.frame(old = 1:12, new = rep(1:4, each = 3))))
    ans.obtained <- AgFun(value = value, sd = sd, weights = weights,
                          concordances = concordances, FUN = FUN)
    ans.expected <- new("SpecAgFun",
                        funAg = FUN,
                        metadataAg = value@metadata,
                        sdAg = new("ScaleVec", as.double(3:1)),
                        valueAg = new("ParameterVector", as.double(1:3)),
                        weightAg = weights,
                        concordancesAg = concordances)
    expect_identical(ans.obtained, ans.expected)
})

test_that("AgLife works", {
    Concordance <- classconc::Concordance
    ## value is scalar
    ans.obtained <- AgLife(value = 80, sd = 3)
    ans.expected <- new("SpecAgLife",
                        metadataAg = NULL,
                        sdAg = new("ScaleVec", 3),
                        valueAg = new("ParameterVector", 80),
                        axAg = NULL)
    expect_identical(ans.obtained, ans.expected)
    ## value is demographic array
    value <- Counts(array(80:82,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    sd <- Counts(array(3:1,
                       dim = 3,
                       dimnames = list(time = c(2000, 2005, 2010))))
    concordances <- list(region = Concordance(data.frame(old = 1:12, new = rep(1:4, each = 3))))
    ax <- Values(array(c(0.1, 0.2, 0.1),
                       dim = c(1, 3),
                       dimnames = list(age = "0",
                                       time = c(2000, 2005, 2010))),
                 dimscales = c(age = "Intervals"))
    ans.obtained <- AgLife(value = value, sd = sd, ax = ax,
                           concordances = concordances)
    ans.expected <- new("SpecAgLife",
                        metadataAg = value@metadata,
                        sdAg = new("ScaleVec", as.double(3:1)),
                        valueAg = new("ParameterVector", as.double(80:82)),
                        axAg = ax,
                        concordancesAg = concordances)
    expect_identical(ans.obtained, ans.expected)
    ## value has age dimension
    value <- Counts(array(80:82,
                          dim = 3,
                          dimnames = list(age = c("0", "1-4", "5-9"))))
    sd <- Counts(array(3:1,
                       dim = 3,
                       dimnames = list(age = c("0", "1-4", "5-9"))))
    ax <- Values(array(0.1,
                       dim = 1,
                       dimnames = list(age = "0")),
                 dimscales = c(age = "Intervals"))
    expect_error(AgLife(value = value, sd = sd, ax = ax),
                 "'values' has a dimension with dimtype \"age\"")
})

test_that("AgPoisson works", {
    Concordance <- classconc::Concordance
    ans.obtained <- AgPoisson(value = 4)
    ans.expected <- new("SpecAgPoisson",
                        metadataAg = NULL,
                        scaleAg = new("Scale", 0.1),
                        valueAg = new("ParameterVector", 4))
    expect_identical(ans.obtained, ans.expected)
    value <- Counts(array(1:3,
                          dim = 3,
                          dimnames = list(time = c(2000, 2005, 2010))))
    concordances <- list(region = Concordance(data.frame(old = 1:12, new = rep(1:4, each = 3))))
    ans.obtained <- AgPoisson(value = value, 
                              concordances = concordances, jump = 0.3)
    ans.expected <- new("SpecAgPoisson",
                        metadataAg = value@metadata,
                        scaleAg = new("Scale", 0.3),
                        valueAg = new("ParameterVector", as.double(1:3)),
                        concordancesAg = concordances)
    expect_identical(ans.obtained, ans.expected)
})




