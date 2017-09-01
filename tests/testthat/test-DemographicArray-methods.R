
context("DemographicArray-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


## castExposure #######################################################################

test_that("castExposure works with Binomial", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("BinomialVarying")
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     toInteger(exposure))
    exposure <- exposure + 0.5
    expect_error(castExposure(exposure = exposure, model = model),
                 "'exposure' cannot be coerced to integer : non-integer values")
    expect_identical(castExposure(exposure = NULL, model = model),
                     NULL)
})

test_that("castExposure works with Poisson", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("PoissonVaryingUseExp")
    expect_identical(castExposure(exposure = exposure, model = model),
                     toDouble(exposure))
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- exposure + 0.5
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    expect_identical(castExposure(exposure = NULL, model = model),
                     NULL)
})

test_that("castExposure works with Normal", {
    castExposure <- demest:::castExposure
    model <- new("NormalVaryingVarsigmaKnown")
    expect_identical(castExposure(exposure = NULL, model = model),
                     NULL)
})

test_that("castExposure works with PoissonBinomialMixture", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("PoissonBinomialMixture")
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     toInteger(exposure))
    exposure <- exposure + 0.5
    expect_error(castExposure(exposure = exposure, model = model),
                 "'exposure' cannot be coerced to integer : non-integer values")
})


test_that("castExposure works with SpecBinomial", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- Model(y ~ Binomial(mean ~ sex))
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     toInteger(exposure))
    exposure <- exposure + 0.5
    expect_error(castExposure(exposure = exposure, model = model),
                 "'exposure' cannot be coerced to integer : non-integer values")
})

test_that("castExposure works with SpecPoisson", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ sex))
    expect_identical(castExposure(exposure = exposure, model = model),
                     toDouble(exposure))
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- exposure + 0.5
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    expect_identical(castExposure(exposure = NULL, model = model),
                     NULL)
})

test_that("castExposure works with SpecNormal", {
    castExposure <- demest:::castExposure
    model <- Model(y ~ Normal(mean ~ sex))
    expect_identical(castExposure(exposure = NULL, model = model),
                     NULL)
})

test_that("castExposure works with SpecPoissonBinomialMixture", {
    castExposure <- demest:::castExposure
    exposure <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- Model(y ~ PoissonBinomial(prob = 0.98))
    expect_identical(castExposure(exposure = exposure, model = model),
                     exposure)
    exposure <- toDouble(exposure)
    expect_identical(castExposure(exposure = exposure, model = model),
                     toInteger(exposure))
    exposure <- exposure + 0.5
    expect_error(castExposure(exposure = exposure, model = model),
                 "'exposure' cannot be coerced to integer : non-integer values")
})


## castY ############################################################################

test_that("castY works with SpecBinomial", {
    castY <- demest:::castY
    y <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    spec <- Model(y ~ Binomial(mean ~ sex))
    expect_identical(castY(y = y, spec = spec),
                     y)
    y <- toDouble(y)
    expect_identical(castY(y = y, spec = spec),
                     toInteger(y))
    y <- y + 0.5
    expect_error(castY(y = y, spec = spec),
                 "'y' cannot be coerced to integer : non-integer values")
})

test_that("castY works with SpecNormal", {
    castY <- demest:::castY
    y <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    spec <- Model(y ~ Normal(mean ~ sex))
    expect_identical(castY(y = y, spec = spec),
                     toDouble(y))
    y <- toDouble(y)
    expect_identical(castY(y = y, spec = spec),
                     y)
    y <- y + 0.5
    expect_identical(castY(y = y, spec = spec),
                     y)
})

test_that("castY works with SpecBinomial", {
    castY <- demest:::castY
    y <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    spec <- Model(y ~ Poisson(mean ~ sex))
    expect_identical(castY(y = y, spec = spec),
                     y)
    y <- toDouble(y)
    expect_identical(castY(y = y, spec = spec),
                     toInteger(y))
    y <- y + 0.5
    expect_error(castY(y = y, spec = spec),
                 "'y' cannot be coerced to integer : non-integer values")
})

test_that("castY works with SpecBinomial", {
    castY <- demest:::castY
    y <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    spec <- Model(y ~ PoissonBinomial(prob = 0.98))
    expect_identical(castY(y = y, spec = spec),
                     y)
    y <- toDouble(y)
    expect_identical(castY(y = y, spec = spec),
                     toInteger(y))
    y <- y + 0.5
    expect_error(castY(y = y, spec = spec),
                 "'y' cannot be coerced to integer : non-integer values")
})


## castPopnOrSampled ##################################################################

test_that("castPopnOrSampled works as expected", {
    castPopnOrSampled <- demest:::castPopnOrSampled
    ## Binomial
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("BinomialVarying")
    expect_identical(castPopnOrSampled(x, model = model, name = "population"),
                     x)
    expect_identical(castPopnOrSampled(toDouble(x), model = model, name = "population"),
                     x)
    expect_error(castPopnOrSampled(x + 0.01, model = model, name = "population"),
                 "'population' cannot be coerced to integer : non-integer values")
    ## Poisson
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("PoissonVaryingUseExp")
    expect_identical(castPopnOrSampled(x, model = model, name = "population"),
                     toDouble(x))
    expect_identical(castPopnOrSampled(toDouble(x), model = model, name = "population"),
                     toDouble(x))
    ## Normal
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("NormalVaryingVarsigmaKnown")
    expect_identical(castPopnOrSampled(x, model = model, name = "population"),
                     toDouble(x))
    expect_identical(castPopnOrSampled(toDouble(x), model = model, name = "population"),
                     toDouble(x))
    ## PoissonBinomial
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("PoissonBinomialMixture")
    expect_identical(castPopnOrSampled(x, model = model, name = "population"),
                     x)
    expect_identical(castPopnOrSampled(toDouble(x), model = model, name = "population"),
                     x)
    expect_error(castPopnOrSampled(x + 0.01, model = model, name = "sample"),
                 "'sample' cannot be coerced to integer : non-integer values")
})


## checkForSubtotals ########################################################################

test_that("checkForSubtotals works when y is ordinary Counts object", {
    checkForSubtotals <- demest:::checkForSubtotals
    y <- CountsOne(1:5, labels = 0:4, name = "age")
    model <- new("SpecNormalVaryingVarsigmaKnown")
    expect_identical(checkForSubtotals(object = y, model = model),
                     NULL)
})

test_that("checkForSubtotals works when y has subtotals and model is Binomial", {
    checkForSubtotals <- demest:::checkForSubtotals
    y <- CountsOne(1:5, labels = 0:4, name = "age")
    subtotals <- CountsOne(3, labels = "0-1", name = "age")
    y[1:2] <- NA
    y <- attachSubtotals(y, subtotals = subtotals)
    model <- new("SpecBinomialVarying")
    expect_error(checkForSubtotals(object = y, model = model),
                 "'y' has subtotals but model has class \"Binomial\"")
})

test_that("checkForSubtotals works when y has subtotals and model is Normal", {
    checkForSubtotals <- demest:::checkForSubtotals
    y <- CountsOne(1:5, labels = 0:4, name = "age")
    subtotals <- CountsOne(3, labels = "0-1", name = "age")
    y[1:2] <- NA
    y <- attachSubtotals(y, subtotals = subtotals)
    model <- new("SpecNormalVaryingVarsigmaKnown")
    expect_error(checkForSubtotals(object = y, model = model),
                 "'y' has subtotals but model has class \"Normal\"")
})

test_that("checkForSubtotals works when y has subtotals and model is PoissonBinomialMixture", {
    checkForSubtotals <- demest:::checkForSubtotals
    y <- CountsOne(1:5, labels = 0:4, name = "age")
    subtotals <- CountsOne(3, labels = "0-1", name = "age")
    y[1:2] <- NA
    y <- attachSubtotals(y, subtotals = subtotals)
    model <- new("SpecPoissonBinomialMixture")
    expect_error(checkForSubtotals(object = y, model = model),
                 "'y' has subtotals but model has class \"PoissonBinomialMixture\"")
})

test_that("checkForSubtotals works when y has subtotals and model is character", {
    checkForSubtotals <- demest:::checkForSubtotals
    y <- CountsOne(1:5, labels = 0:4, name = "age")
    subtotals <- CountsOne(3, labels = "0-1", name = "age")
    y[1:2] <- NA
    y <- attachSubtotals(y, subtotals = subtotals)
    model <- "wrong"
    expect_error(checkForSubtotals(object = y, model = model, name = "XX"),
                 "'XX' has subtotals but specification has class \"character\"")
})


## classY ############################################################################

test_that("classY works objects of class Demographic", {
    classY <- demest:::classY
    y <- Counts(array(1:3, dim = 3, dimnames = list(age = 0:2)))
    expect_identical(classY(y), "Counts")
    y <- Values(array(1:3, dim = 3, dimnames = list(age = 0:2)))
    expect_identical(classY(y), "Values")
    expect_error(classY("wrong"),
                 "cannot handle 'y' with class \"character\"")
})


## combineEstPred #######################################################

test_that("Counts method for combineEstPred works", {
    combineEstPred <- demest:::combineEstPred
    est <- Counts(array(rnorm(12),
                        dim = 4:3,
                        dimnames = list(age = 0:3, time = 1:3)),
                  dimscales = c(age = "Intervals", time = "Intervals"))
    pred <- Counts(array(rnorm(16),
                         dim = c(4, 4),
                         dimnames = list(age = 0:3, time = 4:7)),
                   dimscales = c(age = "Intervals", time = "Intervals"))
    ans.obtained <- combineEstPred(est = est, pred = pred)
    ans.expected <- dbind(est, pred, along = "time")
    expect_identical(ans.obtained, ans.expected)
    ## invalid inputs
    est <- Counts(array(rnorm(12),
                        dim = 4:3,
                        dimnames = list(age = 0:3, time = 1:3)),
                  dimscales = c(age = "Intervals", time = "Intervals"))
    pred <- Counts(array(rnorm(16),
                         dim = c(4, 4),
                         dimnames = list(age = 0:3, time = 5:8)),
                   dimscales = c(age = "Intervals", time = "Intervals"))
    expect_error(combineEstPred(est = est, pred = pred),
                 "new \"along\" dimension \\[\"time\"\\] has gaps between intervals")
})

test_that("Values method for combineEstPred works", {
    combineEstPred <- demest:::combineEstPred
    est <- Values(array(rnorm(12),
                        dim = 4:3,
                        dimnames = list(age = 0:3, time = 1:3)),
                  dimscales = c(age = "Intervals", time = "Intervals"))
    pred <- Values(array(rnorm(16),
                         dim = c(4, 4),
                         dimnames = list(age = 0:3, time = 4:7)),
                   dimscales = c(age = "Intervals", time = "Intervals"))
    ans.obtained <- combineEstPred(est = est, pred = pred)
    ans.expected <- dbind(est, pred, along = "time")
    expect_identical(ans.obtained, ans.expected)
    ## invalid inputs
    est <- Values(array(rnorm(12),
                        dim = 4:3,
                        dimnames = list(age = 0:3, time = 1:3)),
                  dimscales = c(age = "Intervals", time = "Intervals"))
    pred <- Values(array(rnorm(12),
                         dim = c(3, 4),
                         dimnames = list(age = 0:2, time = 4:7)),
                   dimscales = c(age = "Intervals", time = "Intervals"))
    expect_error(combineEstPred(est = est, pred = pred),
                 "results from 'est' and 'pred' have incompatible dimensions or 'dimscales'")
})                       
                           


## concatDimScaleFirstSecond ########################################################

test_that("default method for concatDimScaleFirstSecond works", {
    concatDimScaleFirstSecond <- demest:::concatDimScaleFirstSecond
    ## same dimscales
    first <- new("Iterations", dimvalues = 1:10)
    second <- new("Iterations", dimvalues = 11:20)
    name <- "iteration"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "\"along\" dimension \\[\"iteration\"\\] has dimscale \"Iterations")
    ## different dimscales
    first <- new("Points", dimvalues = 1:10)
    second <- new("Intervals", dimvalues = 10:20)
    name <- "age"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "new \"along\" dimension \\[\"age\"\\] has dimscales \"Points\" and \"Intervals\"")
})

test_that("Categories method for concatDimScaleFirstSecond works", {
    concatDimScaleFirstSecond <- demest:::concatDimScaleFirstSecond
    first <- new("Categories", dimvalues = c("a", "b"))
    second <- new("Categories", dimvalues = c("c", "d"))
    name <- "region"
    expect_identical(concatDimScaleFirstSecond(first = first, second = second,
                                               name = name),
                     new("Categories", dimvalues = c("a", "b", "c", "d")))
    first <- new("Categories", dimvalues = c("a", "b", "c"))
    second <- new("Categories", dimvalues = c("c", "d"))
    name <- "region"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "new \"along\" dimension \\[\"region\"\\] has duplicated categories")
})

test_that("Points method for concatDimScaleFirstSecond works", {
    concatDimScaleFirstSecond <- demest:::concatDimScaleFirstSecond
    first <- new("Points", dimvalues = 1:5)
    second <- new("Points", dimvalues = 6:10)
    name <- "time"
    expect_identical(concatDimScaleFirstSecond(first = first, second = second,
                                               name = name),
                     new("Points", dimvalues = 1:10))
    first <- new("Points", dimvalues = 1:6)
    second <- new("Points", dimvalues = 6:10)
    name <- "time"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "new \"along\" dimension \\[\"time\"\\] has overlapping points")
})

test_that("Points method for concatDimScaleFirstSecond works", {
    concatDimScaleFirstSecond <- demest:::concatDimScaleFirstSecond
    first <- new("Intervals", dimvalues = 0:5)
    second <- new("Intervals", dimvalues = 5:10)
    name <- "time"
    expect_identical(concatDimScaleFirstSecond(first = first, second = second,
                                               name = name),
                     new("Intervals", dimvalues = 0:10))
    first <- new("Intervals", dimvalues = 0:6)
    second <- new("Intervals", dimvalues = 5:10)
    name <- "time"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "new \"along\" dimension \\[\"time\"\\] has overlapping intervals")
    first <- new("Intervals", dimvalues = 0:4)
    second <- new("Intervals", dimvalues = 5:10)
    name <- "time"
    expect_error(concatDimScaleFirstSecond(first = first, second = second,
                                           name = name),
                 "new \"along\" dimension \\[\"time\"\\] has gaps between intervals")
})


## equivalentSample ##############################################################

test_that("equivalentSample works", {
    equivalentSample <- demest:::equivalentSample
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## binomial
        mean <- Values(array(runif(n = 12, min = 0, max = 1),
                             dim = 4:3,
                             dimnames = list(reg = 1:4,
                                 age = c("0-4", "5-9", "10+"))))
        se <- Values(array(runif(n = 12, min = 0.1, max = 0.1),
                           dim = 4:3,
                           dimnames = list(reg = 1:4,
                               age = c("0-4", "5-9", "10+"))))
        ans.obtained <- equivalentSample(mean = mean, se = se)
        ans.expected <- list(y = as(toInteger(mean^2 * (1-mean) / se^2, force = TRUE), "Counts"),
                             exposure = as(toInteger(mean * (1-mean) / se^2, force = TRUE), "Counts"))
        expect_identical(ans.obtained, ans.expected)
        ## Poisson
        mean <- Values(array(runif(n = 12, min = 0, max = 20),
                             dim = 4:3,
                             dimnames = list(reg = 1:4,
                                 age = c("0-4", "5-9", "10+"))))
        se <- Values(array(runif(n = 12, min = 0.1, max = 2),
                           dim = 4:3,
                           dimnames = list(reg = 1:4,
                               age = c("0-4", "5-9", "10+"))))
        ans.obtained <- equivalentSample(mean = mean, se = se, to = "Poisson")
        ans.expected <- list(y = as(toInteger(mean^2  / se^2, force = TRUE), "Counts"),
                             exposure = as(mean / se^2, "Counts"))
        expect_identical(ans.obtained, ans.expected)
        ## 0s and NAs in right place
        mean <- Values(array(runif(n = 12, min = 0, max = 1),
                             dim = 4:3,
                             dimnames = list(reg = 1:4,
                                 age = c("0-4", "5-9", "10+"))))
        se <- Values(array(runif(n = 12, min = 0.1, max = 0.5),
                           dim = 4:3,
                           dimnames = list(reg = 1:4,
                               age = c("0-4", "5-9", "10+"))))
        mean[1] <- NA
        se[2] <- 0.5 * 1e-6
        ans.obtained <- equivalentSample(mean = mean, se = se)
        expect_identical(is.na(as.numeric(ans.obtained$y)), rep(c(TRUE, FALSE), times = c(2, 10)))
        expect_identical(is.na(as.numeric(ans.obtained$exposure)), rep(c(TRUE, FALSE), times = c(2, 10)))
        ## y equals exposure when mean is 0 and to is "binomial"
        mean <- Values(array(runif(n = 12, min = 0, max = 1),
                             dim = 4:3,
                             dimnames = list(reg = 1:4,
                                 age = c("0-4", "5-9", "10+"))))
        se <- Values(array(runif(n = 12, min = 0.1, max = 10),
                           dim = 4:3,
                           dimnames = list(reg = 1:4,
                               age = c("0-4", "5-9", "10+"))))
        mean[1] <- 1
        ans.obtained <- equivalentSample(mean = mean, se = se)
        expect_identical(ans.obtained$y[[1]], ans.obtained$exposure[[1]])
    }
})


test_that("equivalentSample throws appropriate errors", {
    equivalentSample <- demest:::equivalentSample
    mean <- Values(array(runif(n = 12, min = 0, max = 1),
                         dim = 4:3,
                         dimnames = list(reg = 1:4,
                             age = c("0-4", "5-9", "10+"))))
    se <- Values(array(runif(n = 12, min = 0.1, max = 0.1),
                       dim = 4:3,
                       dimnames = list(reg = 1:4,
                           age = c("0-4", "5-9", "10+"))))
    mean.wrong <- mean
    mean.wrong[1] <- -0.001
    expect_error(equivalentSample(mean = mean.wrong, se = se),
                 "'mean' has negative values")
    se.wrong <- se
    se.wrong[1] <- -0.001
    expect_error(equivalentSample(mean = mean, se = se.wrong),
                 "'se' has negative values")
    se.wrong <- subarray(se, age < 10)
    expect_error(equivalentSample(mean = mean, se = se.wrong),
                 "'se' not compatible with 'mean' :")
    mean.wrong <- mean
    mean.wrong[1] <- 1.001
    expect_error(equivalentSample(mean = mean.wrong, se = se),
                 "'to' is \"binomial\" but 'mean' has values greater than 1")
})


## sweepAllMargins ###############################################################

test_that("sweepAllMargins works with arrays", {
    sweepAllMargins <- demest:::sweepAllMargins
    ## dim = c(10, 5)
    x <- array(rnorm(50), dim = c(10, 5))
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - rowMeans(x)
    ans.expected <- ans.expected - rep(colMeans(ans.expected), each = 10)
    expect_equal(ans.obtained, ans.expected)
    ## dim = 10
    x <- array(rnorm(50), dim = 50)
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - mean(x)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(10, 10, 5)
    x <- array(rnorm(500), dim = c(10, 10, 5))
    ans.obtained <- sweepAllMargins(x)
    expect_equal(apply(ans.obtained, 1, mean),
                 rep(0, 10))
    expect_equal(apply(ans.obtained, 1:2, mean),
                 array(0, dim = c(10, 10)))
    expect_equal(apply(ans.obtained, c(1, 3), mean),
                 array(0, dim = c(10, 5)))
})

test_that("sweepAllMargins works with Values - no iteration dimension", {
    sweepAllMargins <- demest:::sweepAllMargins
    ## dim = c(10, 5)
    x <- Values(array(rnorm(50),
                      dim = c(10, 5),
                      dimnames = list(age = 0:9, region = 1:5)))
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - rowMeans(x)
    ans.expected <- ans.expected - rep(colMeans(ans.expected), each = 10)
    expect_equal(ans.obtained, ans.expected)
    ## dim = 10
    x <- Values(array(rnorm(50),
                      dim = 50,
                      dimnames = list(time = 1:50)),
                dimscales = c(time = "Intervals"))
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - mean(x)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(10, 10, 5)
    x <- Values(array(rnorm(500),
                      dim = c(10, 10, 5),
                      dimnames = list(age = 0:9, time = 1:10, region = 1:5)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    ans.obtained <- sweepAllMargins(x)
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), 1, mean)),
                 rep(0, 10))
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), 1:2, mean)),
                 rep(0, 100))
    expect_identical(ans.obtained@metadata, x@metadata)
})

test_that("sweepAllMargins works with Values - has iteration dimension", {
    sweepAllMargins <- demest:::sweepAllMargins
    ## dim = c(10, 5)
    x <- Values(array(rnorm(50),
                      dim = c(10, 5),
                      dimnames = list(iteration = 1:10, region = 1:5)))
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - rowMeans(x)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(5, 10)
    x <- Values(array(rnorm(50),
                      dim = c(5, 10),
                      dimnames = list(region = 1:5, iteration = 1:10)))
    ans.obtained <- sweepAllMargins(x)
    ans.expected <- x - rep(colMeans(x), each = 5)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(10, 10, 5)
    x <- Values(array(rnorm(500),
                      dim = c(10, 10, 5),
                      dimnames = list(age = 0:9, time = 1:10, iteration = 1:5)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    ans.obtained <- sweepAllMargins(x)
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), 3, mean)),
                 rep(0, 5))
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), c(1, 3), mean)),
                 rep(0, 50))
    expect_identical(ans.obtained@metadata, x@metadata)
    ## dim = c(10, 10, 5, 10)
    x <- Values(array(rnorm(5000),
                      dim = c(10, 10, 5, 10),
                      dimnames = list(age = 0:9, time = 1:10, region = 1:5,
                          iteration = 1:10)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    ans.obtained <- sweepAllMargins(x)
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), 4, mean)),
                 rep(0, 10))
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), c(1, 4), mean)),
                 rep(0, 100))
    expect_equal(as.numeric(apply(as(ans.obtained, "array"), c(1, 2, 4), mean)),
                 rep(0, 1000))
    expect_identical(ans.obtained@metadata, x@metadata)
    ## dim = 50
    x <- Values(array(rnorm(50),
                      dim = 50,
                      dimnames = list(iteration = 1:50)))
    expect_identical(sweepAllMargins(x), x)
})

test_that("subtractAlpha0 works", {
    subtractAlpha0 <- demest:::subtractAlpha0
    ## dim = c(10, 5)
    x <- Values(array(rnorm(50),
                      dim = c(10, 5),
                      dimnames = list(age = 0:9, region = 1:5)),
                dimscales = c(age = "Intervals"))
    ans.obtained <- subtractAlpha0(x, iAlong = 1L)
    ans.expected <- x - rep(colMeans(x), each = 10)
    expect_equal(ans.obtained, ans.expected)
    ## dim = 10
    x <- Values(array(rnorm(50),
                      dim = 50,
                      dimnames = list(time = 1:50)),
                dimscales = c(time = "Intervals"))
    ans.obtained <- subtractAlpha0(x, iAlong = 1L)
    ans.expected <- x - mean(x)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(10, 10, 5)
    x <- Values(array(rnorm(500),
                      dim = c(10, 10, 5),
                      dimnames = list(iteration = 1:10, time = 1:10, region = 1:5)),
                dimscales = c(time = "Intervals"))
    ans.obtained <- subtractAlpha0(x, iAlong = 2L)
    means <- apply(ans.obtained, c(1, 3), mean)
    expect_equal(as.numeric(means), rep(0, times = length(means)))
})

