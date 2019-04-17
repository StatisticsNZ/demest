
context("Prior-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

## betaIsEstimated ####################################################################

test_that("betaIsEstimated works in default case", {
    betaIsEstimated <- demest:::betaIsEstimated
    x <- new("ExchFixed",
             isSaturated = new("LogicalFlag", FALSE),
             allStrucZero = FALSE)
    expect_false(betaIsEstimated(x))
})

test_that("betaIsEstimated works with Zero prior", {
    betaIsEstimated <- demest:::betaIsEstimated
    x <- new("Zero",
             isSaturated = new("LogicalFlag", FALSE),
             J = new("Length", 2L),
             allStrucZero = c(FALSE, FALSE))
    expect_false(betaIsEstimated(x))
})

## describePrior #############################################################

test_that("describePrior works with exchangeable", {
    describePrior <- demest:::describePrior
    x <- new("ExchFixed")
    expect_identical(describePrior(x),
                     "Exchangeable with known variance")
    x <- new("ExchNormZero")
    expect_identical(describePrior(x),
                     "Exchangeable")
    x <- new("ExchNormCov")
    expect_identical(describePrior(x),
                     "Exchangeable with covariates")
    x <- new("ExchRobustZero")
    expect_identical(describePrior(x),
                     "Robust exchangeable")
    x <- new("ExchRobustCov")
    expect_identical(describePrior(x),
                     "Robust exchangeable with covariates")
})

test_that("describePrior works with damped DLM", {
    describePrior <- demest:::describePrior
    x <- new("DLMNoTrendNormZeroNoSeason")
    expect_identical(describePrior(x),
                     "Damped local level")
    x <- new("DLMWithTrendNormZeroNoSeason")
    expect_identical(describePrior(x),
                     "Damped local trend")
    x <- new("DLMNoTrendNormZeroWithSeason")
    expect_identical(describePrior(x),
                     "Damped local level with seasonal effect")
    x <- new("DLMWithTrendNormZeroWithSeason")
    expect_identical(describePrior(x),
                     "Damped local trend with seasonal effect")
    x <- new("DLMNoTrendNormCovNoSeason")
    expect_identical(describePrior(x),
                     "Damped local level with covariates")
    x <- new("DLMWithTrendNormCovNoSeason")
    expect_identical(describePrior(x),
                     "Damped local trend with covariates")
    x <- new("DLMNoTrendNormCovWithSeason")
    expect_identical(describePrior(x),
                     "Damped local level with covariates and seasonal effect")
    x <- new("DLMWithTrendNormCovWithSeason")
    expect_identical(describePrior(x),
                     "Damped local trend with covariates and seasonal effect")
    x <- new("DLMNoTrendRobustZeroNoSeason")
    expect_identical(describePrior(x),
                     "Damped robust local level")
    x <- new("DLMWithTrendRobustZeroNoSeason")
    expect_identical(describePrior(x),
                     "Damped robust local trend")
    x <- new("DLMNoTrendRobustZeroWithSeason")
    expect_identical(describePrior(x),
                     "Damped robust local level with seasonal effect")
    x <- new("DLMWithTrendRobustZeroWithSeason")
    expect_identical(describePrior(x),
                     "Damped robust local trend with seasonal effect")
    x <- new("DLMNoTrendRobustCovNoSeason")
    expect_identical(describePrior(x),
                     "Damped robust local level with covariates")
    x <- new("DLMWithTrendRobustCovNoSeason")
    expect_identical(describePrior(x),
                     "Damped robust local trend with covariates")
    x <- new("DLMNoTrendRobustCovWithSeason")
    expect_identical(describePrior(x),
                     "Damped robust local level with covariates and seasonal effect")
    x <- new("DLMWithTrendRobustCovWithSeason")
    expect_identical(describePrior(x),
                     "Damped robust local trend with covariates and seasonal effect")
})

test_that("describePrior works with non-damped DLM", {
    describePrior <- demest:::describePrior
    x <- new("DLMNoTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local level")
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local trend")
    x <- new("DLMNoTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local level with seasonal effect")
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local trend with seasonal effect")
    x <- new("DLMNoTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local level with covariates")
    x <- new("DLMWithTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local trend with covariates")
    x <- new("DLMNoTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local level with covariates and seasonal effect")
    x <- new("DLMWithTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Local trend with covariates and seasonal effect")
    x <- new("DLMNoTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local level")
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local trend")
    x <- new("DLMNoTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local level with seasonal effect")
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local trend with seasonal effect")
    x <- new("DLMNoTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local level with covariates")
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local trend with covariates")
    x <- new("DLMNoTrendRobustCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local level with covariates and seasonal effect")
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@phi <- 1
    expect_identical(describePrior(x),
                     "Robust local trend with covariates and seasonal effect")
})

test_that("describePrior works with remaining priors", {
    describePrior <- demest:::describePrior
    x <- new("KnownCertain")
    expect_identical(describePrior(x),
                     "Known values")
    x <- new("KnownUncertain")
    expect_identical(describePrior(x),
                     "Normal with known mean and variance")
    x <- new("MixNormZero")
    expect_identical(describePrior(x),
                     "Mixture model")
    x <- new("Zero")
    expect_identical(describePrior(x),
                     "Set to zero")
})


## drawPrior ####################################################################

## ExchFixed

test_that("drawPrior works with ExchFixed", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    ans.obtained <- drawPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of drawPrior give same answer with ExchFixed", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed(sd = 1)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    ans.R <- drawPrior(prior, useC = FALSE)
    ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)

})

## Exch

test_that("drawPrior works with ExchNormZero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected@tau@.Data <- rhalft(n = 1,
                                         df = prior@nuTau@.Data,
                                         scale = prior@ATau@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with ExchNormZero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with ExchRobustZero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    spec <- Exch(error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected@tau@.Data <- rhalft(n = 1,
                                         df = prior@nuTau@.Data,
                                         scale = prior@ATau@.Data)
        for (i in seq_len(ans.expected@J@.Data))
            ans.expected@UBeta@.Data[i] <- rinvchisq1(df = ans.expected@nuBeta@.Data,
                                                      scaleSq = ans.expected@tau@.Data^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with ExchRobustZero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with ExchNormCov", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    rhalftTrunc1 <- demest:::rhalftTrunc1
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormCov")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected@tau@.Data <- rhalftTrunc1(df = ans.expected@nuTau@.Data,
                                               scale = ans.expected@ATau@.Data,
                                               max = ans.expected@tauMax@.Data)
        for (i in seq_len(ans.expected@P@.Data - 1))
            ans.expected@UEtaCoef@.Data[i] <- rinvchisq1(df = ans.expected@nuEtaCoef@.Data[i],
                                                         scaleSq = ans.expected@AEtaCoef@.Data[i]^2)
        ans.expected@eta@.Data[1L] <- 0
        for (i in seq_len(ans.expected@P@.Data - 1))
            ans.expected@eta@.Data[i+1] <- rnorm(n = 1,
                                                 mean = ans.expected@meanEtaCoef@.Data[i],
                                                 sd = sqrt(ans.expected@UEtaCoef@.Data[i]))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with ExchNormCov", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = list(cat = diag(3)),
                                         coef = TDist(scale = 0.3)),
                 error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormCov")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with ExchRobustCov", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    rhalftTrunc1 <- demest:::rhalftTrunc1
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = list(cat = diag(3)),
                                         coef = TDist(scale = 0.3)),
                 error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustCov")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected@tau@.Data <- rhalftTrunc1(df = prior@nuTau@.Data,
                                               scale = prior@ATau@.Data,
                                               max = prior@tauMax@.Data)
        for (i in seq_len(ans.expected@J@.Data))
            ans.expected@UBeta@.Data[i] <- rinvchisq1(df = ans.expected@nuBeta@.Data,
                                                      scaleSq = ans.expected@tau@.Data^2)
        for (i in seq_len(ans.expected@P@.Data - 1))
            ans.expected@UEtaCoef@.Data[i] <- rinvchisq1(df = ans.expected@nuEtaCoef@.Data[i],
                                                         scaleSq = ans.expected@AEtaCoef@.Data[i]^2)
        ans.expected@eta@.Data[1] <- 0
        for (i in seq_len(ans.expected@P@.Data - 1))
            ans.expected@eta@.Data[i+1] <- rnorm(n = 1,
                                                 mean = ans.expected@meanEtaCoef@.Data[i],
                                                 sd = sqrt(ans.expected@UEtaCoef@.Data[i]))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with ExchRobustCov", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = list(cat = diag(3)),
                                         coef = TDist(scale = 0.3)),
                 error = Error(robust = TRUE,
                               scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustCov")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

## DLM - Norm, Zero

test_that("drawPrior works with DLMNoTrendNormZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawPhi <- demest:::drawPhi
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendNormZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendNormZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawPhi <- demest:::drawPhi
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendNormZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMNoTrendNormZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaSeason <- demest:::drawOmegaSeason
    drawPhi <- demest:::drawPhi
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendNormZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendNormZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawOmegaSeason <- demest:::drawOmegaSeason
    drawPhi <- demest:::drawPhi
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendNormZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

## DLM - Norm, Cov

test_that("drawPrior works with DLMNoTrendNormCovNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 3, 4)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendNormCovNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendNormCovNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendNormCovNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMNoTrendNormCovWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaSeason <- demest:::drawOmegaSeason
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictSeason <- demest:::predictSeason
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendNormCovWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendNormCovWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawOmegaSeason <- demest:::drawOmegaSeason
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictSeason <- demest:::predictSeason
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.01)),
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendNormCovWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.01)),
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

## DLM - Robust, Zero

test_that("drawPrior works with DLMNoTrendRobustZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendRobustZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendRobustZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendRobustZeroNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMNoTrendRobustZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaSeason <- demest:::drawOmegaSeason
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendRobustZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendRobustZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawOmegaSeason <- demest:::drawOmegaSeason
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendRobustZeroWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

## DLM - Robust, Cov

test_that("drawPrior works with DLMNoTrendRobustCovNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendRobustCovNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendRobustCovNoSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendRobustCovNoSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.1)),
                damp = Damp(shape1 = 3, shape2 = 3),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMNoTrendRobustCovWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaSeason <- demest:::drawOmegaSeason
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictSeason <- demest:::predictSeason
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- predictAlphaDLMNoTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMNoTrendRobustCovWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("drawPrior works with DLMWithTrendRobustCovWithSeason", {
    drawPrior <- demest:::drawPrior
    drawTau <- demest:::drawTau
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    drawOmegaDelta <- demest:::drawOmegaDelta
    drawOmegaSeason <- demest:::drawOmegaSeason
    predictUBeta <- demest:::predictUBeta
    drawPhi <- demest:::drawPhi
    drawUEtaCoef <- demest:::drawUEtaCoef
    drawEta <- demest:::drawEta
    predictSeason <- demest:::predictSeason
    drawDelta0 <- demest:::drawDelta0
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.01)),
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPrior(prior)
        set.seed(seed)
        ans.expected <- prior
        ans.expected <- drawTau(ans.expected)
        ans.expected <- drawOmegaAlpha(ans.expected)
        ans.expected <- drawOmegaDelta(ans.expected)
        ans.expected <- drawOmegaSeason(ans.expected)
        ans.expected <- predictUBeta(ans.expected)
        ans.expected <- drawPhi(ans.expected)
        ans.expected <- drawUEtaCoef(ans.expected)
        ans.expected <- drawEta(ans.expected)
        ans.expected <- predictSeason(ans.expected)
        ans.expected <- drawDelta0(ans.expected)
        ans.expected <- predictAlphaDeltaDLMWithTrend(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPrior give same answer with DLMWithTrendRobustCovWithSeason", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(3, 4, 3)))
    formula <- mean ~ income * cat
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.1), scale = HalfT(scale = 0.01)),
                damp = Damp(shape1 = 3, shape2 = 3),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                covariates = Covariates(formula = formula,
                                        data = data,
                                        contrastsArg = list(cat = diag(3)),
                                        coef = TDist(scale = 0.3)),
                error = Error(robust = TRUE, scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPrior(prior, useC = FALSE)
        set.seed(seed)
        ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

## Known

test_that("drawPrior works with KnownCertain", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.obtained <- drawPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of drawPrior give same answer with KnownCertain", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.R <- drawPrior(prior, useC = FALSE)
    ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("drawPrior works with KnownUncertain", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.obtained <- drawPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of drawPrior give same answer with KnownUncertain", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.R <- drawPrior(prior, useC = FALSE)
    ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

## Zero

test_that("drawPrior works with Zero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.obtained <- drawPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of drawPrior give same answer with Zero", {
    drawPrior <- demest:::drawPrior
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.R <- drawPrior(prior, useC = FALSE)
    ans.C.generic <- drawPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- drawPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})


## makeOutputPrior ###################################################################

test_that("makeOutputPrior works with ExchFixed", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(1)
    metadata <- NULL
    sY <- NULL
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = sY,
                          isSaturated = FALSE,
                          margin = 0L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior)
    ans.expected <- list(scaleError = prior@tau@.Data)
    expect_identical(ans.obtained, ans.expected)
})


## Exch

test_that("makeOutputPrior works with ExchNormZero", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(scaleError = Skeleton(first = 3L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with ExchNormCov", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat, data = data))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(coef = new("SkeletonCovariates",
                                    first = 3L,
                                    last = 5L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 6L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with ExchRobustZero", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 3L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with ExchRobustCov", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat, data = data),
                 error = Error(robust = TRUE))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(coef = new("SkeletonCovariates",
                             first = 3L,
                             last = 5L,
                             metadata = metadata.coef),
                           meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 6L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

## DLM - Norm, Zero

test_that("makeOutputPrior works with DLMNoTrendNormZeroNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         scaleError = Skeleton(first = 16L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendNormZeroNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     indices0 = 1L,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     iAlong = 1L,
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     indices0 = 1L,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     iAlong = 1L,
                                     first = 3L,
                                     last = 13L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     indices0 = 1L,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     iAlong = 1L,
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     indices0 = 1L,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMNoTrendNormZeroWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- DLM(trend = NULL,
                season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                         nms = "season",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                         nms = c("season", "time"),
                         dimtypes = c("state", "state"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     iAlong = 1L,
                                     first = 3L,
                                     last = 13L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     indices0 = 1L,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonStateDLM",
                                      iAlong = 1L,
                                      first = 16L,
                                      last = 59L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      metadata = metadata,
                                      indices0 = 1:4,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 60L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         scaleError = Skeleton(first = 61L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendNormZeroWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                         nms = "season",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                         nms = c("season", "time"),
                         dimtypes = c("state", "state"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indices0 = 1:4,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         scaleError = Skeleton(first = 73L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL, season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                         nms = "season",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                         nms = c("season", "time"),
                         dimtypes = c("state", "state"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indices0 = 1:4,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         scaleError = Skeleton(first = 73L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

## DLM - Norm, Cov

test_that("makeOutputPrior works with DLMNoTrendNormCovNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income + cat, data = data))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         coef = new("SkeletonCovariates",
                                    first = 16L,
                                    last = 18L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 19L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendNormCovNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(covariates = Covariates(mean ~ income + cat, data = data))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 31L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(level = NULL,
                covariates = Covariates(mean ~ income + cat, data = data))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indices0 = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef), 
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 31L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMNoTrendNormCovWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                         nms = "season",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                         nms = c("season", "time"),
                         dimtypes = c("state", "state"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonStateDLM",
                                      first = 16L,
                                      last = 59L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                     metadata = metadata,
                                     metadata0 = metadata0.season,
                                     metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 60L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 61L,
                                    last = 63L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 64L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendNormCovWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 76L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(level = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         scaleError = Skeleton(first = 76L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

## DLM - Robust, Zero

test_that("makeOutputPrior works with DLMNoTrendRobustZeroNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- DLM(trend = NULL,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 16L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendRobustZeroNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    spec <- DLM(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 28L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 28L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMNoTrendRobustZeroWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonStateDLM",
                                      first = 16L,
                                      last = 59L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 60L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 61L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendRobustZeroWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    spec <- DLM(season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 73L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL,
                season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 73L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})


## DLM - Robust, Cov

test_that("makeOutputPrior works with DLMNoTrendRobustCovNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         coef = new("SkeletonCovariates",
                                    first = 16L,
                                    last = 18L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 19L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendRobustCovNoSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(covariates = Covariates(mean ~ income + cat, data = data),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 31L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(level = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 31L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMNoTrendRobustCovWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonStateDLM",
                                      first = 16L,
                                      last = 59L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 60L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 61L,
                                    last = 63L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 64L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with DLMWithTrendRobustCovWithSeason", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    ## has level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 76L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## no level
    data <- data.frame(time = 1:10,
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- DLM(level = NULL,
                covariates = Covariates(mean ~ income + cat, data = data),
                season = Season(n = 4),
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    metadata0.season <- new("MetaData",
                            nms = "season",
                            dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0.season <- new("MetaData",
                                nms = c("season", "time"),
                                dimtypes = c("state", "state"),
                                DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                                 new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- list(level = new("SkeletonStateDLM",
                                     first = 3L,
                                     last = 13L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleLevel = Skeleton(first = 14L),
                         dfScaleLevel = prior@nuAlpha@.Data,
                         scaleScaleLevel = prior@AAlpha@.Data,
                         trend = new("SkeletonStateDLM",
                                     first = 15L,
                                     last = 25L,
                                     iAlong = 1L,
                                     indicesShow = 2:11,
                                     indices0 = 1L,
                                     metadata = metadata,
                                     metadata0 = NULL,
                                     metadataIncl0 = metadataIncl0),
                         scaleTrend = Skeleton(first = 26L),
                         dfScaleTrend = prior@nuDelta@.Data,
                         scaleScaleTrend = prior@ADelta@.Data,
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonStateDLM",
                                      first = 28L,
                                      last = 71L,
                                      iAlong = 1L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      indices0 = 1:4,
                                      metadata = metadata,
                                      metadata0 = metadata0.season,
                                      metadataIncl0 = metadataIncl0.season),
                         scaleSeason = Skeleton(first = 72L),
                         dfScaleSeason = prior@nuSeason@.Data,
                         scaleScaleSeason = prior@ASeason@.Data,
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         meanCoef = prior@meanEtaCoef@.Data,
                         dfCoef = prior@nuEtaCoef@.Data,
                         scaleCoef = prior@AEtaCoef@.Data,
                         dfError = prior@nuBeta@.Data,
                         scaleError = Skeleton(first = 76L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with KnownCertain", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:4])))
    mean <- ValuesOne(1:6, labels = letters[1:6], name = "region")
    spec <- Known(mean = mean)
    strucZeroArray <- Counts(array(1L,
                                   dim = 4,
                                   dimnames = list(region = letters[1:4])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 101L)
    ans.expected <- list(mean = ValuesOne(as.double(1:4), labels = letters[1:4], name = "region"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with KnownUncertain", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:4])))
    mean <- ValuesOne(1:6, labels = letters[1:6], name = "region")
    sd <- ValuesOne(1:6, labels = letters[1:6], name = "region")
    spec <- Known(mean = mean, sd = sd)
    strucZeroArray <- Counts(array(1L,
                                   dim = 4,
                                   dimnames = list(region = letters[1:4])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 101L)
    ans.expected <- list(mean = ValuesOne(as.double(1:4), labels = letters[1:4], name = "region"),
                         sd = ValuesOne(as.double(1:4), labels = letters[1:4], name = "region"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with MixNormZero", {
    makeOutputPrior <- demest:::makeOutputPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix(weights = Weights(mean = -10))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 101L)
    metadata.vec <- new("MetaData",
                        nms = c("component", "reg", "age"),
                        dimtypes = c("state", "state", "age"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:10)),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    metadata.wt <- new("MetaData",
                       nms = c("time", "component"),
                       dimtypes = c("time", "state"),
                       DimScales = list(new("Points", dimvalues = 2001:2010),
                                        new("Categories", dimvalues = as.character(1:10))))
    ans.expected <- list(components = Skeleton(metadata = metadata.vec,
                                               first = 101L),
                         scaleComponents = Skeleton(first = 301L),
                         weights = Skeleton(metadata = metadata.wt,
                                            first = 302L),
                         level1 = Skeleton(metadata = metadata.wt,
                                             first = 402L),
                         scale1 = Skeleton(first = 502L),
                         level2 = Skeleton(metadata = metadata.wt,
                                             first = 503L),
                         mean = Skeleton(first = 603L),
                         damp = Skeleton(first = 604L),
                         scale2 = Skeleton(first = 605L),
                         scaleError = Skeleton(first = 606L),
                         dfScaleError = prior@nuTau@.Data,
                         scaleScaleError = prior@ATau@.Data)
    expect_identical(ans.obtained, ans.expected)
})

          
## predictPrior ######################################################################


## ExchFixed

test_that("predictPrior works with ExchFixed", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchFixed", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})


## Exch

test_that("predictPrior works with ExchNormZero", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchNormZero", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with ExchRobustZero", {
    predictPrior <- demest:::predictPrior
    predictUBeta <- demest:::predictUBeta
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    set.seed(1)
    ans.obtained <- predictPrior(prior)
    set.seed(1)
    ans.expected <- predictUBeta(prior)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchRobustZero", {
    predictPrior <- demest:::predictPrior
    predictUBeta <- demest:::predictUBeta
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    set.seed(1)
    ans.R <- predictPrior(prior, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with ExchNormCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchNormCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with ExchRobustCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    predictUBeta <- demest:::predictUBeta
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictPrior(prior)
    set.seed(1)
    ans.expected <- predictUBeta(prior)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchRobustCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.R <- predictPrior(prior, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})


## DLM - Norm, Zero

test_that("predictPrior works with DLMNoTrendNormZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     margin = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendNormZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendNormZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    spec <- DLM()
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendNormZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    spec <- DLM()
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMNoTrendNormZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    spec <- DLM(trend = NULL, season = Season(n = 2))
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendNormZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    spec <- DLM(trend = NULL, season = Season(n = 2))
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendNormZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    spec <- DLM(season = Season(n = 2))
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendNormZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    spec <- DLM(season = Season(n = 2))
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})


## DLM - Norm, Cov

test_that("predictPrior works with DLMNoTrendNormCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendNormCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendNormCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendNormCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMNoTrendNormCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = season,
                covariates = covariates)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendNormCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = season,
                covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendNormCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = season,
                covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendNormCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = season,
                covariates = covariates)
    beta <- rnorm(10)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})


## DLM - Robust, Zero

test_that("predictPrior works with DLMNoTrendRobustZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictUBeta <- demest:::predictUBeta
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL, error = error)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendRobustZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL, error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points")) 
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendRobustZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictUBeta <- demest:::predictUBeta
    error <- Error(robust = TRUE)
    spec <- DLM(error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendRobustZeroNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    error <- Error(robust = TRUE)
    spec <- DLM(error = error)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMNoTrendRobustZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    predictUBeta <- demest:::predictUBeta
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL, season = Season(n = 2), error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendRobustZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL, season = Season(n = 2), error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendRobustZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    predictUBeta <- demest:::predictUBeta
    error <- Error(robust = TRUE)
    spec <- DLM(season = Season(n = 2), error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendRobustZeroWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    error <- Error(robust = TRUE)
    spec <- DLM(season = Season(n = 2), error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})


## DLM - Robust, Cov

test_that("predictPrior works with DLMNoTrendRobustCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictUBeta <- demest:::predictUBeta
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendRobustCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendRobustCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictUBeta <- demest:::predictUBeta
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(covariates = covariates,
                error = error)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovNoSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendRobustCovNoSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(covariates = covariates,
                error = error)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovNoSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMNoTrendRobustCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    predictSeason <- demest:::predictSeason
    predictUBeta <- demest:::predictUBeta
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL,
                season = season,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDLMNoTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMNoTrendRobustCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(trend = NULL,
                season = season,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with DLMWithTrendRobustCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    predictSeason <- demest:::predictSeason
    predictUBeta <- demest:::predictUBeta
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(season = season,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovWithSeasonPredict")
    set.seed(1)
    ans.obtained <- predictPrior(prior.new)
    set.seed(1)
    ans.expected <- predictAlphaDeltaDLMWithTrend(prior.new)
    ans.expected <- predictSeason(ans.expected)
    ans.expected <- predictUBeta(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with DLMWithTrendRobustCovWithSeason", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    season <- Season(n = 2)
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    error <- Error(robust = TRUE)
    spec <- DLM(season = season,
                covariates = covariates,
                error = error)
    beta <- rnorm(10)
    beta <- rnorm(10)
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 11:15)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovWithSeasonPredict")
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with KnownCertain", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with KnownCertain", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with KnownUncertain", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with KnownUncertain", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with MixNormZero", {
    predictPrior <- demest:::predictPrior
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix(weights = Weights())
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1,
                              isSaturated = FALSE,
                              margin = 1:3,
                              strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = list(time = 2011:2030,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L,
                                     margin = 1:3,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "MixNormZeroPredict")
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    ans.obtained <- predictPrior(prior.new)
    expect_true(all(ans.obtained@levelComponentWeightMix@.Data != prior.new@levelComponentWeightMix@.Data))
    expect_true(all(ans.obtained@componentWeightMix@.Data != prior.new@componentWeightMix@.Data))
    expect_true(all(ans.obtained@weightMix@.Data != prior.new@weightMix@.Data))
    expect_true(!all(ans.obtained@indexClassMix@.Data == prior.new@indexClassMix@.Data))
    expect_true(all(ans.obtained@alphaMix@.Data != prior.new@alphaMix@.Data))
})

test_that("R and C versions of predictPrior give same answer MixNormZero", {
    predictPrior <- demest:::predictPrior
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix()
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1,
                              isSaturated = FALSE,
                              margin = 1:3,
                              strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = list(time = 2011:2030,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L,
                                     margin = 1:3,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "MixNormZeroPredict")
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    set.seed(1)
    ans.R <- predictPrior(prior.new, useC = FALSE)
    set.seed(1)
    ans.C.generic <- predictPrior(prior.new, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- predictPrior(prior.new, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_equal(ans.C.generic, ans.C.specific)
})

test_that("predictPrior works with Zero", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with Zero", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})


## rescalePairPriors ##################################################################

test_that("rescalePairPriors works with Exchangeable-Exchangeable", {
    rescalePairPriors <- demest:::rescalePairPriors
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm    
    spec.high <- Exch()
    spec.low <- Exch()
    beta.high <- rnorm(10)
    beta.low <- rnorm(2)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(country = letters[1:5],
                                                   sex = c("F", "M"))))
    metadata.high <- new("MetaData",
                         nms = c("country", "sex"),
                         dimtypes = c("state", "sex"),
                         DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                          new("Sexes", dimvalues = c("F", "M"))))
    metadata.low <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("F", "M"))))
    prior.high <- initialPrior(spec.high,
                               beta = beta.high,
                               metadata = metadata.high,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.low <- initialPrior(spec.low,
                              beta = beta.low,
                              metadata = metadata.low,
                              sY = NULL,
                              isSaturated = FALSE, margin = 2L, strucZeroArray = strucZeroArray)
    skeleton.beta.high <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.high)
    skeleton.beta.low <- SkeletonBetaTerm(first = 30L,
                                          metadata = metadata.low)
    skeletons.prior.high <- makeOutputPrior(prior = prior.high,
                                            metadata = metadata.high,
                                            pos = 50L)
    skeletons.prior.low <- makeOutputPrior(prior = prior.low,
                                           metadata = metadata.low,
                                           pos = 100L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePairPriors(priorHigh = prior.high,
                      priorLow = prior.low,
                      skeletonBetaHigh = skeleton.beta.high,
                      skeletonBetaLow = skeleton.beta.low,
                      skeletonsPriorHigh = skeletons.prior.high,
                      skeletonsPriorLow = skeletons.prior.low,
                      adjustments = adjustments,
                      prefixAdjustments = prefix.adjustments,
                      filename = filename,
                      nIteration = nIteration,
                      lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.high <- seq(from = skeleton.beta.high@first, to = skeleton.beta.high@last)
    here.low <- seq(from = skeleton.beta.low@first, to = skeleton.beta.low@last)
    data.high <- data[here.high, ]
    data.low <- data[here.low, ]
    output.high <- output[here.high, ]
    output.low <- output[here.low, ]
    means <- array(data.high, dim = c(5, 2, 20))
    means <- apply(means, 2:3, mean)
    expect_equal(as.numeric(adjustments[["model.prior.country:sex"]]), -as.numeric(means))
    expect_equal(as.numeric(adjustments[["model.prior.sex"]]), as.numeric(means))
    expect_equal(output.high, data.high - rep(means, each = 5))
    expect_equal(output.low, data.low + means)
})


test_that("rescalePairPriors works with Exchangeable-DLM", {
    rescalePairPriors <- demest:::rescalePairPriors
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm    
    spec.high <- Exch()
    spec.low <- DLM(damp = NULL)
    beta.high <- rnorm(10)
    beta.low <- rnorm(5)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(time = 2001:2005,
                                                   sex = c("F", "M"))),
                             dimscales = c(time = "Points"))
    metadata.high <- new("MetaData",
                         nms = c("time", "sex"),
                         dimtypes = c("time", "sex"),
                         DimScales = list(new("Points", dimvalues = 2001:2005),
                                          new("Sexes", dimvalues = c("F", "M"))))
    metadata.low <- new("MetaData",
                         nms = "time",
                         dimtypes = "time",
                         DimScales = list(new("Points", dimvalues = 2001:2005)))
    prior.high <- initialPrior(spec.high,
                               beta = beta.high,
                               metadata = metadata.high,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.low <- initialPrior(spec.low,
                              beta = beta.low,
                              metadata = metadata.low,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    skeleton.beta.high <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.high)
    skeleton.beta.low <- SkeletonBetaTerm(first = 30L,
                                          metadata = metadata.low)
    skeletons.prior.high <- makeOutputPrior(prior = prior.high,
                                            metadata = metadata.high,
                                            pos = 50L)
    skeletons.prior.low <- makeOutputPrior(prior = prior.low,
                                           metadata = metadata.low,
                                           pos = 100L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 200L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:4000)
    writeBin(data, con = con)
    close(con)
    rescalePairPriors(priorHigh = prior.high,
                      priorLow = prior.low,
                      skeletonBetaHigh = skeleton.beta.high,
                      skeletonBetaLow = skeleton.beta.low,
                      skeletonsPriorHigh = skeletons.prior.high,
                      skeletonsPriorLow = skeletons.prior.low,
                      adjustments = adjustments,
                      prefixAdjustments = prefix.adjustments,
                      filename = filename,
                      nIteration = nIteration,
                      lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 4000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.beta.high <- seq(from = skeleton.beta.high@first, to = skeleton.beta.high@last)
    here.beta.low <- seq(from = skeleton.beta.low@first, to = skeleton.beta.low@last)
    here.level.low <- seq(from = skeletons.prior.low$level@first,
                          to = skeletons.prior.low$level@last)
    data.beta.high <- data[here.beta.high, ]
    data.beta.low <- data[here.beta.low, ]
    data.level.low <- data[here.level.low, ]
    output.beta.high <- output[here.beta.high, ]
    output.beta.low <- output[here.beta.low, ]
    output.level.low <- output[here.level.low, ]
    means <- array(data.beta.high, dim = c(5, 2, 20))
    means <- apply(means, 3, mean)
    expect_equal(as.numeric(adjustments[["model.prior.time:sex"]]), -as.numeric(means))
    expect_equal(output.beta.high, data.beta.high - rep(means, each = 10))
    expect_equal(output.beta.low, data.beta.low + rep(means, each = 5))
    expect_equal(output.level.low, data.level.low + rep(means, each = 6))
})

test_that("rescalePairPriors works with DLM-Exchangeable", {
    rescalePairPriors <- demest:::rescalePairPriors
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm    
    spec.high <- DLM(damp = NULL)
    spec.low <- Exch()
    beta.high <- rnorm(10)
    beta.low <- rnorm(2)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(time = 2001:2005,
                                                   sex = c("F", "M"))),
                             dimscales = c(time = "Points"))
    metadata.high <- new("MetaData",
                         nms = c("time", "sex"),
                         dimtypes = c("time", "sex"),
                         DimScales = list(new("Points", dimvalues = 2001:2005),
                                          new("Sexes", dimvalues = c("F", "M"))))
    metadata.low <- new("MetaData",
                         nms = "sex",
                         dimtypes = "sex",
                         DimScales = list(new("Sexes", dimvalues = c("F", "M"))))
    prior.high <- initialPrior(spec.high,
                               beta = beta.high,
                               metadata = metadata.high,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.low <- initialPrior(spec.low,
                              beta = beta.low,
                              metadata = metadata.low,
                              sY = NULL,
                              isSaturated = FALSE, margin = 2L, strucZeroArray = strucZeroArray)
    skeleton.beta.high <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.high)
    skeleton.beta.low <- SkeletonBetaTerm(first = 30L,
                                          metadata = metadata.low)
    skeletons.prior.high <- makeOutputPrior(prior = prior.high,
                                            metadata = metadata.high,
                                            pos = 50L)
    skeletons.prior.low <- makeOutputPrior(prior = prior.low,
                                           metadata = metadata.low,
                                           pos = 100L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 200L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:4000)
    writeBin(data, con = con)
    close(con)
    rescalePairPriors(priorHigh = prior.high,
                      priorLow = prior.low,
                      skeletonBetaHigh = skeleton.beta.high,
                      skeletonBetaLow = skeleton.beta.low,
                      skeletonsPriorHigh = skeletons.prior.high,
                      skeletonsPriorLow = skeletons.prior.low,
                      adjustments = adjustments,
                      prefixAdjustments = prefix.adjustments,
                      filename = filename,
                      nIteration = nIteration,
                      lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 4000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.beta.high <- seq(from = skeleton.beta.high@first, to = skeleton.beta.high@last)
    here.beta.low <- seq(from = skeleton.beta.low@first, to = skeleton.beta.low@last)
    here.level.high <- seq(from = skeletons.prior.high$level@first,
                          to = skeletons.prior.high$level@last)
    data.beta.high <- data[here.beta.high, ]
    data.beta.low <- data[here.beta.low, ]
    data.level.high <- data[here.level.high, ]
    data.level.0.high <- data.level.high[skeletons.prior.high$level@indices0,]
    output.beta.high <- output[here.beta.high, ]
    output.beta.low <- output[here.beta.low, ]
    output.level.high <- output[here.level.high, ]
    means <- array(data.level.0.high, dim = c(2, 20))
    expect_equal(as.numeric(adjustments[["model.prior.sex"]]), as.numeric(means))
    expect_equal(output.beta.high, data.beta.high - rep(means, each = 5))
    expect_equal(output.beta.low, data.beta.low + means)
    expect_equal(output.level.high, data.level.high - rep(means, each = 6))
})



## rescalePred ###################################################################


test_that("rescalePred works with Zero", {
    rescalePred <- demest:::rescalePred
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    spec <- Zero()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(region = letters[1:5],
                                                   sex = c("F", "M"))))
    metadata <- new("MetaData",
                    nms = c("country", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                     new("Sexes", dimvalues = c("F", "M"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    skeleton <- SkeletonBetaTerm(first = 10L,
                                 metadata = metadata)
    nIteration <- 20L
    lengthIter <- 100L
    adjustment <- NULL
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePred(prior = prior,
                skeleton = skeleton,
                adjustment = adjustment,
                filename = filename,
                nIteration = 20L,
                lengthIter = 100L)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    ans.obtained <- output[10:19, ]
    ans.expected <- data[10:19,]
    expect_equal(ans.obtained, ans.expected)
})

test_that("rescalePred works with Exchangeable", {
    rescalePred <- demest:::rescalePred
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(region = letters[1:5],
                                                   sex = c("F", "M"))))
    metadata <- new("MetaData",
                    nms = c("country", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                     new("Sexes", dimvalues = c("F", "M"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    skeleton <- SkeletonBetaTerm(first = 10L,
                                 metadata = metadata)
    nIteration <- 20L
    lengthIter <- 100L
    adjustment <- Values(array(rnorm(n = 10 * 20),
                               dim = c(5, 2, 20),
                               dimnames = list(country = letters[1:5],
                                               sex = c("F", "M"),
                                               iteration = 1:20)))
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePred(prior = prior,
                skeleton = skeleton,
                adjustment = adjustment,
                filename = filename,
                nIteration = 20L,
                lengthIter = 100L)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    ans.obtained <- output[10:19, ]
    ans.expected <- data[10:19,] + as.numeric(adjustment)
    expect_equal(ans.obtained, ans.expected)
})


## rescalePriorIntercept ##############################################################

test_that("rescalePriorIntercept works with Exchangeable - with covariates", {
    rescalePriorIntercept <- demest:::rescalePriorIntercept
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    df <- data.frame(country = rep(letters[1:5], times = 2),
                     sex = rep(c("F", "M"), each = 5),
                     income = rnorm(5))
    covariates <- Covariates(mean ~ income, data = df)
    spec.term <- Exch(covariates = covariates)
    spec.int <- ExchFixed()
    beta.term <- rnorm(10)
    beta.int <- rnorm(1)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(region = letters[1:5],
                                                   sex = c("F", "M"))))
    metadata.term <- new("MetaData",
                         nms = c("country", "sex"),
                         dimtypes = c("state", "sex"),
                         DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                          new("Sexes", dimvalues = c("F", "M"))))
    prior.term <- initialPrior(spec.term,
                               beta = beta.term,
                               metadata = metadata.term,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.int <- initialPrior(spec.int,
                              beta = beta.int,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE, margin = 0L, strucZeroArray = strucZeroArray)
    skeleton.beta.term <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.term)
    skeleton.beta.int <- SkeletonBetaIntercept(first = 30L)
    skeletons.prior.term <- makeOutputPrior(prior = prior.term,
                                            metadata = metadata.term,
                                            pos = 50L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePriorIntercept(priorTerm = prior.term,
                          priorIntercept = prior.int,
                          skeletonBetaTerm = skeleton.beta.term,
                          skeletonBetaIntercept = skeleton.beta.int,
                          skeletonsPriorTerm = skeletons.prior.term,
                          adjustments = adjustments,
                          prefixAdjustments = prefix.adjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.term <- seq(from = skeleton.beta.term@first, to = skeleton.beta.term@last)
    here.int <- skeleton.beta.int@first
    here.coef.int <- skeletons.prior.term$coef@first
    data.term <- data[here.term, ]
    data.int <- data[here.int, ]
    data.coef.int <- data[here.coef.int, ]
    output.term <- output[here.term, ]
    output.int <- output[here.int, ]
    output.coef.int <- output[here.coef.int, ]
    expect_equal(as.numeric(adjustments[["model.prior.country:sex"]]), -data.coef.int)
    expect_equal(as.numeric(adjustments[["model.prior.(Intercept)"]]), data.coef.int)
    expect_equal(output.term, data.term - rep(data.coef.int, each = 10))
    expect_equal(output.int, data.int + data.coef.int)
    expect_equal(output.coef.int, rep(0, 20))
})

test_that("rescalePriorIntercept works with Exchangeable - without covariates", {
    rescalePriorIntercept <- demest:::rescalePriorIntercept
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    spec.term <- Exch()
    spec.int <- ExchFixed()
    beta.term <- rnorm(10)
    beta.int <- rnorm(1)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(region = letters[1:5],
                                                   sex = c("F", "M"))))
    metadata.term <- new("MetaData",
                         nms = c("country", "sex"),
                         dimtypes = c("state", "sex"),
                         DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                          new("Sexes", dimvalues = c("F", "M"))))
    prior.term <- initialPrior(spec.term,
                               beta = beta.term,
                               metadata = metadata.term,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.int <- initialPrior(spec.int,
                              beta = beta.int,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE, margin = 0L, strucZeroArray = strucZeroArray)
    skeleton.beta.term <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.term)
    skeleton.beta.int <- SkeletonBetaIntercept(first = 30L)
    skeletons.prior.term <- makeOutputPrior(prior = prior.term,
                                            metadata = metadata.term,
                                            pos = 50L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePriorIntercept(priorTerm = prior.term,
                          priorIntercept = prior.int,
                          skeletonBetaTerm = skeleton.beta.term,
                          skeletonBetaIntercept = skeleton.beta.int,
                          skeletonsPriorTerm = skeletons.prior.term,
                          adjustments = adjustments,
                          prefixAdjustments = prefix.adjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.term <- seq(from = skeleton.beta.term@first, to = skeleton.beta.term@last)
    here.int <- seq(from = skeleton.beta.int@first, to = skeleton.beta.int@last)
    data.term <- data[here.term, ]
    data.int <- data[here.int, ]
    output.term <- output[here.term, ]
    output.int <- output[here.int, ]
    means <- array(data.term, dim = c(5, 2, 20))
    means <- apply(means, 3, mean)
    expect_equal(as.numeric(adjustments[["model.prior.country:sex"]]), -as.numeric(means))
    expect_equal(as.numeric(adjustments[["model.prior.(Intercept)"]]), as.numeric(means))
    expect_equal(output.term, data.term - rep(means, each = 10))
})


test_that("rescalePriorIntercept works with DLM - with covariates", {
    rescalePriorIntercept <- demest:::rescalePriorIntercept
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    df <- data.frame(time = c(2001:2005, 2001:2005),
                     sex = rep(c("F", "M"), each = 5),
                     income = rnorm(5))
    covariates <- Covariates(mean ~ income, data = df)
    spec.term <- DLM(covariates = covariates)
    spec.int <- ExchFixed()
    beta.term <- rnorm(10)
    beta.int <- rnorm(1)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(time = 2001:2005,
                                                   sex = c("F", "M"))),
                             dimscales = c(time = "Points"))
    metadata.term <- new("MetaData",
                         nms = c("time", "sex"),
                         dimtypes = c("time", "sex"),
                         DimScales = list(new("Points", dimvalues = 2001:2005),
                                          new("Sexes", dimvalues = c("F", "M"))))
    prior.term <- initialPrior(spec.term,
                               beta = beta.term,
                               metadata = metadata.term,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.int <- initialPrior(spec.int,
                              beta = beta.int,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE, margin = 0L, strucZeroArray = strucZeroArray)
    skeleton.beta.term <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.term)
    skeleton.beta.int <- SkeletonBetaIntercept(first = 30L)
    skeletons.prior.term <- makeOutputPrior(prior = prior.term,
                                            metadata = metadata.term,
                                            pos = 50L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePriorIntercept(priorTerm = prior.term,
                          priorIntercept = prior.int,
                          skeletonBetaTerm = skeleton.beta.term,
                          skeletonBetaIntercept = skeleton.beta.int,
                          skeletonsPriorTerm = skeletons.prior.term,
                          adjustments = adjustments,
                          prefixAdjustments = prefix.adjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.term <- seq(from = skeleton.beta.term@first, to = skeleton.beta.term@last)
    here.int <- skeleton.beta.int@first
    here.level <- seq(from = skeletons.prior.term$level@first,
                      to = skeletons.prior.term$level@last)
    here.level.0 <- skeletons.prior.term$level@indices0
    here.coef.int <- skeletons.prior.term$coef@first
    data.term <- data[here.term, ]
    data.int <- data[here.int, ]
    data.level <- data[here.level, ]
    data.level.0 <- data.level[here.level.0, ]
    data.coef.int <- data[here.coef.int, ]
    output.term <- output[here.term, ]
    output.int <- output[here.int, ]
    output.coef.int <- output[here.coef.int, ]
    output.level <- output[here.level, ]
    output.level.0 <- output[here.level.0, ]
    means <- apply(data.level.0, 2, mean)
    expect_equal(output.term, data.term - rep(means, each = 10) - rep(data.coef.int, each = 10))
    expect_equal(output.level, data.level - rep(means, each = 12) - rep(data.coef.int, each = 12))
    expect_equal(output.int, data.int + means + data.coef.int)
    expect_equal(as.numeric(adjustments[["model.prior.(Intercept)"]]), means + data.coef.int)
})

test_that("rescalePriorIntercept works with DLM - without covariates", {
    rescalePriorIntercept <- demest:::rescalePriorIntercept
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    spec.term <- DLM()
    spec.int <- ExchFixed()
    beta.term <- rnorm(10)
    beta.int <- rnorm(1)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(time = 2001:2005,
                                                   sex = c("F", "M"))),
                             dimscales = c(time = "Points"))
    metadata.term <- new("MetaData",
                         nms = c("time", "sex"),
                         dimtypes = c("time", "sex"),
                         DimScales = list(new("Points", dimvalues = 2001:2005),
                                          new("Sexes", dimvalues = c("F", "M"))))
    prior.term <- initialPrior(spec.term,
                               beta = beta.term,
                               metadata = metadata.term,
                               sY = NULL,
                               isSaturated = FALSE, margin = 1:2, strucZeroArray = strucZeroArray)
    prior.int <- initialPrior(spec.int,
                              beta = beta.int,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE, margin = 0L, strucZeroArray = strucZeroArray)
    skeleton.beta.term <- SkeletonBetaTerm(first = 10L,
                                           metadata = metadata.term)
    skeleton.beta.int <- SkeletonBetaIntercept(first = 30L)
    skeletons.prior.term <- makeOutputPrior(prior = prior.term,
                                            metadata = metadata.term,
                                            pos = 50L)
    adjustments <- new.env(hash = TRUE)
    prefix.adjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescalePriorIntercept(priorTerm = prior.term,
                          priorIntercept = prior.int,
                          skeletonBetaTerm = skeleton.beta.term,
                          skeletonBetaIntercept = skeleton.beta.int,
                          skeletonsPriorTerm = skeletons.prior.term,
                          adjustments = adjustments,
                          prefixAdjustments = prefix.adjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here.term <- seq(from = skeleton.beta.term@first, to = skeleton.beta.term@last)
    here.int <- skeleton.beta.int@first
    here.level <- seq(from = skeletons.prior.term$level@first,
                      to = skeletons.prior.term$level@last)
    here.level.0 <- skeletons.prior.term$level@indices0
    data.term <- data[here.term, ]
    data.int <- data[here.int, ]
    data.level <- data[here.level, ]
    data.level.0 <- data.level[here.level.0, ]
    output.term <- output[here.term, ]
    output.int <- output[here.int, ]
    output.level <- output[here.level, ]
    means <- apply(data.level.0, 2, mean)
    expect_equal(output.term, data.term - rep(means, each = 10))
    expect_equal(output.level, data.level - rep(means, each = 12))
    expect_equal(output.int, data.int + means)
})


## rescaleSeason ######################################################################

test_that("rescaleSeason works with SeasonMixin", {
    rescaleSeason <- demest:::rescaleSeason
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    Skeleton <- demest:::Skeleton
    spec <- DLM(trend = NULL,
                season = Season(n = 4))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    prior@alphaDLM@.Data <- 1:11
    for (i in 1:11)
        prior@s@.Data[[i]] <- rnorm(4)
    skeleton <- makeOutputPrior(prior = prior,
                                metadata = metadata,
                                pos = 3L)
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    rescaleSeason(prior = prior,
                  skeleton = skeleton,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    lengths <- readBin(con = con, what = "integer", n = 2L)
    results <- readBin(con = con, what = "raw", n = length(results))
    output <- readBin(con = con, what = "double", n = 2000L)
    close(con)
    output <- matrix(output, nr = lengthIter)
    data <- matrix(data, nr = lengthIter)
    here0 <- seq(from = skeleton$season@first, length = 4)
    mean0 <- data[here0, ]
    mean0 <- apply(mean0, 2, mean)
    here.level <- skeleton$level@first:skeleton$level@last
    data.level <- data[here.level, ]
    output.level <- output[here.level, ]
    expect_equal(output.level, data.level + rep(mean0, each = 11))
    here.season <- skeleton$season@first:skeleton$season@last
    data.season <- data[here.season, ]
    output.season <- output[here.season, ]
    expect_equal(output.season, data.season - rep(mean0, each = 44))
})


## transferParamPrior ################################################################

## Exch

test_that("transferParamPrior works with ExchNormZero", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(1)
    ans.obtained <- transferParamPrior(prior = prior,
                                       values = values)
    ans.expected <- prior
    ans.expected@tau@.Data <- values
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with ExchNormZero", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(1)
    ans.R <- transferParamPrior(prior = prior,
                                values = values,
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with ExchNormCov", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- c(rnorm(8), runif(1))
    ans.obtained <- transferParamPrior(prior = prior,
                                       values = values)
    ans.expected <- prior
    ans.expected@eta@.Data <- values[1:8]
    ans.expected@tau@.Data <- values[9]
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with ExchNormCov", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- c(rnorm(8), runif(1))
    ans.R <- transferParamPrior(prior = prior,
                                values = values,
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with ExchRobustZero", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(1)
    ans.obtained <- transferParamPrior(prior = prior,
                                       values = values)
    ans.expected <- prior
    ans.expected@tau@.Data <- values
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with ExchRobustZero", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(1)
    ans.R <- transferParamPrior(prior = prior,
                                values = values,
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with ExchRobustCov", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(9)
    ans.obtained <- transferParamPrior(prior = prior,
                                       values = values)
    ans.expected <- prior
    ans.expected@eta@.Data <- values[1:8]
    ans.expected@tau@.Data <- values[9]
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with ExchRobustCov", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    set.seed(1)
    data <- data.frame(region = letters[1:10],
                       sex = rep(c("f", "m"), each = 5),
                       income = rnorm(10),
                       cat = rep(c("x" ,"y", "z"), times = c(2, 3, 5)))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    values <- runif(9)
    ans.R <- transferParamPrior(prior = prior,
                                values = values,
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior,
                                        values = values,
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})


## DLM - Norm, Zero

test_that("transferParamPrior works with DLMNoTrendNormZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendNormZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendNormZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendNormZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMNoTrendNormZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendNormZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendNormZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendNormZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})


## DLM - Norm, Cov

test_that("transferParamPrior works with DLMNoTrendNormCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendNormCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendNormCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendNormCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                          isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMNoTrendNormCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendNormCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendNormCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = Season(n = 4),
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendNormCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = Season(n = 4),
                covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})


## DLM - Robust, Zero

test_that("transferParamPrior works with DLMNoTrendRobustZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendRobustZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendRobustZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendRobustZeroNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMNoTrendRobustZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, season = Season(n = 4), error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendRobustZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(trend = NULL, season = Season(n = 4), error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendRobustZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendRobustZeroWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})


## DLM - Robust, Cov

test_that("transferParamPrior works with DLMNoTrendRobustCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendRobustCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendRobustCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovNoSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendRobustCovNoSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovNoSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMNoTrendRobustCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMNoTrendRobustCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with DLMWithTrendRobustCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = Season(n = 4),
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovWithSeasonPredict")
    ans.obtained <- transferParamPrior(prior = prior.new,
                                       values = extractValues(prior.old))
    ans.expected <- prior.new
    ans.expected@alphaDLM@.Data[1] <- prior.old@alphaDLM@.Data[11]
    ans.expected@omegaAlpha@.Data <- prior.old@omegaAlpha@.Data
    ans.expected@deltaDLM@.Data[1] <- prior.old@deltaDLM@.Data[11]
    ans.expected@omegaDelta@.Data <- prior.old@omegaDelta@.Data
    ans.expected@phi <- prior.old@phi
    ans.expected@s@.Data[[1]] <- prior.old@s@.Data[[11]]
    ans.expected@omegaSeason@.Data <- prior.old@omegaSeason@.Data
    ans.expected@eta@.Data <- prior.old@eta@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with DLMWithTrendRobustCovWithSeason", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    covariates <- Covariates(formula = formula,
                             data = data)
    spec <- DLM(season = Season(n = 4),
                covariates = covariates,
                error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1L, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, margin = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovWithSeasonPredict")
    ans.R <- transferParamPrior(prior = prior.new,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.generic <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.new,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("transferParamPrior works with MixNormZeroPredict", {
    transferParamPrior <- demest:::transferParamPrior
    predictPrior <- demest:::predictPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix()
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE, margin = 1:3, strucZeroArray = strucZeroArray,
                              multScale = 1)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = list(time = 2011:2030,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L, margin = 1:3, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "MixNormZeroPredict")
    ## create 'prior.updated' that does not have same values as prior.new
    ## for slots where values are transferred
    prior.updated <- predictPrior(prior.new)
    prior.updated@prodVectorsMix@.Data <- rep(0, times = 200)
    prior.updated@omegaVectorsMix@.Data <- runif(1)
    prior.updated@omegaComponentWeightMix@.Data <- runif(1)
    prior.updated@meanLevelComponentWeightMix@.Data <- runif(1)
    prior.updated@phiMix <- runif(1)
    prior.updated@omegaLevelComponentWeightMix@.Data <- runif(1)
    prior.updated@tau@.Data <- runif(1)
    expect_false(isTRUE(all.equal(prior.updated, prior.new)))
    ans.obtained <- transferParamPrior(prior = prior.updated,
                                       values = extractValues(prior.old))
    ans.expected <- prior.updated
    ans.expected@prodVectorsMix@.Data <- prior.old@prodVectorsMix@.Data
    ans.expected@omegaVectorsMix@.Data <- prior.old@omegaVectorsMix@.Data
    ans.expected@omegaComponentWeightMix@.Data <- prior.old@omegaComponentWeightMix@.Data
    lcwo <- matrix(prior.old@levelComponentWeightMix@.Data,
                   nrow = 10)[10,]
    ans.expected@levelComponentWeightOldMix@.Data <- lcwo
    ans.expected@meanLevelComponentWeightMix@.Data <- prior.old@meanLevelComponentWeightMix@.Data
    ans.expected@phiMix <- prior.old@phiMix    
    ans.expected@omegaLevelComponentWeightMix@.Data <- prior.old@omegaLevelComponentWeightMix@.Data
    ans.expected@tau@.Data <- prior.old@tau@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferParamPrior give same answer with MixNormZero", {
    transferParamPrior <- demest:::transferParamPrior
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    predictPrior <- demest:::predictPrior
    extractValues <- demest:::extractValues
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix()
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1,
                              isSaturated = FALSE, margin = 1:3, strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = list(time = 2011:2030,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L, margin = 1:3, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "MixNormZeroPredict")
    ## create 'prior.updated' that does not have same values as prior.new
    ## for slots where values are transferred
    prior.updated <- predictPrior(prior.new)
    prior.updated@prodVectorsMix@.Data <- rep(0, times = 200)
    prior.updated@omegaVectorsMix@.Data <- runif(1)
    prior.updated@omegaComponentWeightMix@.Data <- runif(1)
    prior.updated@meanLevelComponentWeightMix@.Data <- runif(1)
    prior.updated@phiMix <- runif(1)
    prior.updated@omegaLevelComponentWeightMix@.Data <- runif(1)
    prior.updated@tau@.Data <- runif(1)
    expect_false(isTRUE(all.equal(prior.updated, prior.new)))
    ans.R <- transferParamPrior(prior = prior.updated,
                                values = extractValues(prior.old),
                                useC = FALSE)
    ans.C.specific <- transferParamPrior(prior = prior.updated,
                                         values = extractValues(prior.old),
                                         useC = TRUE,
                                         useSpecific = TRUE)
    ans.C.generic <- transferParamPrior(prior = prior.updated,
                                        values = extractValues(prior.old),
                                        useC = TRUE,
                                        useSpecific = FALSE)
    expect_identical(ans.R, ans.C.specific)
    expect_identical(ans.C.generic, ans.C.specific)
})    


## hasEstimated ######################################################################

## test_that("hasEstimated works", {
##     hasEstimated <- demest:::hasEstimated
##     x <- new("PriorVarDLMNormKnown")
##     expect_false(hasEstimated(x))
##     x <- new("PriorVarDLMNormUnknown")
##     expect_true(hasEstimated(x))
##     x <- new("PriorVarDLMRobustKnown")
##     expect_false(hasEstimated(x))
##     x <- new("PriorVarDLMRobustUnknown")
##     expect_true(hasEstimated(x))
##     x <- new("PriorVarDLMZero")
##     expect_false(hasEstimated(x))
## })


## whereEstimated #####################################################################

test_that("whereEstimated works", {
    whereEstimated <- demest:::whereEstimated
    x <- new("ExchFixed")
    expect_identical(whereEstimated(x),
                     character())
    x <- new("ExchNormZero")
    expect_identical(whereEstimated(x),
                     "scaleError")
    x <- new("ExchNormCov")
    expect_identical(whereEstimated(x),
                     c("coef", "scaleError"))
    x <- new("ExchRobustZero")
    expect_identical(whereEstimated(x),
                     "scaleError")
    x <- new("ExchRobustCov")
    expect_identical(whereEstimated(x),
                     c("coef", "scaleError"))
    x <- new("DLMNoTrendNormZeroNoSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroWithSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovNoSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovWithSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroNoSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroWithSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovNoSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovWithSeason")
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("scaleTrend",
                       "damp",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- TRUE
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("scaleLevel",
                       "scaleTrend",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("KnownCertain")
    expect_identical(whereEstimated(x),
                     NULL)
    x <- new("KnownUncertain")
    expect_identical(whereEstimated(x),
                     NULL)
    x <- new("MixNormZero")
    expect_identical(whereEstimated(x),
                     c("scaleComponents",
                       "scale1",
                       "mean",
                       "damp",
                       "scale2",
                       "scaleError"))
    x <- new("Zero")
    expect_identical(whereEstimated(x),
                     NULL)
})


