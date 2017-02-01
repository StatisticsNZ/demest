
context("updateBetaAndPriorBeta")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

test_that("updateBetaAndPriorBeta works with ExchFixed", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- ExchFixed()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchFixed")
        vbar <- rnorm(2)
        n <- 10L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        set.seed(seed)
        beta2 <- rnorm(2,
                       mean = (n*vbar/sigma^2)/(n/sigma^2+1/prior0@tau@.Data^2),
                       sd = 1/sqrt(n/sigma^2+1/prior0@tau@.Data^2))
        expect_identical(prior1, prior0)
        expect_equal(beta2, beta1)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with ExchFixed", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- ExchFixed()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchFixed")
        vbar <- rnorm(2)
        n <- 10L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with ExchNormZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        ## no max
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "ExchNormZero")
        expect_true(all(beta1 != beta0))
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        ## with max
        set.seed(seed)
        spec <- Exch(error = Error(scale = HalfT(max = 0.5)))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "ExchNormZero")
        expect_true(all(beta1 != beta0))
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same ansewr with ExchNormZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        ## no max
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## with max
        set.seed(seed)
        spec <- Exch(error = Error(scale = HalfT(max = 0.5)))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


test_that("updateBetaAndPriorBeta works with ExchRobustZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(100)
        spec <- Exch(error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchRobustZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "ExchRobustZero")
        expect_true(all(beta1 != beta0))
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta@.Data != prior0@UBeta@.Data))
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with ExchRobustZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(100)
        spec <- Exch(error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchRobustZero")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with ExchNormCov", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- Exch(covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormCov")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "ExchNormCov")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with ExchNormCov", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- Exch(covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchNormCov")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with ExchRobustCov", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- Exch(covariates = covariates,
                     error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchRobustCov")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "ExchRobustCov")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@nuEtaCoef, prior0@nuEtaCoef)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with ExchRobustCov", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- Exch(covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "ExchRobustCov")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## DLM - Norm, Zero

test_that("updateBetaAndPriorBeta works with DLMNoTrendNormZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormZeroNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendNormZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendNormZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendNormZeroNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendNormZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMNoTrendNormZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        expect_is(prior1, "DLMNoTrendNormZeroWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendNormZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendNormZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        expect_is(prior1, "DLMWithTrendNormZeroWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendNormZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})





## DLM - Norm, Cov

test_that("updateBetaAndPriorBeta works with DLMNoTrendNormCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(trend = NULL,
                    covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormCovNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with DLMNoTrendNormCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(trend = NULL,
                    covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updateBetaAndPriorBeta(prior0,
                                                 vbar = vbar,
                                                 n = n,
                                                 sigma = sigma, 
                                                 useC = TRUE,
                                                 useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updateBetaAndPriorBeta(prior0,
                                                vbar = vbar,
                                                n = n,
                                                sigma = sigma, 
                                                useC = TRUE,
                                                useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendNormCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        expect_is(prior1, "DLMWithTrendNormCovNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with DLMWithTrendNormCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updateBetaAndPriorBeta(prior0,
                                                 vbar = vbar,
                                                 n = n,
                                                 sigma = sigma, 
                                                 useC = TRUE,
                                                 useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updateBetaAndPriorBeta(prior0,
                                                vbar = vbar,
                                                n = n,
                                                sigma = sigma, 
                                                useC = TRUE,
                                                useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("updateBetaAndPriorBeta works with DLMNoTrendNormCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(trend = NULL,
                    covariates = covariates,
                    season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormCovWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with DLMNoTrendNormCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(trend = NULL,
                    covariates = covariates,
                    season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updateBetaAndPriorBeta(prior0,
                                                 vbar = vbar,
                                                 n = n,
                                                 sigma = sigma, 
                                                 useC = TRUE,
                                                 useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updateBetaAndPriorBeta(prior0,
                                                vbar = vbar,
                                                n = n,
                                                sigma = sigma, 
                                                useC = TRUE,
                                                useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendNormCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        season <- Season(n = 2)
        spec <- DLM(covariates = covariates,
                    season = season)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendNormCovWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@omegaAlpha != prior0@omegaAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with DLMWithTrendNormCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        season <- Season(n = 2)
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        spec <- DLM(season = season,
                    covariates = covariates)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updateBetaAndPriorBeta(prior0,
                                                 vbar = vbar,
                                                 n = n,
                                                 sigma = sigma, 
                                                 useC = TRUE,
                                                 useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updateBetaAndPriorBeta(prior0,
                                                vbar = vbar,
                                                n = n,
                                                sigma = sigma, 
                                                useC = TRUE,
                                                useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C.specific)
        else
            expect_equal(ans.R, ans.C.specific)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})


## DLM - Robust, Zero

test_that("updateBetaAndPriorBeta works with DLMNoTrendRobustZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendRobustZeroNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendRobustZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendRobustZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        error <- Error(robust = TRUE)
        spec <- DLM(error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustZeroNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendRobustZeroNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        error <- Error(robust = TRUE)
        spec <- DLM(error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustZeroNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMNoTrendRobustZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        for (i in 1:5) {
            l <- updateBetaAndPriorBeta(prior1,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
            prior1 <- l[[2]]
        }
        expect_is(prior1, "DLMNoTrendRobustZeroWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendRobustZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendRobustZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustZeroWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendRobustZeroWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
          spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustZeroWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## DLM - Robust, Cov

test_that("updateBetaAndPriorBeta works with DLMNoTrendRobustCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendRobustCovNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendRobustCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendRobustCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        damp <- Damp(min = 0.4, max = 0.999)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- DLM(damp = damp, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustCovNoSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        # trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendRobustCovNoSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- DLM(covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustCovNoSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMNoTrendRobustCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        season <- Season(n = 4)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, season = season, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendRobustCovWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMNoTrendRobustCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        season <- Season(n = 4)
        error <- Error(robust = TRUE)
        spec <- DLM(trend = NULL, season = season, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMNoTrendRobustCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetaAndPriorBeta works with DLMWithTrendRobustCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        damp <- Damp(min = 0.4, max = 0.999)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        season <- Season(n = 4)
        error <- Error(robust = TRUE)
        spec <- DLM(damp = damp, season = season, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustCovWithSeason")
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_true(all(prior1@alphaDLM != prior0@alphaDLM))
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(prior1@phi != prior0@phi)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        # trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_false(identical(prior1@GWithTrend, prior0@GWithTrend))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
        ## season
        expect_identical(prior1@ASeason, prior0@ASeason)
        expect_true(all(unlist(prior1@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(prior1@nSeason, prior0@nSeason)
        expect_identical(prior1@nuSeason, prior0@nuSeason)
        ## covariates
        expect_identical(prior1@P, prior0@P)
        expect_identical(prior1@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(prior1@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(prior1@UEtaCoef != prior0@UEtaCoef))
        expect_identical(prior1@Z, prior0@Z)
    }
})

test_that("R and C version updateBetaAndPriorBeta give same answer with DLMWithTrendRobustCovWithSeason", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(time = rep(1:10, times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg <- list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        season <- Season(n = 4)
        error <- Error(robust = TRUE)
        spec <- DLM(season = season, covariates = covariates, error = error)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "DLMWithTrendRobustCovWithSeason")
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = FALSE)
        set.seed(seed)
        ans.C <- updateBetaAndPriorBeta(prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma, 
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})
