
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
        n <- rep(10L, 2)
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
        n <- rep(10L, 2)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        spec <- DLM(trend = NULL,
                    damp = Damp(min = 0.1, max = 0.8))
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormZeroNoSeason")
        phi.updated <- FALSE
        if (!phi.updated)
            phi.updated <- prior1@phi != prior0@phi
        ## beta
        expect_true(all(beta1 != beta0))
        ## basic
        expect_identical(prior1@J, prior0@J)
        expect_identical(prior1@ATau, prior0@ATau)
        expect_identical(prior1@nuTau, prior0@nuTau)
        expect_true(prior1@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(prior1@AAlpha, prior0@AAlpha)
        expect_identical(prior1@K, prior0@K)
        expect_identical(prior1@L, prior0@L)
        expect_identical(prior1@nuAlpha, prior0@nuAlpha)
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
    }
    expect_true(phi.updated)
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
        n <- rep(5L, 10)
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
        spec <- DLM(damp = Damp(min = 0, max = 1))
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
                    damp = Damp(min = 0),
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormCovNoSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendNormCovWithSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendRobustZeroNoSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
    }
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustZeroNoSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
        expect_identical(prior1@nuDelta, prior0@nuDelta)
        expect_true(prior1@omegaDelta != prior0@omegaDelta)
        expect_false(identical(prior1@WSqrt, prior0@WSqrt))
        expect_false(identical(prior1@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(prior1@nuBeta, prior0@nuBeta)
        expect_true(all(prior1@UBeta != prior0@UBeta))
    }
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMNoTrendRobustCovWithSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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
    updated.phi <- FALSE
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
        n <- rep(5L, 10)
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        expect_is(prior1, "DLMWithTrendRobustCovWithSeason")
        if (!updated.phi && prior1@phi != prior0@phi)
            updated.phi <- TRUE
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
        expect_identical(prior1@phiKnown, prior0@phiKnown)
        expect_identical(prior1@minPhi, prior0@minPhi)
        expect_identical(prior1@maxPhi, prior0@maxPhi)
        # trend
        expect_identical(prior1@ADelta, prior0@ADelta)
        expect_true(all(prior1@deltaDLM != prior0@deltaDLM))
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
    expect_true(updated.phi)
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
        n <- rep(5L, 10)
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


## Known #################################################################################

test_that("updateBetaAndPriorBeta works with KnownCertain", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mean <- ValuesOne(c(0.1, 0.2), labels = c("f", "m"), name = "sex")
        spec <- Known(mean)
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "KnownCertain")
        vbar <- rnorm(2)
        n <- 9:10
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        set.seed(seed)
        beta2 <- as.numeric(mean)
        expect_identical(prior1, prior0)
        expect_equal(beta2, beta1)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with KnownCertain", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mean <- ValuesOne(c(0.1, 0.2), labels = c("f", "m"), name = "sex")
        spec <- Known(mean)
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "KnownCertain")
        vbar <- rnorm(2)
        n <- 9:10
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

test_that("updateBetaAndPriorBeta works with KnownUncertain", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mean <- ValuesOne(c(0.1, 0.2), labels = c("f", "m"), name = "sex")
        spec <- Known(mean, sd = 0.1)
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "KnownUncertain")
        vbar <- rnorm(2)
        n <- 9:10
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
                       mean = ((n*vbar/sigma^2 + prior0@alphaKnown@.Data/prior0@AKnownVec@.Data^2)
                           /(n/sigma^2+1/prior0@AKnownVec@.Data^2)),
                       sd = 1/sqrt(n/sigma^2+1/prior0@AKnownVec@.Data^2))
        expect_identical(prior1, prior0)
        expect_equal(beta2, beta1)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with KnownUncertain", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mean <- ValuesOne(c(0.1, 0.2), labels = c("f", "m"), name = "sex")
        spec <- Known(mean, sd = 0.1)
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "KnownUncertain")
        vbar <- rnorm(2)
        n <- 9:10
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



## Mix #################################################################################



test_that("updateBetaAndPriorBeta updates correct slots with MixNormZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    set.seed(1)
    beta0 <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("reg", "time", "age"),
                    dimtypes = c("state", "time", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                     new("Points", dimvalues = 2001:2010),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix(weights = Weights(mean = -10))
    prior0 <- initialPrior(spec,
                           beta = beta0,
                           metadata = metadata,
                           sY = NULL,
                           multScale = 1)
    vbar <- rnorm(200)
    n <- rep(5L, 200)
    sigma <- runif(1)
    l <- updateBetaAndPriorBeta(prior = prior0,
                                vbar = vbar,
                                n = n,
                                sigma = sigma)
    beta1 <- l[[1]]
    prior1 <- l[[2]]
    expect_is(prior1, "MixNormZero")
    ## beta
    expect_true(all(beta0 != beta1))
    ## u
    u0 <- prior0@latentWeightMix@.Data
    u1 <- prior1@latentWeightMix@.Data
    expect_true(all(u0 != u1))
    ## k
    k0 <- prior0@indexClassMix
    k1 <- prior1@indexClassMix
    expect_true(!all(k0 == k1))
    ## z 
    z0 <- prior0@latentComponentWeightMix@.Data
    z1 <- prior1@latentComponentWeightMix@.Data
    expect_true(!all(z0 == z1))
    ## W
    W0 <- prior0@componentWeightMix@.Data
    W1 <- prior1@componentWeightMix@.Data
    expect_true(all(W0 != W1))
    ## v
    v0 <- prior0@weightMix@.Data
    v1 <- prior1@weightMix@.Data
    expect_true(all(v0 != v1))
    ## psi
    psi0 <- unlist(prior0@vectorsMix)
    psi1 <- unlist(prior1@vectorsMix)
    expect_true(!all(psi0 == psi1))
    ## sigma_delta
    sdelta0 <- prior0@tau@.Data
    sdelta1 <- prior1@tau@.Data
    expect_true(sdelta0 != sdelta1)
    ## sigma_e
    se0 <- prior0@omegaVectorsMix@.Data
    se1 <- prior1@omegaVectorsMix@.Data
    expect_true(se0 != se1)
    ## sigma_epsilon
    sepsilon0 <- prior0@omegaComponentWeightMix@.Data
    sepsilon1 <- prior1@omegaComponentWeightMix@.Data
    expect_true(sepsilon0 != sepsilon1)
    ## sigma_eta
    seta0 <- prior0@omegaLevelComponentWeightMix@.Data
    seta1 <- prior1@omegaLevelComponentWeightMix@.Data
    expect_true(seta0 != seta1)
    ## mu
    mu0 <- prior0@meanLevelComponentWeightMix@.Data
    mu1 <- prior1@meanLevelComponentWeightMix@.Data
    expect_true(mu0 != mu1)
    ## alpha
    alpha0 <- prior0@levelComponentWeightMix@.Data
    alpha1 <- prior1@levelComponentWeightMix@.Data
    expect_true(all(alpha0 != alpha1))
    ## alphaMix
    alphaMix0 <- prior0@alphaMix@.Data
    alphaMix1 <- prior1@alphaMix@.Data
    expect_true(all(alphaMix0 != alphaMix1))
})

test_that("updateBetaAndPriorBeta updates correct slots with MixNormZero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        metadata <- new("MetaData",
                        nms = c("reg", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                         new("Points", dimvalues = 2001:2010),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
        spec <- Mix(weights = Weights(mean = -10))
        beta0 <- rnorm(200)
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               multScale = 1)
        vbar <- rnorm(200)
        n <- rep(5L, 200)
        sigma <- runif(1)
        set.seed(seed + 1)
        ans.R <- updateBetaAndPriorBeta(prior = prior0,
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma,
                                        useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateBetaAndPriorBeta(prior = prior0,
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


## Zero #################################################################################

test_that("updateBetaAndPriorBeta works with Zero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Zero()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "Zero")
        vbar <- rnorm(2)
        n <- 9:10
        sigma <- runif(1)
        set.seed(seed)
        l <- updateBetaAndPriorBeta(prior0,
                                    vbar = vbar,
                                    n = n,
                                    sigma = sigma)
        beta1 <- l[[1]]
        prior1 <- l[[2]]
        set.seed(seed)
        beta2 <- rep(0, 2)
        expect_identical(prior1, prior0)
        expect_equal(beta2, beta1)
    }
})

test_that("R and C versions of updateBetaAndPriorBeta give same answer with Zero", {
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Zero()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL)
        expect_is(prior0, "Zero")
        vbar <- rnorm(2)
        n <- 9:10
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
