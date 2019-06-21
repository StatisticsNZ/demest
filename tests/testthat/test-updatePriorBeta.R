
context("updatePriorBeta")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("updatePriorBeta works with ExchFixed - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    ## 'allStrucZero' all FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- ExchFixed()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = c("f", "m"))))
        strucZeroArray <- ValuesOne(c(1L, 1L), labels = c("f", "m"), name = "sex")
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchFixed")
        vbar <- rnorm(2)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = rnorm(20),
                                        sigma = sigma)
        ans.expected <- prior0
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with ExchFixed - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta0 <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("f", "m"))))
    strucZeroArray <- ValuesOne(c(1L, 1L), labels = c("f", "m"), name = "sex")
    prior0 <- initialPrior(spec,
                           beta = beta0,
                           metadata = metadata,
                           sY = NULL,
                           isSaturated = FALSE,
                           margin = 1L,
                           strucZeroArray = strucZeroArray)
    expect_is(prior0, "ExchFixed")
    sigma <- runif(1)
    ans.R <- updatePriorBeta(prior0,
                             beta = beta0,
                             thetaTransformed = rnorm(20),
                             sigma = sigma, 
                             useC = FALSE)
    ans.C <- updatePriorBeta(prior0,
                             beta = beta0,
                             thetaTransformed = rnorm(20),
                             sigma = sigma, 
                             useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("updatePriorBeta works with ExchNormZero - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormZero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "ExchNormZero")
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
    }
})

test_that("R and C versions of updatePriorBeta give same ansewr with ExchNormZero - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormZero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with ExchNormZero - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormZero")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_identical(ans.obtained@tau@.Data, sigma)
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with ExchNormZero - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Exch()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormZero")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with ExchRobustZero", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(100)
        spec <- Exch(error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchRobustZero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "ExchRobustZero")
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta@.Data != prior0@UBeta@.Data))
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with ExchRobustZero", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(100)
        spec <- Exch(error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchRobustZero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with ExchNormCov - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income + cat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormCov")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "ExchNormCov")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with ExchNormCov - not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income + cat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormCov")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with ExchNormCov - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "ExchNormCov")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "ExchNormCov")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_identical(ans.obtained@tau@.Data, sigma)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
})

test_that("updatePriorBeta works with ExchRobustCov", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "ExchRobustCov")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "ExchRobustCov")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@nuEtaCoef, prior0@nuEtaCoef)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
    }
})

test_that("R and C versions of updatePriorBeta give same answer with ExchRobustCov", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "ExchRobustCov")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## DLM - Norm, Zero

test_that("updatePriorBeta works with DLMNoTrendNormZeroNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                             beta = beta0,
                                    thetaTransformed = thetaTransformed,
                                    sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormZeroNoSeason")
        phi.updated <- FALSE
        if (!phi.updated)
            phi.updated <- ans.obtained@phi != prior0@phi
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
    }
    expect_true(phi.updated)
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendNormZeroNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMNoTrendNormZeroNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL,
                    damp = Damp(min = 0.1, max = 0.8))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormZeroNoSeason")
        phi.updated <- FALSE
        if (!phi.updated)
            phi.updated <- ans.obtained@phi != prior0@phi
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
    }
    expect_true(phi.updated)
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendNormZeroNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL)
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMNoTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendNormZeroNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(damp = Damp(min = 0, max = 1))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendNormZeroNoSeason")
        phi.updated <- FALSE
        if (!phi.updated)
            phi.updated <- ans.obtained@phi != prior0@phi
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
    }
    expect_true(phi.updated)
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendNormZeroNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendNormZeroNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    phi.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(damp = Damp(min = 0, max = 1))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendNormZeroNoSeason")
        if (!phi.updated)
            phi.updated <- ans.obtained@phi != prior0@phi
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
    }
    expect_true(phi.updated)
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendNormZeroNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM()
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMWithTrendNormZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMNoTrendNormZeroWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = beta0,
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMNoTrendNormZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendNormZeroWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               strucZeroArray = strucZeroArray,
                               margin = 1L)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMNoTrendNormZeroWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = beta0,
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMNoTrendNormZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendNormZeroWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendNormZeroWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = beta0,
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = beta0,
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMWithTrendNormZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendNormZeroWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendNormZeroWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = rnorm(10),
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMWithTrendNormZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendNormZeroWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 2))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormZeroWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})



## DLM - Norm, Cov

test_that("updatePriorBeta works with DLMNoTrendNormCovNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               margin = 1L,
                               strucZeroArray = strucZeroArray,
                               isSaturated = FALSE)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormCovNoSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
    expect_true(updated.phi)
})

test_that("R and C versions of updatePriorBeta give same answer with DLMNoTrendNormCovNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMNoTrendNormCovNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormCovNoSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
    expect_true(updated.phi)
})

test_that("R and C versions of updatePriorBeta give same answer with DLMNoTrendNormCovNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMWithTrendNormCovNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = rnorm(10),
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMWithTrendNormCovNoSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with DLMWithTrendNormCovNoSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMWithTrendNormCovNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = rnorm(10),
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMWithTrendNormCovNoSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with DLMWithTrendNormCovNoSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMNoTrendNormCovWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormCovWithSeason")
        if (!updated.phi && (ans.obtained@phi != prior0@phi))
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
    expect_true(updated.phi)
})

test_that("R and C versions of updatePriorBeta give same answer with DLMNoTrendNormCovWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMNoTrendNormCovWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendNormCovWithSeason")
        if (!updated.phi && (ans.obtained@phi != prior0@phi))
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
    expect_true(updated.phi)
})

test_that("R and C versions of updatePriorBeta give same answer with DLMNoTrendNormCovWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendNormCovWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMWithTrendNormCovWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5)
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = rnorm(10),
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendNormCovWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with DLMWithTrendNormCovWithSeason - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMWithTrendNormCovWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendNormCovWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_true(ans.obtained@omegaAlpha != prior0@omegaAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        if (ans.obtained@phi == prior0@phi)
            expect_identical(ans.obtained@GWithTrend, prior0@GWithTrend)
        else
            expect_false(identical(ans.obtained@GWithTrend, prior0@GWithTrend))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with DLMWithTrendNormCovWithSeason - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendNormCovWithSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C.specific <- updatePriorBeta(prior0,
                                          beta = beta0,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma, 
                                          useC = TRUE,
                                          useSpecific = TRUE)
        set.seed(seed)
        ans.C.generic <- updatePriorBeta(prior0,
                                         beta = beta0,
                                         thetaTransformed = thetaTransformed,
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

test_that("updatePriorBeta works with DLMNoTrendRobustZeroNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL, isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendRobustZeroNoSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
    }
    expect_true(updated.phi)
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendRobustZeroNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendRobustZeroNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustZeroNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendRobustZeroNoSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
    }
    expect_true(updated.phi)
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendRobustZeroNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustZeroNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMNoTrendRobustZeroWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        for (i in 1:5) {
            ans.obtained <- updatePriorBeta(ans.obtained,
                                            beta = rnorm(10),
                                            thetaTransformed = thetaTransformed,
                                            sigma = sigma)
        }
        expect_is(ans.obtained, "DLMNoTrendRobustZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_true(ans.obtained@phi != prior0@phi)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendRobustZeroWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(trend = NULL, season = Season(n = 2), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendRobustZeroWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendRobustZeroWithSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## Trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendRobustZeroWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM(season = Season(n = 4), error = Error(robust = TRUE))
        beta0 <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustZeroWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## DLM - Robust, Cov

test_that("updatePriorBeta works with DLMNoTrendRobustCovNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustCovNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendRobustCovNoSeason")
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendRobustCovNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustCovNoSeason")
        thetaTransformed <- rnorm(10)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendRobustCovNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    phi.updated <- FALSE
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustCovNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendRobustCovNoSeason")
        if (!phi.updated)
            phi.updated <- ans.obtained@phi != prior0@phi
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
                                        # trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
    expect_true(phi.updated)
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendRobustCovNoSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustCovNoSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMNoTrendRobustCovWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMNoTrendRobustCovWithSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
    expect_true(updated.phi)
})

test_that("R and C version updatePriorBeta give same answer with DLMNoTrendRobustCovWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMNoTrendRobustCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with DLMWithTrendRobustCovWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        expect_is(ans.obtained, "DLMWithTrendRobustCovWithSeason")
        if (!updated.phi && ans.obtained@phi != prior0@phi)
            updated.phi <- TRUE
        ## basic
        expect_identical(ans.obtained@J, prior0@J)
        expect_identical(ans.obtained@ATau, prior0@ATau)
        expect_identical(ans.obtained@nuTau, prior0@nuTau)
        expect_true(ans.obtained@tau@.Data != prior0@tau@.Data)
        ## DLM
        expect_identical(ans.obtained@AAlpha, prior0@AAlpha)
        expect_true(all(ans.obtained@alphaDLM[-1] != prior0@alphaDLM[-1]))
        expect_identical(ans.obtained@K, prior0@K)
        expect_identical(ans.obtained@L, prior0@L)
        expect_identical(ans.obtained@nuAlpha, prior0@nuAlpha)
        expect_false(prior0@phiKnown@.Data)
        expect_identical(ans.obtained@phiKnown, prior0@phiKnown)
        expect_identical(ans.obtained@minPhi, prior0@minPhi)
        expect_identical(ans.obtained@maxPhi, prior0@maxPhi)
                                        # trend
        expect_identical(ans.obtained@ADelta, prior0@ADelta)
        expect_true(all(ans.obtained@deltaDLM != prior0@deltaDLM))
        expect_identical(ans.obtained@nuDelta, prior0@nuDelta)
        expect_true(ans.obtained@omegaDelta != prior0@omegaDelta)
        expect_false(identical(ans.obtained@WSqrt, prior0@WSqrt))
        expect_false(identical(ans.obtained@WSqrtInvG, prior0@WSqrtInvG))
        ## robust
        expect_identical(ans.obtained@nuBeta, prior0@nuBeta)
        expect_true(all(ans.obtained@UBeta != prior0@UBeta))
        ## season
        expect_identical(ans.obtained@ASeason, prior0@ASeason)
        expect_true(all(unlist(ans.obtained@s@.Data) != unlist(prior0@s@.Data)))
        expect_identical(ans.obtained@nSeason, prior0@nSeason)
        expect_identical(ans.obtained@nuSeason, prior0@nuSeason)
        ## covariates
        expect_identical(ans.obtained@P, prior0@P)
        expect_identical(ans.obtained@AEtaIntercept, prior0@AEtaIntercept)
        expect_identical(ans.obtained@AEtaCoef, prior0@AEtaCoef)
        expect_true(all(ans.obtained@UEtaCoef != prior0@UEtaCoef))
        expect_identical(ans.obtained@Z, prior0@Z)
    }
    expect_true(updated.phi)
})

test_that("R and C version updatePriorBeta give same answer with DLMWithTrendRobustCovWithSeason", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "DLMWithTrendRobustCovWithSeason")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## Known #################################################################################

test_that("updatePriorBeta works with KnownCertain", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "KnownCertain")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        set.seed(seed)
        expect_identical(ans.obtained, prior0)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with KnownCertain", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "KnownCertain")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta works with KnownUncertain", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "KnownUncertain")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        set.seed(seed)
        expect_identical(ans.obtained, prior0)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with KnownUncertain", {
    updatePriorBeta <- demest:::updatePriorBeta
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
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "KnownUncertain")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})



## Mix #################################################################################


test_that("updatePriorBeta updates correct slots with MixNormZero - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    set.seed(1)
    beta0 <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("reg", "time", "age"),
                    dimtypes = c("state", "time", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                     new("Points", dimvalues = 2001:2010),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010,
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix(weights = Weights(mean = -10))
    prior0 <- initialPrior(spec,
                           beta = beta0,
                           metadata = metadata,
                           sY = NULL,
                           multScale = 1,
                           isSaturated = FALSE,
                           margin = 1:3,
                           strucZeroArray = strucZeroArray)
    thetaTransformed <- rnorm(1000)
    sigma <- runif(1)
    ans.obtained <- updatePriorBeta(prior = prior0,
                                    beta = beta0,
                                    thetaTransformed = thetaTransformed,
                                    sigma = sigma)
    expect_is(ans.obtained, "MixNormZero")
    ## u
    u0 <- prior0@latentWeightMix@.Data
    u1 <- ans.obtained@latentWeightMix@.Data
    expect_true(all(u0 != u1))
    ## k
    k0 <- prior0@indexClassMix
    k1 <- ans.obtained@indexClassMix
    expect_true(!all(k0 == k1))
    ## z 
    z0 <- prior0@latentComponentWeightMix@.Data
    z1 <- ans.obtained@latentComponentWeightMix@.Data
    expect_true(!all(z0 == z1))
    ## W
    W0 <- prior0@componentWeightMix@.Data
    W1 <- ans.obtained@componentWeightMix@.Data
    expect_true(all(W0 != W1))
    ## v
    v0 <- prior0@weightMix@.Data
    v1 <- ans.obtained@weightMix@.Data
    expect_true(all(v0 != v1))
    ## psi
    psi0 <- unlist(prior0@vectorsMix)
    psi1 <- unlist(ans.obtained@vectorsMix)
    expect_true(!all(psi0 == psi1))
    ## sigma_delta
    sdelta0 <- prior0@tau@.Data
    sdelta1 <- ans.obtained@tau@.Data
    expect_true(sdelta0 != sdelta1)
    ## sigma_e
    se0 <- prior0@omegaVectorsMix@.Data
    se1 <- ans.obtained@omegaVectorsMix@.Data
    expect_true(se0 != se1)
    ## sigma_epsilon
    sepsilon0 <- prior0@omegaComponentWeightMix@.Data
    sepsilon1 <- ans.obtained@omegaComponentWeightMix@.Data
    expect_true(sepsilon0 != sepsilon1)
    ## sigma_eta
    seta0 <- prior0@omegaLevelComponentWeightMix@.Data
    seta1 <- ans.obtained@omegaLevelComponentWeightMix@.Data
    expect_true(seta0 != seta1)
    ## mu
    mu0 <- prior0@meanLevelComponentWeightMix@.Data
    mu1 <- ans.obtained@meanLevelComponentWeightMix@.Data
    expect_true(mu0 != mu1)
    ## alpha
    alpha0 <- prior0@levelComponentWeightMix@.Data
    alpha1 <- ans.obtained@levelComponentWeightMix@.Data
    expect_true(all(alpha0 != alpha1))
    ## alphaMix
    alphaMix0 <- prior0@alphaMix@.Data
    alphaMix1 <- ans.obtained@alphaMix@.Data
    expect_true(all(alphaMix0 != alphaMix1))
})

test_that("R and C versions of updatePriorBeta give same answer with MixNormZero - is not saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        strucZeroArray <- Counts(array(1L,
                                       dim = c(2, 10, 10),
                                       dimnames = list(reg = c("a", "b"),
                                                       time = 2001:2010,
                                                       age = 0:9)),
                                 dimscales = c(time = "Points", age = "Intervals"))
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
                               multScale = 1,
                               isSaturated = FALSE,
                               margin = 1:3,
                               strucZeroArray = strucZeroArray)
        thetaTransformed <- rnorm(2000)
        sigma <- runif(1)
        set.seed(seed + 1)
        ans.R <- updatePriorBeta(prior = prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma,
                                 useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updatePriorBeta(prior = prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma,
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePriorBeta updates correct slots with MixNormZero - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    set.seed(1)
    beta0 <- rnorm(200)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010,
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
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
                           multScale = 1,
                           isSaturated = TRUE,
                           strucZeroArray = strucZeroArray,
                           margin = 1:3)
    thetaTransformed <- rnorm(200)
    sigma <- runif(1)
    ans.obtained <- updatePriorBeta(prior = prior0,
                                    beta = beta0,
                                    thetaTransformed = thetaTransformed,
                                    sigma = sigma)
    expect_is(ans.obtained, "MixNormZero")
    ## u
    u0 <- prior0@latentWeightMix@.Data
    u1 <- ans.obtained@latentWeightMix@.Data
    expect_true(all(u0 != u1))
    ## k
    k0 <- prior0@indexClassMix
    k1 <- ans.obtained@indexClassMix
    expect_true(!all(k0 == k1))
    ## z 
    z0 <- prior0@latentComponentWeightMix@.Data
    z1 <- ans.obtained@latentComponentWeightMix@.Data
    expect_true(!all(z0 == z1))
    ## W
    W0 <- prior0@componentWeightMix@.Data
    W1 <- ans.obtained@componentWeightMix@.Data
    expect_true(all(W0 != W1))
    ## v
    v0 <- prior0@weightMix@.Data
    v1 <- ans.obtained@weightMix@.Data
    expect_true(all(v0 != v1))
    ## psi
    psi0 <- unlist(prior0@vectorsMix)
    psi1 <- unlist(ans.obtained@vectorsMix)
    expect_true(!all(psi0 == psi1))
    ## sigma_delta
    sdelta0 <- prior0@tau@.Data
    sdelta1 <- ans.obtained@tau@.Data
    expect_true(sdelta0 != sdelta1)
    ## sigma_e
    se0 <- prior0@omegaVectorsMix@.Data
    se1 <- ans.obtained@omegaVectorsMix@.Data
    expect_true(se0 != se1)
    ## sigma_epsilon
    sepsilon0 <- prior0@omegaComponentWeightMix@.Data
    sepsilon1 <- ans.obtained@omegaComponentWeightMix@.Data
    expect_true(sepsilon0 != sepsilon1)
    ## sigma_eta
    seta0 <- prior0@omegaLevelComponentWeightMix@.Data
    seta1 <- ans.obtained@omegaLevelComponentWeightMix@.Data
    expect_true(seta0 != seta1)
    ## mu
    mu0 <- prior0@meanLevelComponentWeightMix@.Data
    mu1 <- ans.obtained@meanLevelComponentWeightMix@.Data
    expect_true(mu0 != mu1)
    ## alpha
    alpha0 <- prior0@levelComponentWeightMix@.Data
    alpha1 <- ans.obtained@levelComponentWeightMix@.Data
    expect_true(all(alpha0 != alpha1))
    ## alphaMix
    alphaMix0 <- prior0@alphaMix@.Data
    alphaMix1 <- ans.obtained@alphaMix@.Data
    expect_true(all(alphaMix0 != alphaMix1))
})

test_that("R and C versions of updatePriorBeta give same answer with MixNormZero - is saturated", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        metadata <- new("MetaData",
                        nms = c("reg", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                         new("Points", dimvalues = 2001:2010),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
        strucZeroArray <- Counts(array(1L,
                                       dim = c(2, 10, 10),
                                       dimnames = list(reg = c("a", "b"),
                                                       time = 2001:2010,
                                                       age = 0:9)),
                                 dimscales = c(time = "Points", age = "Intervals"))
        spec <- Mix(weights = Weights(mean = -10))
        beta0 <- rnorm(200)
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               multScale = 1,
                               isSaturated = TRUE,
                               margin = 1:3,
                               strucZeroArray = strucZeroArray)
        thetaTransformed <- rnorm(200)
        sigma <- runif(1)
        set.seed(seed + 1)
        ans.R <- updatePriorBeta(prior = prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma,
                                 useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updatePriorBeta(prior = prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma,
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## Zero #################################################################################

test_that("updatePriorBeta works with Zero", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Zero()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "Zero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updatePriorBeta(prior0,
                                        beta = beta0,
                                        thetaTransformed = thetaTransformed,
                                        sigma = sigma)
        set.seed(seed)
        expect_identical(ans.obtained, prior0)
    }
})

test_that("R and C versions of updatePriorBeta give same answer with Zero", {
    updatePriorBeta <- demest:::updatePriorBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- Zero()
        beta0 <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "sex",
                        dimtypes = "sex",
                        DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(sex = c("f", "m"))))
        prior0 <- initialPrior(spec,
                               beta = beta0,
                               metadata = metadata,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        expect_is(prior0, "Zero")
        thetaTransformed <- rnorm(20)
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = FALSE)
        set.seed(seed)
        ans.C <- updatePriorBeta(prior0,
                                 beta = beta0,
                                 thetaTransformed = thetaTransformed,
                                 sigma = sigma, 
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})
