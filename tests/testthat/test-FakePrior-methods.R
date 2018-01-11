

## context("FakePrior-generators")

## n.test <- 5
## test.identity <- FALSE
## test.extended <- TRUE


## ## fakeBeta ###########################################################################

## test_that("fakeBeta works with FakeExchFixed", {
##     fakeBeta <- demest:::fakeBeta
##     fakePrior <- demest:::fakePrior
##     spec <- ExchFixed(mean = 3, sd = 2)
##     metadata <- new("MetaData",
##                     nms = "region",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     set.seed(1)
##     ans.obtained <- fakeBeta(prior)
##     set.seed(1)
##     ans.expected <- rnorm(3, mean = prior@mean@.Data, sd = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("fakeBeta works with FakeExchNormZero", {
##     fakeBeta <- demest:::fakeBeta
##     fakePrior <- demest:::fakePrior
##     spec <- Exch(error = Error(scale = HalfT(scale = 1.3)))
##     metadata <- new("MetaData",
##                     nms = "region",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     set.seed(1)
##     ans.obtained <- fakeBeta(prior)
##     set.seed(1)
##     ans.expected <- rnorm(3, sd = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })


## ## makeFakeOutputPrior ######################################################################

## test_that("makeFakeOutputPrior works with FakeExchFixed", {
##     makeFakeOutputPrior <- demest:::makeFakeOutputPrior
##     fakePrior <- demest:::fakePrior
##     spec <- ExchFixed(mean = 3, sd = 2)
##     metadata <- new("MetaData",
##                     nms = "region",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     ans.obtained <- makeFakeOutputPrior(prior,
##                                         metadata = metadata)
##     ans.expected <- list(mean = prior@mean@.Data,
##                          sd = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakeOutputPrior works with FakeExchNormZero", {
##     makeFakeOutputPrior <- demest:::makeFakeOutputPrior
##     fakePrior <- demest:::fakePrior
##     spec <- Exch(error = Error(scale = HalfT(scale = 1.3)))
##     metadata <- new("MetaData",
##                     nms = "region",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     ans.obtained <- makeFakeOutputPrior(prior,
##                                         metadata = metadata)
##     ans.expected <- list(scaleError = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakeOutputPrior works with FakeDLMNoTrendNormZeroNoSeason", {
##     makeFakeOutputPrior <- demest:::makeFakeOutputPrior
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
##     ans.obtained <- makeFakeOutputPrior(prior = prior,
##                                         metadata = metadata)
##     level <- array(prior@alphaDLM[-1L],
##                    dim = dim(metadata),
##                    dimnames = dimnames(metadata))
##     level <- new("Values",
##                  .Data = level,
##                  metadata = metadata)
##     ans.expected <- list(level = level,
##                          scaleLevel = prior@omegaAlpha@.Data,
##                          damp = prior@phi,
##                          scaleError = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("makeFakeOutputPrior works with FakeDLMWithTrendNormZeroNoSeason", {
##     makeFakeOutputPrior <- demest:::makeFakeOutputPrior
##     fakePrior <- demest:::fakePrior
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.01)),
##                 trend = Trend(initial = Initial(sd = 0.1),
##                               scale = HalfT(scale = 0.01)),,
##                 error = Error(scale = HalfT(scale = 0.01)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 1:10)))
##     prior <- fakePrior(spec,
##                        metadata = metadata,
##                        isSaturated = FALSE)
##     ans.obtained <- makeFakeOutputPrior(prior = prior,
##                                         metadata = metadata)
##     level <- array(prior@alphaDLM[-1L],
##                    dim = dim(metadata),
##                    dimnames = dimnames(metadata))
##     level <- new("Values",
##                  .Data = level,
##                  metadata = metadata)
##     trend <- array(prior@deltaDLM[-1L],
##                    dim = dim(metadata),
##                    dimnames = dimnames(metadata))
##     trend <- new("Values",
##                  .Data = trend,
##                  metadata = metadata)
##     ans.expected <- list(level = level,
##                          scaleLevel = prior@omegaAlpha@.Data,
##                          trend = trend,
##                          scaleTrend = prior@omegaDelta@.Data,
##                          damp = prior@phi,
##                          scaleError = prior@tau@.Data)
##     expect_identical(ans.obtained, ans.expected)
## })







## ## test_that("fakeBeta works with SpecExchRobustCovUnknown", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     d <- data.frame(region = c("a", "b", "c"), income = rnorm(3))
## ##     x <- Exch(mean ~ income, data = d, robust = TRUE)
## ##     metadata <- new("MetaData",
## ##                     nms = "region",
## ##                     dimtypes = "state",
## ##                     DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
## ##     expect_error(fakeBeta(object = x, metadata = metadata),
## ##                  "priors must not have covariates when used in function 'fakeData'")
## ## })

## ## test_that("fakeBeta works with SpecUniform", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     x <- Unif()
## ##     metadata <- new("MetaData",
## ##                     nms = "age",
## ##                     dimtypes = "age",
## ##                     DimScales = list(new("Intervals", dimvalues = 1:4)))
## ##     expect_error(fakeBeta(object = x, metadata = metadata),
## ##                  "uniform prior may not be used in function 'fakeData'")
## ## })

## ## test_that("fakeBeta works with SpecKnownCertain", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     sweepAllMargins <- demest:::sweepAllMargins
## ##     ## one dimension
## ##     mean <- Values(array(rnorm(6), dim = 6, dimnames = list(age = 1:6)))
## ##     x <- Known(mean = mean)
## ##     metadata <- new("MetaData",
## ##                     nms = "age",
## ##                     dimtypes = "age",
## ##                     DimScales = list(new("Intervals", dimvalues = 1:4)))
## ##     ans.obtained <- fakeBeta(object = x, metadata = metadata)
## ##     ans.expected <- as.numeric(mean)[1:3]
## ##     ans.expected <- ans.expected - mean(ans.expected)
## ##     expect_identical(ans.obtained, ans.expected)
## ##     ## two dimensions
## ##     mean <- Values(array(rnorm(12),
## ##                          dim = c(6, 2),
## ##                          dimnames = list(age = 1:6, sex = c("m", "f"))))
## ##     x <- Known(mean = mean)
## ##     metadata <- new("MetaData",
## ##                     nms = c("age", "sex"),
## ##                     dimtypes = c("age", "state"),
## ##                     DimScales = list(new("Intervals", dimvalues = 1:5),
## ##                         new("Categories", dimvalues = c("f", "m"))))
## ##     ans.obtained <- fakeBeta(object = x, metadata = metadata)
## ##     ans.expected <- mean[1:4, 2:1]
## ##     ans.expected <- sweepAllMargins(ans.expected)
## ##     ans.expected <- as.double(ans.expected)
## ##     expect_identical(ans.obtained, ans.expected)
## ##     ## incompatible beta and prior
## ##     mean.wrong <- Values(array(rnorm(5), dim = 5, dimnames = list(age = 2:6)))
## ##     x.wrong <- Known(mean = mean.wrong)
## ##     expect_error(fakeBeta(object = x.wrong, metadata = metadata),
## ##                  "prior incompatible with 'y'")
## ## })

## ## test_that("fakeBeta works with SpecKnownUncertain", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     mean <- Values(array(rpois(6, lambda = 5), dim = 6, dimnames = list(age = 1:6)))
## ##     sd <- sqrt(mean)
## ##     x <- Known(mean = mean, sd = sd)
## ##     metadata <- new("MetaData",
## ##                     nms = "age",
## ##                     dimtypes = "age",
## ##                     DimScales = list(new("Intervals", dimvalues = 1:4)))
## ##     set.seed(1)
## ##     ans.obtained <- fakeBeta(object = x, metadata = metadata)
## ##     set.seed(1)
## ##     ans.expected <- rnorm(n = 3, mean = as.numeric(mean)[1:3], sd = as.numeric(sd)[1:3])
## ##     ans.expected <- ans.expected - mean(ans.expected)
## ##     expect_identical(ans.obtained, ans.expected)
## ##     mean.wrong <- Values(array(abs(rnorm(5)), dim = 5, dimnames = list(age = 2:6)))
## ##     x.wrong <- Known(mean = mean.wrong, sd = sqrt(mean.wrong))
## ##     expect_error(fakeBeta(object = x.wrong, metadata = metadata),
## ##                  "prior incompatible with 'y'")
## ## })

## ## test_that("fakeBeta works with SpecPoly", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     fakeDLMErrors <- demest:::fakeDLMErrors
## ##     for (seed in seq_len(n.test)) {
## ##         set.seed(seed)
## ##         spec <- new("SpecPoly",
## ##                     trend = new("SpecPolyComponentTrend",
## ##                         q = 2L,
## ##                         priorsW = list(new("SpecPriorVarDLMNormKnown", tau = runif(1, 0.1, 2)),
## ##                             new("SpecPriorVarDLMNormKnown", tau = runif(1, 0.1, 2))),
## ##                         m0 = rnorm(2),
## ##                         C0 = diag(runif(2))),
## ##                     seasonal = new("SpecPolyComponentSeasonal",
## ##                         q = 4L,
## ##                         priorsW = list(new("SpecPriorVarDLMNormKnown", tau = runif(1, 0.1, 2)),
## ##                             new("SpecPriorVarDLMZero"),
## ##                             new("SpecPriorVarDLMZero"),
## ##                             new("SpecPriorVarDLMZero")),
## ##                         m0 = rnorm(4),
## ##                         C0 = diag(runif(4))),
## ##                     priorV = new("SpecPriorVarDLMRobustKnown", nu = 4, tau = 0.2))
## ##         metadata <- new("MetaData",
## ##                         nms = "age",
## ##                         dimtypes = "age",
## ##                         DimScales = list(new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         trend <- fakeBeta(object = spec@trend, metadata = metadata)
## ##         season <- fakeBeta(object = spec@seasonal, metadata = metadata)
## ##         v <- fakeDLMErrors(spec = spec@priorV, J = 10L)
## ##         ans.expected <- trend + season + v
## ##         ans.expected <- ans.expected - mean(ans.expected)
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ## })

## ## test_that("fakeBeta works with SpecPolyComponentTrend", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     fakeDLMErrors <- demest:::fakeDLMErrors
## ##     rmvnorm1 <- demest:::rmvnorm1
## ##     tau1 <- runif(1, 0.01, 2)
## ##     tau2 <- runif(1, 0.01, 2)
## ##     m0 <- rnorm(2)
## ##     C0 <- diag(runif(2))
## ##     for (seed in seq_len(n.test)) {
## ##         set.seed(seed)
## ##         spec <- new("SpecPolyComponentTrend",
## ##                     q = 2L,
## ##                     priorsW = list(new("SpecPriorVarDLMNormKnown", tau = tau2),
## ##                         new("SpecPriorVarDLMNormKnown", tau = tau2)),
## ##                     m0 = m0,
## ##                     C0 = C0)
## ##         seed <- 1
## ##         metadata <- new("MetaData",
## ##                         nms = "age",
## ##                         dimtypes = "age",
## ##                         DimScales = list(new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         G <- matrix(c(1, 0, 1, 1), nrow = 2)
## ##         W <- rbind(fakeDLMErrors(spec@priorsW[[1]], J = 10L),
## ##                    fakeDLMErrors(spec@priorsW[[2]], J = 10L))
## ##         gamma <- matrix(nr = 2, ncol = 10)
## ##         gamma0 <- rmvnorm1(spec@m0, spec@C0)
## ##         gamma[,1] <- G %*% gamma0 + W[,1]
## ##         for (i in 2:10)
## ##             gamma[,i] <- G %*% gamma[,i-1] + W[,i]
## ##         ans.expected <- gamma[1,] - mean(gamma[1,])
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ##     nu <- rpois(1, lambda = 3) + 1.0
## ##     tau <- runif(1, 0.01, 2)
## ##     m0 <- rnorm(1)
## ##     C0 <- matrix(runif(1), nr = 1)
## ##     for (seed in seq_len(n.test)) {
## ##         set.seed(seed)
## ##         spec <- new("SpecPolyComponentTrend",
## ##                     q = 1L,
## ##                     priorsW = list(new("SpecPriorVarDLMRobustKnown", tau = tau, nu = nu)),
## ##                     m0 = m0,
## ##                     C0 = C0)
## ##         seed <- 1
## ##         metadata <- new("MetaData",
## ##                         nms = "age",
## ##                         dimtypes = "age",
## ##                         DimScales = list(new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         G <- matrix(1, nrow = 1)
## ##         W <- matrix(fakeDLMErrors(spec@priorsW[[1]], J = 10L), nr = 1)
## ##         gamma <- matrix(nr = 1, ncol = 10)
## ##         gamma0 <- rnorm(n = 1, mean = spec@m0, sd = sqrt(spec@C0))
## ##         gamma[,1] <- G * gamma0 + W[,1]
## ##         for (i in 2:10)
## ##             gamma[,i] <- G * gamma[,i-1] + W[,i]
## ##         ans.expected <- gamma[1,] - mean(gamma[1,])
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ## })

## ## test_that("fakeBeta works with SpecPolyComponentSeasonal", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     fakeDLMErrors <- demest:::fakeDLMErrors
## ##     rmvnorm1 <- demest:::rmvnorm1
## ##     tau <- runif(1, 0.01, 2)
## ##     m0 <- rnorm(4)
## ##     C0 <- diag(runif(4))
## ##     for (seed in seq_len(n.test)) {
## ##         set.seed(seed)
## ##         spec <- new("SpecPolyComponentSeasonal",
## ##                     q = 4L,
## ##                     priorsW = list(new("SpecPriorVarDLMNormKnown", tau = tau),
## ##                         new("SpecPriorVarDLMZero"),
## ##                         new("SpecPriorVarDLMZero"),
## ##                         new("SpecPriorVarDLMZero")),
## ##                     m0 = m0,
## ##                     C0 = C0)
## ##         seed <- 1
## ##         metadata <- new("MetaData",
## ##                         nms = "age",
## ##                         dimtypes = "age",
## ##                         DimScales = list(new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         G <- rbind(c(1, 1, 1, 1),
## ##                    c(-1, 0, 0, 0),
## ##                    c(0, -1, 0, 0),
## ##                    c(0, 0, -1, 0))
## ##         W <- rbind(fakeDLMErrors(spec@priorsW[[1]], J = 10L),
## ##                    fakeDLMErrors(spec@priorsW[[2]], J = 10L),
## ##                    fakeDLMErrors(spec@priorsW[[3]], J = 10L),
## ##                    fakeDLMErrors(spec@priorsW[[4]], J = 10L))
## ##         gamma <- matrix(nr = 4, ncol = 10)
## ##         gamma0 <- rmvnorm1(mean = m0, var = C0, useC = TRUE)
## ##         gamma[,1] <- G %*% gamma0 + W[,1]
## ##         for (i in 2:10)
## ##             gamma[,i] <- G %*% gamma[,i-1] + W[,i]
## ##         ans.expected <- gamma[1,] - mean(gamma[1,])
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ## })

## ## test_that("fakeBeta works with SpecAR10", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     fakeDLMErrors <- demest:::fakeDLMErrors
## ##     sweepAllMargins <- demest:::sweepAllMargins
## ##     for (seed in seq_len(n.test)) {
## ##         ## dim = c(3, 10); along = 2
## ##         set.seed(seed)
## ##         spec <- AR1(coef = 0.5, sdObs = 0.4, sdTrend = 0.1)
## ##         metadata <- new("MetaData",
## ##                         nms = c("region", "time"),
## ##                         dimtypes = c("state", "time"),
## ##                         DimScales = list(new("Categories",
## ##                             dimvalues = c("a", "b", "c")),
## ##                             new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         gamma <- matrix(nr = 3, ncol = 11)
## ##         gamma[,1] <- rnorm(n = 3, mean = spec@m0, sd = sqrt(spec@C0))
## ##         for (j in 1:10)
## ##             gamma[,j+1] <- 0.5 * gamma[,j] + rnorm(n=3, sd = 0.1)
## ##         ans.expected <- gamma[,-1] + rnorm(n = 30, sd = 0.4)
## ##         ans.expected <- as.double(sweepAllMargins(ans.expected))
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##         ## dim = c(5, 5, 5), along = 1
## ##         set.seed(seed)
## ##         spec <- AR1(coef = 0.2, sdObs = 0.3, sdTrend = 0.5, along = "age")
## ##         metadata <- new("MetaData",
## ##                         nms = c("age", "region", "time"),
## ##                         dimtypes = c("age", "state", "time"),
## ##                         DimScales = list(new("Points", dimvalues = 1:5),
## ##                             new("Categories", dimvalues = c("a", "b", "c", "d", "e")),
## ##                             new("Intervals", dimvalues = 0:5)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         gamma <- array(dim = c(6, 5, 5))
## ##         gamma[1,,] <- rnorm(n = 25, mean = spec@m0, sd = sqrt(spec@C0))
## ##         for (j in 1:5)
## ##             gamma[j+1,,] <- 0.2 * gamma[j,,] + rnorm(n=25, sd = 0.5)
## ##         ans.expected <- gamma[-1,,] + rnorm(n = 125, sd = 0.3)
## ##         ans.expected <- as.double(sweepAllMargins(ans.expected))
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ## })

## ## test_that("fakeBeta works with SpecAR11", {
## ##     fakeBeta <- demest:::fakeBeta
## ##     fakeDLMErrors <- demest:::fakeDLMErrors
## ##     sweepAllMargins <- demest:::sweepAllMargins
## ##     rmvnorm2 <- demest:::rmvnorm2
## ##     for (seed in seq_len(n.test)) {
## ##         ## dim = c(3, 10); along = 2
## ##         set.seed(seed)
## ##         spec <- AR1(integrated = TRUE, coef = 0.5,
## ##                     sdObs = 0.4, sdTrend = 0.1)
## ##         metadata <- new("MetaData",
## ##                         nms = c("region", "time"),
## ##                         dimtypes = c("state", "time"),
## ##                         DimScales = list(new("Categories",
## ##                             dimvalues = c("a", "b", "c")),
## ##                             new("Intervals", dimvalues = 0:10)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         gamma <- matrix(nr = 3, ncol = 11)
## ##         delta <- matrix(nr = 3, ncol = 11)
## ##         for (j in 1:3) {
## ##             tmp <- rmvnorm2(spec@m0, spec@C0)
## ##             gamma[j,1] <- tmp[1]
## ##             delta[j,1] <- tmp[2]
## ##         }
## ##         for (j in 1:10) {
## ##             delta[,j+1] <- 0.5 * delta[,j] + rnorm(n=3, sd = 0.1)
## ##             gamma[,j+1] <- gamma[,j] + delta[,j]
## ##         }
## ##         ans.expected <- gamma[,-1] + rnorm(n = 30, sd = 0.4)
## ##         ans.expected <- as.double(sweepAllMargins(ans.expected))
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##         ## dim = c(5, 5, 5), along = 1
## ##         set.seed(seed)
## ##         spec <- AR1(integrated = TRUE, coef = 0.2, sdObs = 0.3,
## ##                     sdTrend = 0.5, along = "age")
## ##         metadata <- new("MetaData",
## ##                         nms = c("age", "region", "time"),
## ##                         dimtypes = c("age", "state", "time"),
## ##                         DimScales = list(new("Points", dimvalues = 1:5),
## ##                             new("Categories", dimvalues = c("a", "b", "c", "d", "e")),
## ##                             new("Intervals", dimvalues = 0:5)))
## ##         set.seed(seed + 1)
## ##         ans.obtained <- fakeBeta(object = spec, metadata = metadata)
## ##         set.seed(seed + 1)
## ##         gamma <- array(dim = c(6, 5, 5))
## ##         delta <- array(dim = c(6, 5, 5))
## ##         for (j in 1:5) {
## ##             for (k in 1:5) {
## ##                 tmp <- rmvnorm2(spec@m0, spec@C0)
## ##                 gamma[1,k,j] <- tmp[1]
## ##                 delta[1,k,j] <- tmp[2]
## ##             }
## ##         }
## ##         for (j in 1:5) {
## ##             delta[j+1,,] <- 0.2 * delta[j,,] + rnorm(n=25, sd = 0.5)
## ##             gamma[j+1,,] <- gamma[j,,] + delta[j,,]
## ##         }
## ##         ans.expected <- gamma[-1,,] + rnorm(n = 125, sd = 0.3)
## ##         ans.expected <- as.double(sweepAllMargins(ans.expected))
## ##         if (test.identity)
## ##             expect_identical(ans.obtained, ans.expected)
## ##         else
## ##             expect_equal(ans.obtained, ans.expected)
## ##     }
## ## })          

