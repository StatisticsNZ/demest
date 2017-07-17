
context("Prior-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## betaIsEstimated ####################################################################

test_that("betaIsEstimated works in default case", {
    betaIsEstimated <- demest:::betaIsEstimated
    x <- new("ExchFixed")
    expect_true(betaIsEstimated(x))
})

test_that("betaIsEstimated works with Zero prior", {
    betaIsEstimated <- demest:::betaIsEstimated
    x <- new("Zero")
    expect_false(betaIsEstimated(x))
})


## fakeBeta ###########################################################################

test_that("fakeBeta works with ExchFixed", {
    fakeBeta <- demest:::fakeBeta
    initialPrior <- demest:::initialPrior
    ## one dimension; no 'sd' supplied
    spec <- ExchFixed()
    beta <- rnorm(3)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    set.seed(1)
    ans.obtained <- fakeBeta(object = prior,
                             metadata = metadata)
    set.seed(1)
    ans.expected <- rnorm(3, sd = prior@tau@.Data)
    ans.expected <- ans.expected - mean(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## two dimensions; sd supplied
    spec <- ExchFixed(sd = 0.5)
    beta <- rnorm(15)
    metadata <- new("MetaData",
                    nms = c("region", "age"),
                    dimtypes = c("state", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Intervals", dimvalues = 0:5)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    set.seed(1)
    ans.obtained <- fakeBeta(object = prior, metadata = metadata)
    set.seed(1)
    ans.expected <- array(rnorm(15, sd = 0.5), dim = c(3, 5))
    ans.expected <- ans.expected - mean(ans.expected)
    ans.expected <- ans.expected - rowMeans(ans.expected)
    ans.expected <- ans.expected - rep(colMeans(ans.expected), each = 3)
    ans.expected <- as.double(ans.expected)
    expect_equal(ans.obtained, ans.expected)
})

test_that("fakeBeta works with ExchNormZero", {
    fakeBeta <- demest:::fakeBeta
    initialPrior <- demest:::initialPrior
    ## one dimension
    spec <- Exch()
    beta <- rnorm(3)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    set.seed(1)
    ans.obtained <- fakeBeta(object = prior,
                             metadata = metadata)
    set.seed(1)
    ans.expected <- rnorm(3, sd = prior@tau@.Data)
    ans.expected <- ans.expected - mean(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## two dimensions
    spec <- Exch()
    beta <- rnorm(15)
    metadata <- new("MetaData",
                    nms = c("region", "age"),
                    dimtypes = c("state", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Intervals", dimvalues = 0:5)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    set.seed(1)
    ans.obtained <- fakeBeta(object = prior, metadata = metadata)
    set.seed(1)
    ans.expected <- array(rnorm(15, sd = prior@tau@.Data), dim = c(3, 5))
    ans.expected <- ans.expected - mean(ans.expected)
    ans.expected <- ans.expected - rowMeans(ans.expected)
    ans.expected <- ans.expected - rep(colMeans(ans.expected), each = 3)
    ans.expected <- as.double(ans.expected)
    expect_equal(ans.obtained, ans.expected)
})



## makeDescriptor ####################################################################


## makeOutputPrior ###################################################################

test_that("makeOutputPrior works with ExchFixed", {
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(1)
    metadata <- NULL
    sY <- NULL
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = sY)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(scaleError = Skeleton(first = 3L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPrior works with ExchCovZero", {
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
                         scaleError = Skeleton(first = 6L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(scaleError = Skeleton(first = 3L))
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
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat, data = data),
                 error = Error(robust = TRUE))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
                         scaleError = Skeleton(first = 6L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         scaleError = Skeleton(first = 16L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 16L,
                             lastSeason = 59L,
                             iAlong = 1L,
                             nSeason = 4L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonSeasonDLM",
                             first = 16L,
                             last = 59L,
                             indicesShow = seq.int(5L, 41L, 4L),
                             nSeason = 4L,
                             iAlong = 1L,
                             metadata = metadata),
                         scaleSeason = Skeleton(first = 60L),
                         scaleError = Skeleton(first = 61L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         scaleError = Skeleton(first = 73L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         scaleError = Skeleton(first = 73L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 0L,
                             lastSeason = 0L,
                             iAlong = 1L,
                             nSeason = 1L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         coef = new("SkeletonCovariates",
                             first = 16L,
                             last = 18L,
                             metadata = metadata.coef),
                         scaleError = Skeleton(first = 19L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 31L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 31L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 16L,
                             lastSeason = 59L,
                             indicesShow = 2:11,
                             iAlong = 1L,
                             nSeason = 4L,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonSeasonDLM",
                             first = 16L,
                             last = 59L,
                             indicesShow = seq.int(5L, 41L, 4L),
                             nSeason = 4L,
                             iAlong = 1L,
                             metadata = metadata),
                         scaleSeason = Skeleton(first = 60L),
                         coef = new("SkeletonCovariates",
                             first = 61L,
                             last = 63L,
                             metadata = metadata.coef),
                         scaleError = Skeleton(first = 64L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 76L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 76L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 0L,
                             lastSeason = 0L,
                             indicesShow = 2:11,
                             iAlong = 1L,
                             nSeason = 1L,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         scaleError = Skeleton(first = 16L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 0L,
                             lastSeason = 0L,
                             nSeason = 1L,
                             iAlong = 1L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                             first = 15L,
                             last = 25L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L))
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 0L,
                             lastSeason = 0L,
                             nSeason = 1L,
                             iAlong = 1L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                             first = 15L,
                             last = 25L,
                             indicesShow = 2:11,
                             metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         scaleError = Skeleton(first = 28L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 16L,
                             lastSeason = 59L,
                             indicesShow = 2:11,
                             iAlong = 1L,
                             nSeason = 4L,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonSeasonDLM",
                             first = 16L,
                             last = 59L,
                             indicesShow = seq.int(5L, 41L, 4L),
                             iAlong = 1L,
                             nSeason = 4L,
                             metadata = metadata),
                         scaleSeason = Skeleton(first = 60L),
                         scaleError = Skeleton(first = 61L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         scaleError = Skeleton(first = 73L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         scaleError = Skeleton(first = 73L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 0L,
                             lastSeason = 0L,
                             indicesShow = 2:11,
                             iAlong = 1L,
                             nSeason = 1L,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         coef = new("SkeletonCovariates",
                             first = 16L,
                             last = 18L,
                             metadata = metadata.coef),
                         scaleError = Skeleton(first = 19L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 31L))
    expect_identical(ans.obtained, ans.expected)
    ## no level
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 0L,
                                     lastSeason = 0L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 1L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         coef = new("SkeletonCovariates",
                                    first = 28L,
                                    last = 30L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 31L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                             first = 3L,
                             last = 13L,
                             firstSeason = 16L,
                             lastSeason = 59L,
                             indicesShow = 2:11,
                             iAlong = 1L,
                             nSeason = 4L,
                             metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         damp = Skeleton(first = 15L),
                         season = new("SkeletonSeasonDLM",
                             first = 16L,
                             last = 59L,
                             indicesShow = seq.int(5L, 41L, 4L),
                             nSeason = 4L,
                             iAlong = 1L,
                             metadata = metadata),
                         scaleSeason = Skeleton(first = 60L),
                         coef = new("SkeletonCovariates",
                             first = 61L,
                             last = 63L,
                             metadata = metadata.coef),
                         scaleError = Skeleton(first = 64L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 76L))
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
                          sY = NULL)
    ans.obtained <- makeOutputPrior(prior = prior,
                                    metadata = metadata,
                                    pos = 3L)
    metadata.coef <- new("MetaData",
                         nms = "coef",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = c("income", "catb"))))
    ans.expected <- list(level = new("SkeletonLevelDLM",
                                     first = 3L,
                                     last = 13L,
                                     firstSeason = 28L,
                                     lastSeason = 71L,
                                     indicesShow = 2:11,
                                     iAlong = 1L,
                                     nSeason = 4L,
                                     metadata = metadata),
                         scaleLevel = Skeleton(first = 14L),
                         trend = new("SkeletonTrendDLM",
                                     first = 15L,
                                     last = 25L,
                                     indicesShow = 2:11,
                                     metadata = metadata),
                         scaleTrend = Skeleton(first = 26L),
                         damp = Skeleton(first = 27L),
                         season = new("SkeletonSeasonDLM",
                                      first = 28L,
                                      last = 71L,
                                      indicesShow = seq.int(5L, 41L, 4L),
                                      iAlong = 1L,
                                      nSeason = 4L,
                                      metadata = metadata),
                         scaleSeason = Skeleton(first = 72L),
                         coef = new("SkeletonCovariates",
                                    first = 73L,
                                    last = 75L,
                                    metadata = metadata.coef),
                         scaleError = Skeleton(first = 76L))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    spec <- Mix(weights = Weights(mean = -10))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
                         level1AR = Skeleton(metadata = metadata.wt,
                                             first = 402L),
                         scale1AR = Skeleton(first = 502L),
                         level2AR = Skeleton(metadata = metadata.wt,
                                             first = 503L),
                         meanAR = Skeleton(first = 603L),
                         coefAR = Skeleton(first = 604L),
                         scale2AR = Skeleton(first = 605L),
                         scaleError = Skeleton(first = 606L))
    expect_identical(ans.obtained, ans.expected)
})


          
## predictPrior ######################################################################


## ExchFixed

test_that("predictPrior works with ExchFixed", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchFixed", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchNormZero", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.obtained <- predictPrior(prior)
    ans.expected <- prior
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchNormCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    set.seed(1)
    ans.obtained <- predictPrior(prior)
    set.seed(1)
    ans.expected <- predictUBeta(prior)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictPrior give same answer with ExchRobustCov", {
    predictPrior <- demest:::predictPrior
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    metadata.old <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata.old,
                              sY = NULL)
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L)
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
    spec <- Mix()
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    ans.R <- predictPrior(prior, useC = FALSE)
    ans.C.generic <- predictPrior(prior, useC = TRUE, useSpecific = FALSE)
    ans.C.specific <- predictPrior(prior, useC = TRUE, useSpecific = TRUE)
    expect_identical(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})



## transferParamPrior ################################################################

## Exch

test_that("transferParamPrior works with ExchNormZero", {
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    covariates <- Covariates(formula = formula,
                             data = data,
                             contrastsArg = contrastsArg)
    spec <- Exch(covariates = covariates, error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior.old,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL)
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
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L)
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
    prior.old <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    prior.new <- initialPriorPredict(prior.old,
                                     metadata = metadata.new,
                                     name = "time:reg:age",
                                     along = 1L)
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
                     c("level", "scaleLevel",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level", 
                       "trend",
                       "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroWithSeason")
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovNoSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovWithSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroNoSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroWithSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovNoSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovWithSeason")
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level",
                       "scaleLevel",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- FALSE
    expect_identical(whereEstimated(x),
                     c("level",
                       "trend",
                       "scaleTrend",
                       "damp",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "scaleError"))
    x <- new("DLMNoTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendNormZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendNormCovWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "scaleError"))
    x <- new("DLMNoTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMWithTrendRobustZeroWithSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "season",
                       "scaleSeason",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovNoSeason")
    x@phiKnown@.Data <- TRUE
    x@hasLevel@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "coef",
                       "scaleError"))
    x <- new("DLMNoTrendRobustCovWithSeason")
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("DLMWithTrendRobustCovWithSeason")
    x@hasLevel@.Data <- TRUE
    x@phiKnown@.Data <- TRUE
    expect_identical(whereEstimated(x),
                     c("level", "scaleLevel",
                       "trend", "scaleTrend",
                       "season",
                       "scaleSeason",
                       "coef",
                       "scaleError"))
    x <- new("MixNormZero")
    expect_identical(whereEstimated(x),
                     c("components",
                       "scaleComponents",
                       "weights",
                       "level1AR",
                       "scale1AR",
                       "level2AR",
                       "meanAR",
                       "coefAR",
                       "scale2AR",
                       "scaleError"))
    x <- new("Zero")
    expect_identical(whereEstimated(x),
                     NULL)
})


