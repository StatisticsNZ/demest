
context("helper-predict")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("checkDataPredict works", {
    checkDataPredict <- demest:::checkDataPredict
    expect_identical(checkDataPredict(NULL), NULL)
    ## has class 'list'
    expect_error(checkDataPredict("wrong"),
                 "'data' has class \"character\"")
    ## has names
    data <- list(data.frame(reg = letters, income = rnorm(26)),
                       data.frame(age = 10:14, eduation = rnorm(5)))
    expect_error(checkDataPredict(data),
                 "'data' does not have names")
    ## names have no missing values
    data <- list(reg = data.frame(reg = letters, income = rnorm(26)),
                       age = data.frame(age = 10:14, eduation = rnorm(5)))
    names(data)[2] <- NA
    expect_error(checkDataPredict(data),
                 "names for 'data' have missing values")
    ## names have no blanks
    data <- list(reg = data.frame(reg = letters, income = rnorm(26)),
                       age = data.frame(age = 10:14, eduation = rnorm(5)))
    names(data)[2] <- ""
    expect_error(checkDataPredict(data),
                 "names for 'data' have blanks")
    ## names have no duplicates
    data <- list(reg = data.frame(reg = letters, income = rnorm(26)),
                       age = data.frame(age = 10:14, eduation = rnorm(5)))
    names(data)[2] <- "reg"
    expect_error(checkDataPredict(data),
                 "names for 'data' have duplicates")
    ## all elements are data.frames
    data <- list(reg = data.frame(reg = letters, income = rnorm(26)),
                       age = "wrong")
    expect_error(checkDataPredict(data),
                 "item \"age\" from 'data' has class \"character\"")
})

test_that("checkEstAndPredFilenamesDifferent works", {
    checkEstAndPredFilenamesDifferent <- demest:::checkEstAndPredFilenamesDifferent
    ans.obtained <- checkEstAndPredFilenamesDifferent(filenameEst = "model.est",
                                                      filenamePred = "model.pred")
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkEstAndPredFilenamesDifferent(filenameEst = "model.est",
                                                   filenamePred = "model.est"),
                 "'filenameEst' and 'filenamePred' are identical")
})

test_that("initialModelPredictHelper works", {
    initialModelPredictHelper <- demest:::initialModelPredictHelper
    initialModel <- demest:::initialModel
    makeMetadataPredict <- demest:::makeMetadataPredict
    initialPriorPredict <- demest:::initialPriorPredict
    BetaIterator <- demest:::BetaIterator
    makeOffsetsBetas <- demest:::makeOffsetsBetas
    makeOffsetsPriorsBetas <- demest:::makeOffsetsPriorsBetas
    makeOffsetsSigma <- demest:::makeOffsetsSigma
    exposure <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                             dim = c(5, 4),
                             dimnames = list(time = 2001:2005, region = 1:4)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.7),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod <- initialModel(spec, y = y, exposure = exposure) 
    strucZeroArray <- Counts(array(1L,
                                   dim = c(4, 4),
                                   dimnames = list(time = 2006:2009, region = 1:4)),
                             dimscales = c(time = "Intervals"))
    set.seed(1)
    ans.obtained <- initialModelPredictHelper(model = mod,
                                              along = 1L,
                                              labels = NULL,
                                              n = 4,
                                              offsetModel = 1L,
                                              covariates = NULL)
    set.seed(1)
    ans.expected <- list(theta = rep(mean(mod@theta), times = 16),
                         thetaTransformed = rep(0, 16),
                         mu = rep(0, 16),
                         metadataY = new("MetaData",
                                         nms = c("time", "region"),
                                         dimtypes = c("time", "state"),
                                         DimScales = list(new("Intervals",
                                                              dimvalues = as.numeric(2005:2009)),
                                                          new("Categories", dimvalues = as.character(1:4)))),
                         cellInLik = rep(FALSE, 16),
                         betas = list(mod@betas[[1]], rep(0, 4), mod@betas[[3]]),
                         meansBetas = list(0, rep(0, 4), rep(0, 4)),
                         variancesBetas = list(0, rep(0, 4), rep(0, 4)),
                         strucZeroArray = strucZeroArray,
                         priorsBetas = list(new("TimeInvariant", J = new("Length", 1L),
                                                isSaturated = new("LogicalFlag", FALSE)),
                                            initialPriorPredict(prior = mod@priorsBetas[[2]],
                                                                data = NULL,
                                                                metadata = new("MetaData",
                                                                               nms = "time",
                                                                               dimtypes = "time",
                                                                               DimScales = list(new("Intervals",
                                                                                                    dimvalues =
                                                                                                        as.numeric(2005:2009)))),
                                                                name = "time",
                                                                along = 1L,
                                                                margin = 1L,
                                                                strucZeroArray = strucZeroArray),
                                            new("TimeInvariant", J = new("Length", 4L),
                                                isSaturated = new("LogicalFlag", FALSE))),
                         betaEqualsMean = c(TRUE, FALSE, TRUE),
                         iteratorBetas = BetaIterator(dim = c(4L, 4L),
                                                      margins = c(0L, 1L, 2L)),
                         dims = list(0L, 4L, 4L),
                         betaIsPredicted = c(FALSE, TRUE, FALSE),
                         offsetsBetas = makeOffsetsBetas(model = mod,
                                                         offsetModel = 1L),
                         offsetsPriorsBetas = makeOffsetsPriorsBetas(model = mod,
                                                                     offsetModel = 1L),
                         offsetsSigma = makeOffsetsSigma(model = mod,
                                                         offsetModel = 1L),
                         iMethodModel = 109L)
    expect_identical(ans.obtained, ans.expected)
})
                         
test_that("lengthValues works", {
    lengthValues <- demest:::lengthValues
    initialPrior <- demest:::initialPrior
    ## numeric
    x <- 1:5
    ans.obtained <- lengthValues(x)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ## list
    x <- list(1:5, 1:3, 1, numeric())
    ans.obtained <- lengthValues(x)
    ans.expected <- 9L
    expect_identical(ans.obtained, ans.expected)
    ## ExchNormZero prior
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
    ans.obtained <- lengthValues(prior)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ## ExchRobustZero prior
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- lengthValues(prior)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ## DLMWithTrendNormZeroWithSeason
    season <- Season(n = 4)
    spec <- DLM(season = season)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray = Counts(array(1L,
                                  dim = 10,
                                  dimnames = list(time = 1:10)),
                            dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- lengthValues(prior)
    ans.expected <- 11L + 11L + 4L*11L + 4L + 1L
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataPredict works with Points", {
    makeMetadataPredict <- demest:::makeMetadataPredict
    metadata.old <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 1:5)))
    along <- 2L
    labels <- as.character(6:10)
    ans.obtained <- makeMetadataPredict(metadata.old, along = along, labels = labels,
                                        n = NULL)
    ans.expected <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = as.numeric(6:10))))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMetadataPredict(metadata.old, along = along, labels = NULL,
                                        n = 5)
    ans.expected <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 6:10)))
    expect_identical(ans.obtained, ans.expected)
    expect_warning(makeMetadataPredict(metadata.old, along = along,
                                       labels = labels, n = 5),
                   "'n' ignored when 'labels' provided")
    expect_error(makeMetadataPredict(metadata.old, along = along, labels = NULL,
                                     n = NULL),
                 "must supply 'labels' or 'n' argument")
})

test_that("makeMetadataPredict works with Intervals", {
    makeMetadataPredict <- demest:::makeMetadataPredict
    metadata.old <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Intervals", dimvalues = 0:5)))
    along <- 2L
    labels <- as.character((-5):(-1))
    ans.obtained <- makeMetadataPredict(metadata.old, along = along,
                                        labels = labels, n = NULL)
    ans.expected <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Intervals", dimvalues = as.numeric((-5):0))))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMetadataPredict(metadata.old, along = along,
                                        labels = NULL, n = -5)
    ans.expected <- new("MetaData",
                        nms = c("sex", "time"),
                        dimtypes = c("sex", "time"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Intervals", dimvalues = (-5):0)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataPredict works with Categories", {
    makeMetadataPredict <- demest:::makeMetadataPredict
    metadata.old <- new("MetaData",
                        nms = c("sex", "region"),
                        dimtypes = c("sex", "state"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Categories", dimvalues = c("a", "b" ,"c"))))
    along <- 2L
    labels <- c("d", "e", "f")
    ans.obtained <- makeMetadataPredict(metadata.old, along = along, labels = labels,
                                        n = NULL)
    ans.expected <- new("MetaData",
                        nms = c("sex", "region"),
                        dimtypes = c("sex", "state"),
                        DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Categories", dimvalues = c("d", "e", "f"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOffsetsBetas works", {
    makeOffsetsBetas <- demest:::makeOffsetsBetas
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                             dim = c(5, 4),
                             dimnames = list(time = 2001:2005, region = 1:4)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.7),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOffsetsBetas(mod, offsetModel = 1L)
    ans.expected <- list(new("Offsets", c(23L, 23L)),
                         new("Offsets", c(24L, 28L)),
                         new("Offsets", c(29L, 32L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOffsetsPriorsBetas works", {
    makeOffsetsPriorsBetas <- demest:::makeOffsetsPriorsBetas
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.integer(runif(n = 20, min = 5, max = 100)),
                             dim = c(5, 4),
                             dimnames = list(time = 2001:2005, region = 1:4)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.7),
                      dim = c(5, 4),
                      dimnames = list(time = 2001:2005, region = 1:4)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ time + region))
    mod <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- makeOffsetsPriorsBetas(mod, offsetModel = 1L)
    ans.expected <- list(NULL,
                         new("Offsets", c(34L, 49L)),
                         new("Offsets", c(50L, 50L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOffsetsSigma works", {
    makeOffsetsSigma <- demest:::makeOffsetsSigma
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    set.seed(1)
    y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Poisson(mean ~ sex + age, useExpose = FALSE))
    model <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- makeOffsetsSigma(model, offsetModel = 1L)
    offset <- 20L + 1L + 1L + sum(sapply(model@betas, length)) + 1L
    ans.expected <- new("Offsets", c(offset, offset))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOffsetsVarsigma works", {
    makeOffsetsVarsigma <- demest:::makeOffsetsVarsigma
    extractValues <- demest:::extractValues
    initialModel <- demest:::initialModel
    y <- Counts(array(as.double(rnorm(n = 20)),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    w <- Counts(array(as.double(runif(n = 20)),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Normal(mean ~ sex + age))
    model <- initialModel(spec, y = y, weights = w)
    ans.obtained <- makeOffsetsVarsigma(model, offsetModel = 1L)
    offset <- 20L + 1L + 1L
    ans.expected <- new("Offsets", c(offset, offset))
    expect_identical(ans.obtained, ans.expected)
})

test_that("extrapolateStrucZeroArray works", {
    extrapolateStrucZeroArray <- demest:::extrapolateStrucZeroArray
    strucZeroArray <- Counts(array(1:0,
                                   dim = c(2, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = c(2000, 2005, 2010))))
    ans.obtained <- extrapolateStrucZeroArray(strucZeroArray,
                                              along = "time",
                                              labels = c("2015", "2020"))
    ans.expected <- Counts(array(1:0,
                                 dim = c(2, 2),
                                 dimnames = list(sex = c("f", "m"),
                                                 time = c(2015, 2020))))
    expect_identical(ans.obtained, ans.expected)                             
})




test_that("predictAlphaLN2 works", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    predictAlphaLN2 <- demest:::predictAlphaLN2
    constraint <- Values(array(c(NA, -1L, 0L, 1L),
                               dim = c(2, 2),
                               dimnames = list(age = c("0-39", "40+"),
                                               sex = c("Female", "Male"))))
    y <- Counts(array(10L,
                      dim = c(2, 4, 3),
                      dimnames = c(list(sex = c("Female", "Male"),
                                        age = c("0-19", "20-39", "40-59", "60+"),
                                        time = c("2000", "2010", "2020")))))
    exposure <- 2L * y
    spec <- Model(y ~ LN2(constraint = constraint))
    mod.est <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(mod.est,
                             along = 3L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = NULL)
    has.non.zero <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- predictAlphaLN2(x)
        set.seed(seed)
        ans.expected <- x
        not.zero <- is.na(x@constraintLN2@.Data) | (x@constraintLN2@.Data != 0L)
        if (any(not.zero))
            has.non.zero <- TRUE
        pos <- !is.na(x@constraintLN2@.Data) & (x@constraintLN2@.Data > 0L)
        neg <- !is.na(x@constraintLN2@.Data) & (x@constraintLN2@.Data < 0L)
        ans.expected@alphaLN2@.Data[not.zero] <- rnorm(n = sum(not.zero),
                                                       mean = 0,
                                                       sd = x@sigma@.Data)
        ans.expected@alphaLN2@.Data[pos] <- abs(ans.expected@alphaLN2@.Data[pos])
        ans.expected@alphaLN2@.Data[neg] <- -1 * abs(ans.expected@alphaLN2@.Data[neg])
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
    if (!has.non.zero)
        warning("no non-zero entries")
})

test_that("R and C versions of predictAlphaLN2 give same answer", {
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    predictAlphaLN2 <- demest:::predictAlphaLN2
    constraint <- Values(array(c(NA, -1L, 0L, 1L),
                               dim = c(2, 2),
                               dimnames = list(age = c("0-39", "40+"),
                                               sex = c("Female", "Male"))))
    y <- Counts(array(10L,
                      dim = c(2, 4, 3),
                      dimnames = c(list(sex = c("Female", "Male"),
                                        age = c("0-19", "20-39", "40-59", "60+"),
                                        time = c("2000", "2010", "2020")))))
    exposure <- 2L * y
    spec <- Model(y ~ LN2(constraint = constraint))
    mod.est <- initialModel(spec, y = y, exposure = exposure)
    x <- initialModelPredict(mod.est,
                             along = 3L,
                             labels = NULL,
                             n = 4L,
                             offsetModel = 1L,
                             covariates = NULL,
                             aggregate = NULL,
                             lower = NULL,
                             upper = NULL)
    has.non.zero <- FALSE
    for (seed in seq_len(2 * n.test)) {
        set.seed(seed)
        ans.R <- predictAlphaLN2(x, useC = FALSE)
        set.seed(seed)
        ans.C <- predictAlphaLN2(x, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!has.non.zero)
        warning("no non-zero entries")
})

test_that("predictAlphaDLMNoTrend works", {
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray = Counts(array(1L,
                                  dim = 10,
                                  dimnames = list(time = 1:10)),
                            dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    prior@alphaDLM@.Data[1] <- rnorm(1)
    set.seed(1)
    ans.obtained <- predictAlphaDLMNoTrend(prior)
    set.seed(1)
    ans.expected <- prior
    phi <- ans.expected@phi
    for (i in seq_len(10)) {
        ans.expected@alphaDLM@.Data[i+1] <- rnorm(n = 1,
                                                  mean = phi * ans.expected@alphaDLM@.Data[i],
                                                  sd = ans.expected@omegaAlpha@.Data)
    }
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictAlphaDLMNoTrend give same answer", {
    predictAlphaDLMNoTrend <- demest:::predictAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray = Counts(array(1L,
                                      dim = 10,
                                      dimnames = list(time = 1:10)),
                                dimscales = c(time = "Points"))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(seed+1)
        ans.R <- predictAlphaDLMNoTrend(prior, useC = FALSE)
        set.seed(seed+1)
        ans.C <- predictAlphaDLMNoTrend(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("predictAlphaDeltaDLMWithTrend works", {
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    ## has level
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                  dim = 10,
                                  dimnames = list(time = 1:10)),
                            dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    prior@alphaDLM@.Data[1] <- rnorm(1)
    prior@deltaDLM@.Data[1] <- rnorm(1)
    set.seed(1)
    ans.obtained <- predictAlphaDeltaDLMWithTrend(prior)
    set.seed(1)
    ans.expected <- prior
    phi <- ans.expected@phi
    for (i in seq_len(10)) {
        ans.expected@deltaDLM@.Data[i+1] <- rnorm(n = 1,
                                                  mean = phi * ans.expected@deltaDLM@.Data[i],
                                                  sd = ans.expected@omegaDelta@.Data)
        ans.expected@alphaDLM@.Data[i+1] <- rnorm(n = 1,
                                                  mean = ans.expected@alphaDLM@.Data[i] +
                                                      ans.expected@deltaDLM@.Data[i],
                                                  sd = ans.expected@omegaAlpha@.Data)
    }
    expect_identical(ans.obtained, ans.expected)
    ## no level
    spec <- DLM(level = NULL)
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    prior@alphaDLM@.Data[1] <- rnorm(1)
    prior@deltaDLM@.Data[1] <- rnorm(1)
    set.seed(1)
    ans.obtained <- predictAlphaDeltaDLMWithTrend(prior)
    set.seed(1)
    ans.expected <- prior
    phi <- ans.expected@phi
    for (i in seq_len(10)) {
        ans.expected@deltaDLM@.Data[i+1] <- rnorm(n = 1,
                                                  mean = phi * ans.expected@deltaDLM@.Data[i],
                                                  sd = ans.expected@omegaDelta@.Data)
        ans.expected@alphaDLM@.Data[i+1] <- ans.expected@alphaDLM@.Data[i] + ans.expected@deltaDLM@.Data[i]
    }
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictAlphaDeltaDLMWithTrend give same answer", {
    predictAlphaDeltaDLMWithTrend <- demest:::predictAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    ## has level
    spec <- DLM()
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(seed+1)
        ans.R <- predictAlphaDeltaDLMWithTrend(prior, useC = FALSE)
        set.seed(seed+1)
        ans.C <- predictAlphaDeltaDLMWithTrend(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## no level
    spec <- DLM(level = NULL)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 1:10)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 10,
                                       dimnames = list(time = 1:10)),
                                 dimscales = c(time = "Points"))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(seed+1)
        ans.R <- predictAlphaDeltaDLMWithTrend(prior, useC = FALSE)
        set.seed(seed+1)
        ans.C <- predictAlphaDeltaDLMWithTrend(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("predictBeta gives valid answer", {
    predictBeta <- demest:::predictBeta
    initialPrior <- demest:::initialPrior
    ## ExchFixed
    spec <- ExchFixed()
    beta <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("o", "ns"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = 2,
                                   dimnames = list(region = c("o", "ns"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- rnorm(n = 2, sd = prior@tau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## Zero
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
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- rep(0, 10)
    expect_identical(ans.obtained, ans.expected)
    ## Known
    mean <- ValuesOne(1:10, labels = 2000:2009, name = "time", dimscale = "Points")
    spec <- Known(mean)
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2005:2009)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2005:2009)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- as.numeric(mean@.Data[6:10])
    expect_identical(ans.obtained, ans.expected)
    ## KnownCertain
    mean <- ValuesOne(1:10, labels = 2000:2009, name = "time", dimscale = "Points")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2005:2009)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2005:2009)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- rnorm(n = 5, mean = mean@.Data[6:10], sd = 1)
    expect_identical(ans.obtained, ans.expected)
    ## ExchNormZero
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
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- rnorm(n = 10, sd = prior@tau@.Data)
    expect_identical(ans.obtained, ans.expected)
    ## ExchRobustZero
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictBeta(prior)
    set.seed(1)
    ans.expected <- rnorm(n = 10, mean = 0, sd = sqrt(prior@UBeta@.Data))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictBeta give same answer", {
    predictBeta <- demest:::predictBeta
    initialPrior <- demest:::initialPrior
    ## ExchFixed
    spec <- ExchFixed()
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        beta <- rnorm(2)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = c("o", "ns"))))
        strucZeroArray <- Counts(array(1L,
                                       dim = 2,
                                       dimnames = list(region = c("o", "ns"))))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(seed + 1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- predictBeta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## Zero
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
        set.seed(1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(1)
        ans.C <- predictBeta(prior, useC = FALSE)
        expect_identical(ans.R, ans.C)
        ## Known
        mean <- ValuesOne(1:10, labels = 2000:2009, name = "time", dimscale = "Points")
        spec <- Known(mean)
        beta <- rnorm(5)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2005:2009)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 5,
                                       dimnames = list(time = 2005:2009)),
                                 dimscales = c(time = "Points"))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(1)
        ans.C <- predictBeta(prior, useC = FALSE)
        expect_identical(ans.R, ans.C)
        ## KnownCertain
        mean <- ValuesOne(1:10, labels = 2000:2009, name = "time", dimscale = "Points")
        spec <- Known(mean, sd = 1)
        beta <- rnorm(5)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2005:2009)))
        strucZeroArray <- Counts(array(1L,
                                       dim = 5,
                                       dimnames = list(time = 2005:2009)),
                                 dimscales = c(time = "Points"))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(1)
        ans.C <- predictBeta(prior, useC = FALSE)
        expect_identical(ans.R, ans.C)
        ## ExchNormZero
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
        set.seed(seed + 1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- predictBeta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## ExchRobustZero
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
                              isSaturated = FALSE,
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
        set.seed(seed + 1)
        ans.R <- predictBeta(prior, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- predictBeta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("predictBetas gives valid answer", {
    predictBetas <- demest:::predictBetas
    predictBeta <- demest:::predictBeta
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictBetas(model)
        set.seed(seed + 1)
        ans.expected <- model
        ans.expected@betas[[2]] <- predictBeta(ans.expected@priorsBetas[[2]])
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versons of predictBetas give same answer", {
    predictBetas <- demest:::predictBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictBetas(model, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- predictBetas(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("predictComponentWeightMix works", {
    predictComponentWeightMix <- demest:::predictComponentWeightMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    set.seed(1)
    ans.obtained <- predictComponentWeightMix(prior.new)
    set.seed(1)
    ans.expected <- prior.new
    lcw <- matrix(prior.new@levelComponentWeightMix@.Data,
                  nr = 20, nc = 10)
    cw <- matrix(nr = 20, nc = 10)
    omega <- prior.new@omegaComponentWeightMix@.Data
    for (j in 1:10) {
        for (i in 1:20) {
            cw[i,j] <- rnorm(n = 1,
                             mean = lcw[i,j],
                             sd = omega)
        }
    }
    ans.expected@componentWeightMix@.Data <- as.numeric(cw)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of predictComponentWeightMix give same answer", {
    predictComponentWeightMix <- demest:::predictComponentWeightMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    set.seed(1)
    ans.R <- predictComponentWeightMix(prior.new,
                                       useC = FALSE)
    set.seed(1)
    ans.C <- predictComponentWeightMix(prior.new,
                                       useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})


test_that("predictIndexClassMix works", {
    predictIndexClassMix <- demest:::predictIndexClassMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    prior.new@weightMix@.Data <- runif(n = 200, max = 0.1)
    set.seed(1)
    ans.obtained <- predictIndexClassMix(prior.new)
    expect_true(all(ans.obtained@indexClassMix %in% 1:10))
    expect_true(!all(ans.obtained@indexClassMix == prior.new@indexClassMix))
})

test_that("R and C versions of predictIndexClassMix give same answer", {
    predictIndexClassMix <- demest:::predictIndexClassMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    prior.new@weightMix@.Data <- runif(n = 200, max = 0.1)
    set.seed(1)
    ans.R <- predictIndexClassMix(prior.new,
                                  useC = FALSE)
    set.seed(1)
    ans.C <- predictIndexClassMix(prior.new,
                                  useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("predictLevelComponentWeightMix works", {
    predictLevelComponentWeightMix <- demest:::predictLevelComponentWeightMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    set.seed(1)
    ans.obtained <- predictLevelComponentWeightMix(prior.new)
    set.seed(1)
    ans.expected <- prior.new
    lcw <- matrix(nr = 21, nc = 10)
    lcw[1,] <- prior.new@levelComponentWeightOldMix@.Data
    phi <- prior.new@phiMix
    mu <- prior.new@meanLevelComponentWeightMix@.Data
    omega <- prior.new@omegaLevelComponentWeightMix@.Data
    for (j in 1:10) {
        for (i in 1:20) {
            lcw[i+1,j] <- rnorm(n = 1,
                                mean = mu + phi*lcw[i,j],
                                sd = omega)
        }
    }
    ans.expected@levelComponentWeightMix@.Data <- as.numeric(lcw[-1,])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of predictLevelComponentWeightMix give same answer", {
    predictLevelComponentWeightMix <- demest:::predictLevelComponentWeightMix
    transferParamPrior <- demest:::transferParamPrior
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    extractValues <- demest:::extractValues
    set.seed(100)
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
    prior.new <- transferParamPrior(prior = prior.new,
                                    values = extractValues(prior.old))
    set.seed(1)
    ans.R <- predictLevelComponentWeightMix(prior.new,
                                            useC = FALSE)
    set.seed(1)
    ans.C <- predictLevelComponentWeightMix(prior.new,
                                            useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("predictOneChain works", {
    predictOneChain <- demest:::predictOneChain
    estimateOneChain <- demest:::estimateOneChain
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictCombined <- demest:::predictCombined
    extractValues <- demest:::extractValues
    lengthValues <- demest:::lengthValues
    set.seed(100)
    exposure <- Counts(array(as.double(rpois(n = 30, lambda = 10)),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2,
                                 time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    combined.old.initial <- initialCombinedModel(spec,
                                                 y = y,
                                                 exposure = exposure,
                                                 weights = NULL)
    filename.old <- tempfile()
    combined.old <- estimateOneChain(combined.old.initial,
                                     tempfile = filename.old,
                                     seed = NULL,
                                     nBurnin = 0L,
                                     nSim = 3L,
                                     nUpdateMax = 200L,
                                     continuing = FALSE,
                                     nThin = 1L,
                                     nAttempt = 100L,
                                     useC = TRUE)
    combined.new.initial <- initialCombinedModelPredict(combined = combined.old,
                                                        along = 3L,
                                                        labels = c("2005", "2006"),
                                                        n = NULL,
                                                        covariates = NULL,
                                                        aggregate = NULL,
                                                        lower = NULL,
                                                        upper = NULL,
                                                        yIsCounts = TRUE)
    lengthIter <- lengthValues(combined.old)
    filename.new <- tempfile()
    set.seed(1)
    ans.obtained.obj <- predictOneChain(combined = combined.new.initial,
                                        tempfileOld = filename.old,
                                        tempfileNew = filename.new,
                                        lengthIter = lengthIter,
                                        nIteration = 3L,
                                        nUpdate = 1L,
                                        useC = FALSE)
    con <- file(filename.new, "rb")
    ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
    close(con)
    set.seed(1)
    ans.expected.file <- vector(mode = "list", length = 3)
    combined <- combined.new.initial
    for (i in 1:3) {
        combined <- predictCombined(combined,
                                    filename = filename.old,
                                    lengthIter = lengthIter,
                                    iteration = i,
                                    nUpdate = 1L)
        ans.expected.file[[i]] <- extractValues(combined)
    }
    ans.expected.file <- unlist(ans.expected.file)
    ans.expected.obj <- combined
    if (test.identity) {
        expect_identical(ans.obtained.obj, ans.expected.obj)
        expect_identical(ans.obtained.file, ans.expected.file)
    }
    else {
        expect_equal(ans.obtained.obj, ans.expected.obj)
        expect_equal(ans.obtained.file, ans.expected.file)
    }
})

test_that("predictPriorsBetas gives valid answer", {
    predictPriorsBetas <- demest:::predictPriorsBetas
    predictPrior <- demest:::predictPrior
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.obtained <- predictPriorsBetas(model)
        set.seed(seed + 1)
        ans.expected <- model
        ans.expected@priorsBetas[[2]] <- predictPrior(prior = ans.expected@priorsBetas[[2]])
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versons of predictPriorsBetas give same answer", {
    predictPriorsBetas <- demest:::predictPriorsBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure.est <- Counts(array(runif(n = 20, max = 50),
                                     dim = c(5, 4),
                                     dimnames = list(age = 0:4, region = letters[1:4])))
        exposure.pred <- Counts(array(as.double(NA),
                                      dim = c(10, 4),
                                      dimnames = list(age = 5:14, region = letters[1:4])))
        y.est <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure.est)),
                              dim = c(5, 4),
                              dimnames = list(age = 0:4, region = letters[1:4])))
        y.pred <- Counts(array(as.integer(NA),
                               dim = c(10, 4),
                               dimnames = list(age = 5:14, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y.est, exposure = exposure.est)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     labels = NULL,
                                     n = 10,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        set.seed(seed + 1)
        ans.R <- predictPriorsBetas(model, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- predictPriorsBetas(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("predictSeason works", {
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray = Counts(array(1L,
                                  dim = 10,
                                  dimnames = list(time = 1:10)),
                            dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    prior@s@.Data[[1]] <- rnorm(4)
    set.seed(1)
    ans.obtained <- predictSeason(prior)
    set.seed(1)
    ans.expected <- prior
    for (i in seq_len(10)) {
        ans.expected@s@.Data[[i+1]][1] <- rnorm(n = 1,
                                                mean = ans.expected@s@.Data[[i]][4],
                                                sd = ans.expected@omegaSeason@.Data)
        ans.expected@s@.Data[[i+1]][2:4] <- ans.expected@s@.Data[[i]][1:3]
    }
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictSeason give same answer", {
    predictSeason <- demest:::predictSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    strucZeroArray = Counts(array(1L,
                                  dim = 10,
                                  dimnames = list(time = 1:10)),
                            dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    prior@s@.Data[[1]] <- rnorm(4)
    set.seed(1)
    ans.R <- predictSeason(prior, useC = FALSE)
    set.seed(1)
    ans.C <- predictSeason(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})


test_that("predictUBeta gives valid answer", {
    predictUBeta <- demest:::predictUBeta
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(rep(1L, 9), 0L),
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.obtained <- predictUBeta(prior)
    set.seed(1)
    ans.expected <- prior
    ans.expected@UBeta@.Data[1:9] <- replicate(n = 9,
                                               rinvchisq1(df = ans.expected@nuBeta@.Data,
                                                          scale = ans.expected@tau@.Data^2))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of predictUBeta give same answer", {
    predictUBeta <- demest:::predictUBeta
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(rep(1L, 9), 0L),
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.R <- predictUBeta(prior, useC = FALSE)
    set.seed(1)
    ans.C <- predictUBeta(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})
    
test_that("splitFile works", {
    splitFile <- demest:::splitFile
    filename <- tempfile()
    nChain <- 3L
    nIteration <- 150L
    lengthIter <- 20L
    vals <- rnorm(n = nIteration * lengthIter)
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.result <- length(results)
    con <- file(filename, "wb")
    writeBin(size.result, con = con)
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    writeBin(vals, con = con)
    close(con)
    ans.obtained <- splitFile(filename = filename,
                              nChain = nChain,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
    ans.expected <- paste(filename, seq_len(nChain), sep = "_")
    expect_identical(ans.obtained, ans.expected)
    length.file <- nIteration/nChain * lengthIter
    for (i in seq_len(nChain)) {
        con <- file(paste(filename, i, sep = "_"), "rb")
        vals.i <- readBin(con = con, what = "double", n = 100000)
        close(con)
        idx <- 1:length.file + (i-1) * length.file
        expect_identical(vals.i, vals[idx])
    }
})

test_that("transferParamPriorsBetas gives valid answer", {
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    transferParamPrior <- demest:::transferParamPrior
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    lengthValues <- demest:::lengthValues
    extractValues <- demest:::extractValues
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region),
                      age ~ DLM(trend = NULL))
        model.old <- initialModel(spec, y = y, weights = weights)
        model.new <- initialModelPredict(model.old,
                                         along = 1L,
                                         n = 10L,
                                         labels = NULL,
                                         offsetModel = 1L,
                                         covariates = NULL,
                                         aggregate = NULL,
                                         lower = NULL,
                                         upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:1000)
        writeBin(data, con = con)
        close(con)
        ans.obtained <- transferParamPriorsBetas(model.new,
                                                 filename = filename,
                                                 lengthIter = lengthValues(model.old),
                                                 iteration = 1L,
                                                 useC = FALSE)
        ans.expected <- model.new
        ans.expected@priorsBetas[[2]] <- transferParamPrior(ans.expected@priorsBetas[[2]],
                                                            values = data[34:49])
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of transferParamPriorsBetas give same answer", {
    transferParamPriorsBetas <- demest:::transferParamPriorsBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    lengthValues <- demest:::lengthValues
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model.old <- initialModel(spec, y = y, weights = weights)
        model.new <- initialModelPredict(model.old,
                                         along = 1L,
                                         n = 10L,
                                         labels = NULL,
                                         offsetModel = 1L,
                                         covariates = NULL,
                                         aggregate = NULL,
                                         lower = NULL,
                                         upper = NULL)
        data <- as.double(1:1000)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        writeBin(data, con = con)
        close(con)
        ans.R <- transferParamPriorsBetas(model.new,
                                          filename = filename,
                                          lengthIter = lengthValues(model.old),
                                          iteration = 2L,
                                          useC = FALSE)
        ans.C <- transferParamPriorsBetas(model.new,
                                          filename = filename,
                                          lengthIter = lengthValues(model.old),
                                          iteration = 2L,
                                          useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("transferAlphaDelta0 works", {
    transferAlphaDelta0 <- demest:::transferAlphaDelta0
    AlongIterator <- demest:::AlongIterator
    ## state has dim c(4,11); offset = 1
    state <- numeric(length = 4 * 16)
    values <- rnorm(n = 100)
    offset <- 1L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.obtained <- transferAlphaDelta0(state = state,
                                        values = values,
                                        offset = offset,
                                        iteratorNew = iterNew,
                                        iteratorOld = iterOld)
    ans.expected <- matrix(state, nr = 4)
    ans.expected[,1] <- matrix(values[1:44], nr = 4)[,11]
    ans.expected <- as.numeric(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## state has dim c(4,11); offset = 20L
    state <- numeric(length = 4 * 16)
    values <- rnorm(n = 100)
    offset <- 20L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.obtained <- transferAlphaDelta0(state = state,
                                        values = values,
                                        offset = offset,
                                        iteratorNew = iterNew,
                                        iteratorOld = iterOld)
    ans.expected <- matrix(state, nr = 4)
    ans.expected[,1] <- matrix(values[20:63], nr = 4)[,11]
    ans.expected <- as.numeric(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## state has dim 2:4
    state <- rnorm(2*4*4)
    values <- rnorm(200)
    offset <- 11L
    iterNew <- AlongIterator(dim = c(2:3, 5L), iAlong = 3L)    
    iterOld <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- transferAlphaDelta0(state = state,
                                        values = values,
                                        offset = offset,
                                        iteratorNew = iterNew,
                                        iteratorOld = iterOld)
    ans.expected <- state
    ans.expected[1:6] <- values[29:34]
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferAlphaDelta0 give same answer", {
    transferAlphaDelta0 <- demest:::transferAlphaDelta0
    AlongIterator <- demest:::AlongIterator
    ## state has dim c(4,11); offset = 4L
    state <- numeric(length = 4 * 16)
    values <- rnorm(n = 100)
    offset <- 4L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.R <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = FALSE)
    ans.C <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## state has dim c(4,11); offset = 5L
    state <- numeric(length = 4 * 16)
    values <- rnorm(n = 100)
    offset <- 5L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.R <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = FALSE)
    ans.C <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## state has dim 2:4
    state <- rnorm(2*4*4)
    values <- rnorm(200)
    offset <- 11L
    iterNew <- AlongIterator(dim = c(2:3, 5L), iAlong = 3L)    
    iterOld <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.R <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = FALSE)
    ans.C <- transferAlphaDelta0(state = state,
                                 values = values,
                                 offset = offset,
                                 iteratorNew = iterNew,
                                 iteratorOld = iterOld,
                                 useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("transferLevelComponentWeightOldMix works", {
    transferLevelComponentWeightOldMix <- demest:::transferLevelComponentWeightOldMix
    values <- as.double(1:1000)
    ans.obtained <- transferLevelComponentWeightOldMix(values = values,
                                                       offset = 101L,
                                                       nAlongOld = 20L,
                                                       indexClassMax = 10L)
    ans.expected <- matrix(as.double(101:300),
                           nrow = 20,
                           ncol = 10)
    ans.expected <- ans.expected[20,]
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferLevelComponentWeightOldMix give same answer", {
    transferLevelComponentWeightOldMix <- demest:::transferLevelComponentWeightOldMix
    values <- as.double(1:1000)
    ans.R <- transferLevelComponentWeightOldMix(values = values,
                                                offset = 101L,
                                                nAlongOld = 20L,
                                                indexClassMax = 10L,
                                                useC = FALSE)
    ans.C <- transferLevelComponentWeightOldMix(values = values,
                                                offset = 101L,
                                                nAlongOld = 20L,
                                                indexClassMax = 10L,
                                                useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("transferParamBetas gives valid answer", {
    transferParamBetas <- demest:::transferParamBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 10L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:88)
        writeBin(data, con = con)
        close(con)
        model <- transferParamBetas(model,
                                    filename = filename,
                                    lengthIter = 44L,
                                    iteration = 2L,
                                    useC = FALSE)
        ans.obtained <- c(model@betas[[1L]], model@betas[[3]])
        ans.expected <- c(data[44 + 23], data[44 + (29:32)])
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of transferParamBetas give same answer", {
    transferParamBetas <- demest:::transferParamBetas
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 10L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:88)
        writeBin(data, con = con)
        close(con)
        ans.R <- transferParamBetas(model,
                                    filename = filename,
                                    lengthIter = 44L,
                                    iteration = 2L,
                                    useC = FALSE)
        ans.C <- transferParamBetas(model,
                                    filename = filename,
                                    lengthIter = 44L,
                                    iteration = 2L,
                                    useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("transferParamSigma gives valid answer", {
    transferParamSigma <- demest:::transferParamSigma
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ 1))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 5L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:100)
        writeBin(data, con = con)
        close(con)
        model <- transferParamSigma(model,
                                    filename = filename,
                                    lengthIter = 24L,
                                    iteration = 2L,
                                    useC = FALSE)
        ans.obtained <- model@sigma@.Data
        ans.expected <- data[48]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of transferParamSigma give same answer", {
    transferParamSigma <- demest:::transferParamSigma
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ 1))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 5L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:100)
        writeBin(data, con = con)
        close(con)
        ans.R <- transferParamSigma(model,
                                    filename = filename,
                                    lengthIter = 24L,
                                    iteration = 2L,
                                    useC = FALSE)
        ans.C <- transferParamSigma(model,
                                    filename = filename,
                                    lengthIter = 24L,
                                    iteration = 2L,
                                    useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("transferParamVarsigma gives valid answer", {
    transferParamVarsigma <- demest:::transferParamVarsigma
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ 1))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 5L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:100)
        writeBin(data, con = con)
        close(con)
        model <- transferParamVarsigma(model,
                                       filename = filename,
                                       lengthIter = 24L,
                                       iteration = 2L,
                                       useC = FALSE)
        ans.obtained <- model@varsigma@.Data
        ans.expected <- data[24 + 22]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of transferParamVarsigma give same answer", {
    transferParamVarsigma <- demest:::transferParamVarsigma
    initialModel <- demest:::initialModel
    initialModelPredict <- demest:::initialModelPredict
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Values(array(rnorm(20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(runif(20),
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ 1))
        model <- initialModel(spec, y = y, weights = weights)
        model <- initialModelPredict(model,
                                     along = 1L,
                                     n = 5L,
                                     labels = NULL,
                                     offsetModel = 1L,
                                     covariates = NULL,
                                     aggregate = NULL,
                                     lower = NULL,
                                     upper = NULL)
        filename <- tempfile()
        con <- file(filename, open = "wb")
        data <- as.double(1:100)
        writeBin(data, con = con)
        close(con)
        ans.R <- transferParamVarsigma(model,
                                    filename = filename,
                                    lengthIter = 24L,
                                    iteration = 2L,
                                    useC = FALSE)
        ans.C <- transferParamVarsigma(model,
                                    filename = filename,
                                    lengthIter = 24L,
                                    iteration = 2L,
                                    useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("transferSeason0 works", {
    transferSeason0 <- demest:::transferSeason0
    AlongIterator <- demest:::AlongIterator
    ## s has dim c(4,16); nSeason = 2; offset = 1
    s <- replicate(n = 64, numeric(2), simplify = FALSE)
    values <- rnorm(n = 300)
    offset <- 1L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.obtained <- transferSeason0(s = s,
                                    nSeason = 2L,
                                    values = values,
                                    offset = offset,
                                    iteratorNew = iterNew,
                                    iteratorOld = iterOld)
    ans.expected <- matrix(s, nr = 4)
    for (i in 1:4)
        ans.expected[[i,1]] <- array(values[1:88], dim = c(2, 4, 11))[,i,11]
    dim(ans.expected) <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## s has dim c(4,16); nSeason = 12; offset = 20L
    s <- replicate(n = 64, numeric(12), simplify = FALSE)
    values <- rnorm(n = 1000)
    offset <- 20L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.obtained <- transferSeason0(s = s,
                                    nSeason = 12L,
                                    values = values,
                                    offset = offset,
                                    iteratorNew = iterNew,
                                    iteratorOld = iterOld)
    ans.expected <- matrix(s, nr = 4)
    for (i in 1:4)
        ans.expected[[i,1]] <- array(values[20:(20+12*44-1)], dim = c(12,4,11))[,i,11]
    dim(ans.expected) <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## s has dim c(2, 3, 5); nSeason = 4; offset = 11
    s <- replicate(30, numeric(4), simplify = FALSE)
    values <- rnorm(1000)
    offset <- 11L
    iterNew <- AlongIterator(dim = c(2:3, 5L), iAlong = 3L)    
    iterOld <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- transferSeason0(s = s,
                                    nSeason = 4L,
                                    values = values,
                                    offset = offset,
                                    iteratorNew = iterNew,
                                    iteratorOld = iterOld)
    ans.expected <- array(s, dim = c(2, 3, 5))
    for (i in 1:2)
        for (j in 1:3)
            ans.expected[[i,j,1]] <- array(values[11:(11+24*4-1)], dim = c(4,2:4))[,i,j,4]
    dim(ans.expected) <- NULL
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of transferSeason0 give same answer", {
    transferSeason0 <- demest:::transferSeason0
    AlongIterator <- demest:::AlongIterator
    ## s has dim c(4,16); nSeason = 2; offset = 1
    s <- replicate(n = 64, numeric(2), simplify = FALSE)
    values <- rnorm(n = 300)
    offset <- 1L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.R <- transferSeason0(s = s,
                             nSeason = 2L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = FALSE)
    ans.C <- transferSeason0(s = s,
                             nSeason = 2L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## s has dim c(4,16); nSeason = 12; offset = 20L
    s <- replicate(n = 64, numeric(12), simplify = FALSE)
    values <- rnorm(n = 1000)
    offset <- 20L
    iterNew <- AlongIterator(dim = c(4L, 16L), iAlong = 2L)    
    iterOld <- AlongIterator(dim = c(4L, 11L), iAlong = 2L)
    ans.R <- transferSeason0(s = s,
                             nSeason = 12L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = FALSE)
    ans.C <- transferSeason0(s = s,
                             nSeason = 12L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## s has dim c(2, 3, 5); nSeason = 4; offset = 11
    s <- replicate(30, numeric(4), simplify = FALSE)
    values <- rnorm(1000)
    offset <- 11L
    iterNew <- AlongIterator(dim = c(2:3, 5L), iAlong = 3L)    
    iterOld <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.R <- transferSeason0(s = s,
                             nSeason = 4L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = FALSE)
    ans.C <- transferSeason0(s = s,
                             nSeason = 4L,
                             values = values,
                             offset = offset,
                             iteratorNew = iterNew,
                             iteratorOld = iterOld,
                             useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

