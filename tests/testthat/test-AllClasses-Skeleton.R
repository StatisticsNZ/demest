
context("AllClasses-Skeleton")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("can create valid object of class SkeletonOneCounts", {
    x <- new("SkeletonOneCounts",
             first = 2L)
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonOneCounts inherited from SkeletonFirst work", {
    x <- new("SkeletonOneCounts",
             first = 2L)
    ## 'first' has length 1
    x.wrong <- x
    x.wrong@first <- 1:2
    expect_error(validObject(x.wrong),
                 "'first' does not have length 1")
    ## 'first' is not missing
    x.wrong <- x
    x.wrong@first <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'first' is missing")
    ## 'first' is positive
    x.wrong <- x
    x.wrong@first <- 0L
    expect_error(validObject(x.wrong),
                 "'first' is less than 1")
})

test_that("can create valid object of class SkeletonOneValues", {
    x <- new("SkeletonOneValues",
             first = 2L)
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonManyCounts", {
    x <- new("SkeletonManyCounts",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonManyCounts inherited from SkeletonMany work", {
    x <- new("SkeletonManyCounts",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    ## 'last' has length 1
    x.wrong <- x
    x.wrong@last <- 5:6
    expect_error(validObject(x.wrong),
                 "'last' does not have length 1")
    ## 'last' is not missing
    x.wrong <- x
    x.wrong@last <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'last' is missing")
    ## can't test because test in SkeletonMetadata has precedence
    ## ## 'last' >= 'first'
    ## x.wrong <- x
    ## x.wrong@first <- 8L
    ## expect_error(validObject(x.wrong),
    ##              "'last' is less than 'first'")
})

test_that("validity tests for SkeletonManyCounts inherited from SkeletonMetadata work", {
    x <- new("SkeletonManyCounts",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    ## 'metadata' does not have iteration dimensions
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = "iteration",
                            dimtypes = "iteration",
                            DimScales = list(new("Iterations", dimvalues = 1:4)))
    expect_error(validObject(x.wrong),
                 "'metadata' has dimension with dimtype \"iteration\"")
    ## 'metadata' does not have quantile dimensions
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = "quantile",
                            dimtypes = "quantile",
                            DimScales = list(new("Quantiles", dimvalues = c(0, 0.1, 0.8, 1))))
    expect_error(validObject(x.wrong),
                 "'metadata' has dimension with dimtype \"quantile\"")
    ## dim(metadata) consistent with 'first', 'last'
    x.wrong <- x
    x.wrong@last <- 10L
    expect_error(validObject(x.wrong),
                 "'metadata', 'first', and 'last' inconsistent")
})

test_that("validity tests for SkeletonManyCounts inherited from SkeletonManyCounts work", {
    x <- new("SkeletonManyCounts",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    ## dim(metadata) consistent with 'first', 'last'
    x.wrong <- x
    x.wrong@last <- 10L
    expect_error(validObject(x.wrong),
                 "'metadata', 'first', and 'last' inconsistent")
})

test_that("can create valid object of class SkeletonManyValues", {
    x <- new("SkeletonManyValues",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonManyValues inherited from SkeletonManyValues work", {
    x <- new("SkeletonManyValues",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))))
    ## dim(metadata) consistent with 'first', 'last'
    x.wrong <- x
    x.wrong@last <- 10L
    expect_error(validObject(x.wrong),
                 "'metadata', 'first', and 'last' inconsistent")
})

test_that("validity tests for SkeletonManyValues inherited from SkeletonManyValuesStrucZero work", {
    x <- new("SkeletonManyValues",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
                            DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d")))),
             indicesStrucZero = 1L)
    expect_true(validObject(x))
    ## 'indicesStrucZero' has no missing values
    x.wrong <- x
    x.wrong@indicesStrucZero <- c(1L, NA)
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' has missing values")
    ## 'indicesStrucZero' has no duplicates
    x.wrong <- x
    x.wrong@indicesStrucZero <- c(1L, 1L)
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' has duplicates")
    ## 'indicesStrucZero' picks out indices of array
    ## specified by 'metadata'
    x.wrong <- x
    x.wrong@indicesStrucZero <- 100L
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' outside valid range")
})

test_that("can create valid object of class SkeletonBetaIntercept", {
    x <- new("SkeletonBetaIntercept",
             first = 2L,
             last = 2L)
})

test_that("validity tests for SkeletonBetaIntercept inherited from SkeletonBetaIntercept work", {
    x <- new("SkeletonBetaIntercept",
             first = 2L,
             last = 2L)
    ## 'last' has length 1
    x.wrong <- x
    x.wrong@last <- 5:6
    expect_error(validObject(x.wrong),
                 "'last' does not have length 1")
    ## 'last' is not missing
    x.wrong <- x
    x.wrong@last <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'last' is missing")
    ## 'last' >= 'first'
    x.wrong <- x
    x.wrong@first <- 8L
    expect_error(validObject(x.wrong),
                 "'last' does not equal 'first'")
})

test_that("can create valid object of class SkeletonBetaTerm", {
    x <- new("SkeletonBetaTerm",
             first = 2L,
             last = 5L,
             metadata = new("MetaData", nms = "region", dimtypes = "state",
                 DimScales = list(new("Categories",
                     dimvalues = c("a", "b", "c", "d")))))
})

test_that("can create valid object of class SkeletonMu", {
    ## dim = 4
    x <- new("SkeletonMu",
             metadata = new("MetaData", nms = "region", dimtypes = "state",
                 DimScales = list(new("Categories",
                     dimvalues = c("a", "b", "c", "d")))),
             margins = list(0L, 1L),
             offsets = list(new("Offsets", c(29L, 29L)),
                 new("Offsets", c(30L, 33L))))
    expect_true(validObject(x))
    ## dim = c(3, 5, 2)
    x <- new("SkeletonMu",
             metadata = new("MetaData",
                 nms = c("region", "age", "sex"),
                 dimtypes = c("state", "age", "state"),
                 DimScales = list(new("Categories",
                     dimvalues = c("a", "b", "c", "d")),
                     new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("f", "m")))),
             margins = list(0L, 1L, 2L, 3L, 1:2, 2:3),
             offsets = list(new("Offsets", c(30L, 30L)),
                 new("Offsets", c(31L, 34L)),
                 new("Offsets", c(35L, 39L)),
                 new("Offsets", c(40L, 41L)),
                 new("Offsets", c(42, 61L)),
                 new("Offsets", c(62L, 71L))))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMu inherited from SkeletonMu work", {
    x <- new("SkeletonMu",
             metadata = new("MetaData",
                 nms = c("region", "age", "sex"),
                 dimtypes = c("state", "age", "state"),
                 DimScales = list(new("Categories",
                     dimvalues = c("a", "b", "c", "d")),
                     new("Intervals", dimvalues = 0:5),
                     new("Categories", dimvalues = c("f", "m")))),
             margins = list(0L, 1L, 2L, 3L, 1:2, 2:3),
             offsets = list(new("Offsets", c(30L, 30L)),
                 new("Offsets", c(31L, 34L)),
                 new("Offsets", c(35L, 39L)),
                 new("Offsets", c(40L, 41L)),
                 new("Offsets", c(42, 61L)),
                 new("Offsets", c(62L, 71L))))
    expect_true(validObject(x))
    ## all elements of 'offsets' have class "Offsets"
    x.wrong <- x
    x.wrong@offsets[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'offsets' has elements not of class \"Offsets\"")
    ## 'offsets' and 'margins' have same length
    x.wrong <- x
    x.wrong@offsets <- x.wrong@offsets[1:5]
    expect_error(validObject(x.wrong),
                 "'margins' and 'offsets' have different lengths")
})

test_that("can create valid object of class SkeletonCovariates", {
    x <- new("SkeletonCovariates",
             first = 3L,
             last = 5L,
             metadata = new("MetaData", nms = "covariate", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("income", "education")))))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonCovariates inherited from SkeletonCovariates work", {
    x <- new("SkeletonCovariates",
             first = 3L,
             last = 5L,
             metadata = new("MetaData", nms = "covariate", dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("income", "education")))))
    ## 'metadata' has only one dimension
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = c("covariate", "wrong"),
                            dimtypes = c("state", "state"),
                            DimScales = list(new("Categories", dimvalues = c("income", "education")),
                            new("Categories", dimvalues = "wrong")))
    expect_error(validObject(x.wrong),
                 "'metadata' has more than one dimension")
    ## dimension has dimtype "state"
    x.wrong <- x
    x.wrong@metadata <- new("MetaData",
                            nms = "age",
                            dimtypes = "age",
                            DimScales = list(new("Intervals", dimvalues = 3:5)))
    expect_error(validObject(x.wrong),
                 "dimension does not have dimtype \"state\"")
    ## dim(metadata) consistent with 'first', 'last', allowing for fact
    ## that not using intercept.
    x.wrong <- x
    x.wrong@first <- 1L
    expect_error(validObject(x.wrong),
                 "'metadata', 'first', and 'last' inconsistent")
})

test_that("can create valid object of class SkeletonStateDLM", {
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 50L,
             iAlong = 1L,
             metadata = new("MetaData",
                            nms = "time",
                            dimtypes = "time",
                            DimScales = list(new("Points", dimvalues = 1:10))),
             metadataIncl0 = new("MetaData",
                 nms = "time",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:11)))),
             indices0 = 1L,
             indicesShow = 2:11,
             indicesStrucZero = integer())
    expect_true(validObject(x))
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 61L,
             iAlong = 2L,
             metadata = new("MetaData",
                            nms = c("sex", "time"),
                            dimtypes = c("state", "time"),
                            DimScales = list(new("Categories", dimvalues = c("f", "m")),
                                             new("Points", dimvalues = 1:10))),
             metadata0 = new("MetaData",
                 nms = "sex",
                 dimtypes = "sex",
                 DimScales = list(new("Sexes", dimvalues = c("f", "m")))),
             metadataIncl0 = new("MetaData",
                 nms = c("sex", "time"),
                 dimtypes = c("sex", "state"),
                 DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                                  new("Categories", dimvalues = as.character(1:11)))),
             indices0 = c(1L, 12L),
             indicesShow = c(2:11, 13:22),
             indicesStrucZero = seq.int(from = 2L, to = 20L, by = 2L))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonStateDLM inherited from SkeletonMetadataIncl0 work", {
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 50L,
             iAlong = 1L,
             metadata = new("MetaData",
                 nms = "time",
                 dimtypes = "time",
                 DimScales = list(new("Points", dimvalues = 1:10))),
             metadataIncl0 = new("MetaData",
                 nms = "time",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:11)))),
             indices0 = 1L,
             indicesShow = 2:11)
    expect_true(validObject(x))
    ## 'indices0' has no missing values
    x.wrong <- x
    x.wrong@indices0[1] <- NA
    expect_error(validObject(x.wrong),
                 "'indices0' has missing values")
    ## 'indices0' has no duplicates
    x.wrong <- x
    x.wrong@indices0[2] <- x.wrong@indices0[1]
    expect_error(validObject(x.wrong),
                 "'indices0' has duplicates")
    ## 'indices0' within valid range
    x.wrong <- x
    x.wrong@indices0[1] <- -1L
    expect_error(validObject(x.wrong),
                 "'indices0' has elements outside valid range")
})

test_that("validity tests for SkeletonStateDLM inherited from SkeletonIndicesShow work", {
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 50L,
             iAlong = 1L,
             metadata = new("MetaData",
                 nms = "time",
                 dimtypes = "time",
                 DimScales = list(new("Points", dimvalues = 1:10))),
             metadataIncl0 = new("MetaData",
                 nms = "time",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:11)))),
             indices0 = 1L,
             indicesShow = 2:11)
    expect_true(validObject(x))
    ## 'indicesShow' has no missing values
    x.wrong <- x
    x.wrong@indicesShow[1] <- NA
    expect_error(validObject(x.wrong),
                 "'indicesShow' has missing values")
    ## 'indicesShow' has no duplicates
    x.wrong <- x
    x.wrong@indicesShow[2] <- x.wrong@indicesShow[1]
    expect_error(validObject(x.wrong),
                 "'indicesShow' has duplicates")
    ## 'indicesShow' within valid range
    x.wrong <- x
    x.wrong@indicesShow[1] <- -1L
    expect_error(validObject(x.wrong),
                 "'indicesShow' has elements outside valid range")
})


test_that("can create valid object of class SkeletonStateDLM", {
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 50L,
             iAlong = 1L,
             metadata = new("MetaData",
                            nms = "time",
                            dimtypes = "time",
                            DimScales = list(new("Points", dimvalues = 1:10))),
             metadataIncl0 = new("MetaData",
                                 nms = "time",
                                 dimtypes = "state",
                                 DimScales = list(new("Categories", dimvalues = as.character(1:11)))),
             indices0 = 1L,
             indicesShow = 2:11,
             indicesStrucZero = integer())
    x <- new("SkeletonStateDLM",
             first = 40L,
             last = 61L,
             iAlong = 2L,
             metadata = new("MetaData",
                            nms = c("sex", "time"),
                            dimtypes = c("state", "time"),
                            DimScales = list(new("Categories", dimvalues = c("f", "m")),
                                             new("Points", dimvalues = 1:10))),
             metadata0 = new("MetaData",
                             nms = "sex",
                             dimtypes = "sex",
                             DimScales = list(new("Sexes", dimvalues = c("f", "m")))),
             metadataIncl0 = new("MetaData",
                                 nms = c("sex", "time"),
                                 dimtypes = c("sex", "state"),
                                 DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                                                  new("Categories", dimvalues = as.character(1:11)))),
             indices0 = c(1L, 12L),
             indicesShow = c(2:11, 13:22),
             indicesStrucZero = 2L)
    ## 'indicesStrucZero' has no missing values
    x.wrong <- x
    x.wrong@indicesStrucZero <- c(1L, NA)
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' has missing values")
   ## 'indicesStrucZero' has no duplicates
    x.wrong <- x
    x.wrong@indicesStrucZero <- c(1L, 1L)
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' has duplicates")
    ## 'indicesStrucZero' picks out indices of array
    ## specified by 'metadata0'
    x.wrong <- x
    x.wrong@indicesStrucZero <- 100L
    expect_error(validObject(x.wrong),
                 "'indicesStrucZero' outside valid range")
})

test_that("can create valid object of class SkeletonAccept", {
    x <- new("SkeletonAccept",
             first = 2L,
             iFirstInChain = c(1L, 11L))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonAccept inherited from SkeletonAccept work", {
    x <- new("SkeletonAccept",
             first = 2L,
             iFirstInChain = c(1L, 11L))
    ## 'iFirstInChain' has positive length
    x.wrong <- x
    x.wrong@iFirstInChain <- integer()
    expect_error(validObject(x.wrong),
                 "'iFirstInChain' has length 0")
    ## 'iFirstInChain' has no missing values
    x.wrong <- x
    x.wrong@iFirstInChain[1] <- NA
    expect_error(validObject(x.wrong),
                 "'iFirstInChain' has missing values")
    ## 'iFirstInChain' has not non-positive values
    x.wrong <- x
    x.wrong@iFirstInChain[1] <- -1L
    expect_error(validObject(x.wrong),
                 "'iFirstInChain' has values less than 1")
    ## 'iFirstInChain' has no duplicates
    x.wrong <- x
    x.wrong@iFirstInChain[2] <- 1L
    expect_error(validObject(x.wrong),
                 "'iFirstInChain' has duplicates")
})

test_that("can create valid object of class SkeletonNAccept", {
    x <- new("SkeletonNAccept",
             first = 2L,
             iFirstInChain = c(1L, 11L),
             nAttempt = 4L)
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonNAccept inherited from SkeletonNAccept work", {
    x <- new("SkeletonNAccept",
             first = 2L,
             iFirstInChain = c(1L, 11L),
             nAttempt = 4L)
    ## 'nAttempt' has length 1
    x.wrong <- x
    x.wrong@nAttempt <- 1:2
    expect_error(validObject(x.wrong),
                 "'nAttempt' does not have length 1")
    ## 'nAttempt' is not missing
    x.wrong <- x
    x.wrong@nAttempt <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'nAttempt' is missing")
    ## 'nAttempt' is non-negative
    x.wrong <- x
    x.wrong@nAttempt <- -1L
    expect_error(validObject(x.wrong),
                 "'nAttempt' is negative")
})


## Missing values

test_that("can create valid object of class SkeletonMissingDataNormalVarsigmaKnown", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 6)
    x <- new("SkeletonMissingDataNormalVarsigmaKnown",
             data = data,
             w = w,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             varsigma = new("Scale", 0.5))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMissingDataNormalVarsigmaKnown inherited from SkeletonMissingData work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 6)
    x <- new("SkeletonMissingDataNormalVarsigmaKnown",
             data = data,
             w = w,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             varsigma = new("Scale", 0.5))
    ## 'data' has missing values
    x.wrong <- x
    x.wrong@data[6] <- 1L
    expect_error(validObject(x.wrong),
                 "'data' has no missing value")
})

test_that("validity tests for SkeletonMissingDataNormalVarsigmaKnown inherited from SkeletonOffsetsTheta work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 6)
    x <- new("SkeletonMissingDataNormalVarsigmaKnown",
             data = data,
             w = w,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             varsigma = new("Scale", 0.5))
    ## 'data' and 'offsetsTheta' consistent
    x.wrong <- x
    x.wrong@offsetsTheta <- new("Offsets", c(11L, 17L))
    expect_error(validObject(x.wrong),
                 "'data' and 'offsetsTheta' inconsistent")
})

test_that("validity tests for SkeletonMissingDataNormalVarsigmaKnown inherited from SkeletonW work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 7)
    expect_error(new("SkeletonMissingDataNormalVarsigmaKnown",
                     data = data,
                     w = w,
                     offsetsTheta = new("Offsets", c(11L, 16L)),
                     varsigma = new("Scale", 0.5)),
                 "'data' and 'w' have different lengths")
})

test_that("can create valid object of class SkeletonMissingDataNormalVarsigmaUnknown", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 6)
    x <- new("SkeletonMissingDataNormalVarsigmaUnknown",
             data = data,
             w = w,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             offsetsVarsigma = new("Offsets", c(18L, 18L)))
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMissingDataNormalVarsigmaUnknown inherited from SkeletonMissingDataNormalVarsigmaUnknown work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    w <- rep(0.3, 6)
    expect_error(new("SkeletonMissingDataNormalVarsigmaUnknown",
                     data = data,
                     w = w,
                     offsetsTheta = new("Offsets", c(11L, 16L)),
                     offsetsVarsigma = new("Offsets", c(18L, 19L))),
                 "'offsetsVarsigma' implies 'varsigma' does not have length 1")
})

test_that("can create valid object of class SkeletonMissingDataPoissonNotUseExp", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDataPoissonNotUseExp",
             data = data,
             offsetsTheta = new("Offsets", c(11L, 16L)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDataPoissonNotUseExpSubtotals", {
    data <- Counts(array(c(1:4, c(NA, NA)),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    subtotals <- CountsOne(values = 10L, labels = "2", name = "age")
    data <- attachSubtotals(data, subtotals = subtotals)
    x <- new("SkeletonMissingDataPoissonNotUseExpSubtotals",
             data = data,
             offsetsTheta = new("Offsets", c(11L, 16L)))
    expect_true(validObject(x))
})


test_that("validity tests for SkeletonMissingDataPoissonNotUseExpSubtotals inherited from SkeletonHasSubtotals work", {
    data <- Counts(array(c(1:4, c(NA, NA)),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    expect_error(new("SkeletonMissingDataPoissonNotUseExpSubtotals",
                     data = data,
                     offsetsTheta = new("Offsets", c(11L, 16L))),
                 "'data' does not have class \"HasSubtotals\"")
})

test_that("can create valid object of class SkeletonMissingDataPoissonUseExp", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    x <- new("SkeletonMissingDataPoissonUseExp",
             data = data,
             exposure = exposure,
             offsetsTheta = new("Offsets", c(11L, 16L)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDataPoissonUseExpSubtotals", {
    data <- Counts(array(c(1:4, c(NA, NA)),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    subtotals <- CountsOne(values = 10L, labels = "2", name = "age")
    data <- attachSubtotals(data, subtotals = subtotals)
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    x <- new("SkeletonMissingDataPoissonUseExpSubtotals",
             data = data,
             exposure = exposure,
             offsetsTheta = new("Offsets", c(11L, 16L)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDataCMPNotUseExp", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDataCMPNotUseExp",
             data = data,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             offsetsNu = new("Offsets", c(17L, 22L)))
    expect_true(validObject(x))
})

test_that("validity test for SkeletonMissingDataCMPNotUseExp inherited from SkeletonMissingDataCMPNotUseExp works", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    expect_error(new("SkeletonMissingDataCMPNotUseExp",
                     data = data,
                     offsetsTheta = new("Offsets", c(11L, 16L)),
                     offsetsNu = new("Offsets", c(17L, 23L))),
                 "'data' and 'offsetsNu' inconsistent")
})

test_that("can create valid object of class SkeletonMissingDataCMPUseExp", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    x <- new("SkeletonMissingDataCMPUseExp",
             data = data,
             exposure = exposure,
             offsetsTheta = new("Offsets", c(11L, 16L)),
             offsetsNu = new("Offsets", c(17L, 22L)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDataBinomial", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    x <- new("SkeletonMissingDataBinomial",
             data = data,
             exposure = exposure,
             offsetsTheta = new("Offsets", c(11L, 16L)))
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDatasetPoisson", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDatasetPoisson",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMissingDatasetPoisson inherited from SkeletonMissingDataset work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDatasetPoisson",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    ## 'offsetsComponent' consistent with 'transformComponent'
    x.wrong <- x
    x.wrong@offsetsComponent[2] <- 13L
    expect_error(validObject(x.wrong),
                 "'offsetsComponent' and 'transformComponent' inconsistent")
})

test_that("validity tests for SkeletonMissingDatasetPoisson inherited from SkeletonMissingDatasetPoisson work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDatasetPoisson",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    ## 'data' consistent with 'transformComponent'
    x.wrong <- x
    transform.wrong <- new("CollapseTransform",
                           indices = list(c(1L, 1L), 1:2, c(1:2, 0L)),
                           dims = c(0L, 1L, 2L),
                           dimBefore = c(2L, 2L, 3L),
                           dimAfter = c(2L, 2L))
    x.wrong@transformComponent <- transform.wrong
    expect_error(validObject(x.wrong),
                 "'data' and 'transformComponent' inconsistent")
})

test_that("can create valid object of class SkeletonMissingDatasetPoissonSubtotals", {
    data <- Counts(array(c(1:4, NA, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    subtotals <- CountsOne(values = 10L, labels = "2", name = "age")
    data <- attachSubtotals(data, subtotals = subtotals)
    x <- new("SkeletonMissingDatasetPoissonSubtotals",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDatasetBinomial", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDatasetBinomial",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMissingDatasetBinomial inherited from SkeletonMissingDatasetBinomial work", {
    data <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                             age = 0:2)))
    x <- new("SkeletonMissingDatasetBinomial",
             data = data,
             offsetsTheta = new("Offsets", c(21L, 26L)),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    ## 'data' consistent with 'transformComponent'
    x.wrong <- x
    transform.wrong <- new("CollapseTransform",
                           indices = list(c(1L, 1L), 1:2, c(1:2, 0L)),
                           dims = c(0L, 1L, 2L),
                           dimBefore = c(2L, 2L, 3L),
                           dimAfter = c(2L, 2L))
    x.wrong@transformComponent <- transform.wrong
    expect_error(validObject(x.wrong),
                 "'data' and 'transformComponent' inconsistent")
})

test_that("can create valid object of class SkeletonMissingDatasetPoissonBinomial", {
    x <- new("SkeletonMissingDatasetPoissonBinomial",
             data = Counts(array(c(1:5, NA),
                 dim = 2:3,
                 dimnames = list(sex = c("f", "m"),
                     age = 0:2))),
             prob = 0.98,
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDatasetRound3", {
    x <- new("SkeletonMissingDatasetRound3",
             data = Counts(array(c(1:5, NA),
                 dim = 2:3,
                 dimnames = list(sex = c("f", "m"),
                     age = 0:2))),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                 indices = list(c(1L, 1L), 1:2, 1:3),
                 dims = c(0L, 1L, 2L),
                 dimBefore = c(2L, 2L, 3L),
                 dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("can create valid object of class SkeletonMissingDatasetNormalFixedUseExp", {
    x <- new("SkeletonMissingDatasetNormalFixedUseExp",
             data = Counts(array(c(1:5, NA),
                                 dim = 2:3,
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2))),
             mean = new("ParameterVector", c(0, 1:5)),
             sd = new("ScaleVec", c(1, 1:5)),
             metadata = new("MetaData",
                            nms = c("sex", "age"),
                            dimtypes = c("sex", "age"),
                            DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                                             new("Intervals", dimvalues = 0:3))),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                                      indices = list(c(1L, 1L), 1:2, 1:3),
                                      dims = c(0L, 1L, 2L),
                                      dimBefore = c(2L, 2L, 3L),
                                      dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})

test_that("validity tests for SkeletonMissingDatasetNormalFixedUseExp inherited from SkeletonMeanSD work", {
    x <- new("SkeletonMissingDatasetNormalFixedUseExp",
             data = Counts(array(c(1:5, NA),
                                 dim = 2:3,
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2))),
             mean = new("ParameterVector", c(0, 1:5)),
             sd = new("ScaleVec", c(1, 1:5)),
             metadata = new("MetaData",
                            nms = c("sex", "age"),
                            dimtypes = c("sex", "age"),
                            DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                                             new("Intervals", dimvalues = 0:3))),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                                      indices = list(c(1L, 1L), 1:2, 1:3),
                                      dims = c(0L, 1L, 2L),
                                      dimBefore = c(2L, 2L, 3L),
                                      dimAfter = c(2L, 3L)))
    ## 'mean' and 'sd' have same length
    x.wrong <- x 
    x.wrong@mean@.Data <- x.wrong@mean@.Data[-1]
    expect_error(validObject(x.wrong),
                 "'mean' and 'sd' have different lengths")
    ## length of 'mean' consistent with 'metadata'
    x.wrong <- x 
    x.wrong@mean@.Data <- x.wrong@mean@.Data[-1]
    x.wrong@sd@.Data <- x.wrong@sd@.Data[-1]
    expect_error(validObject(x.wrong),
                 "'mean' and 'metadata' inconsistent")
    expect_true(validObject(x))
})


test_that("can create valid object of class SkeletonMissingDatasetLN2", {
    x <- new("SkeletonMissingDatasetLN2",
             offsetsAlphaLN2 = new("Offsets", c(13L, 14L)),
             offsetsVarsigmaLN2 = new("Offsets", c(15L, 15L)),
             transformLN2 = dembase:::makeCollapseTransformExtra(new("CollapseTransform",
                                                                     indices = list(c(1L, 2L), c(1L, 1L, 1L)),
                                                                     dims = c(1L, 0L),
                                                                     dimBefore = c(2L, 3L),
                                                                     dimAfter = 2L)),
             data = Counts(array(c(1:5, NA),
                                 dim = 2:3,
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2))),
             strucZeroArray = Counts(array(1L,
                                           dim = 2:3,
                                           dimnames = list(sex = c("f", "m"),
                                                           age = 0:2))),
             offsetsComponent = new("Offsets", c(1L, 12L)),
             transformComponent = new("CollapseTransform",
                                      indices = list(c(1L, 1L), 1:2, 1:3),
                                      dims = c(0L, 1L, 2L),
                                      dimBefore = c(2L, 2L, 3L),
                                      dimAfter = c(2L, 3L)))                 
    expect_true(validObject(x))
})


