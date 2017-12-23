

context("Skeleton-generator")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

test_that("Skeleton creates valid object of class SkeletonOneValues from first", {
    Skeleton <- demest:::Skeleton
    x <- Skeleton(first = 3L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonOneValues",
                         first = 3L))
})

test_that("Skeleton creates valid object of class SkeletonManyValues from metadata, first", {
    Skeleton <- demest:::Skeleton
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10))))
    x <- Skeleton(metadata = metadata, first = 3L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonManyValues",
                         metadata = metadata,
                         first = 3L,
                         last = 4L))
    expect_identical(x@indicesStrucZero, integer())
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2, 2),
                                   dimnames = list(age = c("0-4", "5-9"),
                                                   sex = c("f", "m"))))
    x <- Skeleton(metadata = metadata, first = 3L, strucZeroArray = strucZeroArray, margin = 1L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonManyValues",
                         metadata = metadata,
                         first = 3L,
                         last = 4L,
                         indicesStrucZero = 2L))
})

test_that("Skeleton creates valid object of class SkeletonManyCounts from object, first", {
    Skeleton <- demest:::Skeleton
    object <- Counts(array(1:2, dim = 2L, dimnames = list(age = c("0-4", "5-9"))))
    x <- Skeleton(object = object, first = 3L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonManyCounts",
                         metadata = object@metadata,
                         first = 3L,
                         last = 4L))
})

test_that("Skeleton creates valid object of class SkeletonManyValues from object, first", {
    Skeleton <- demest:::Skeleton
    object <- Values(array(1:2, dim = 2L, dimnames = list(age = c("0-4", "5-9"))))
    x <- Skeleton(object = object, first = 3L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonManyValues",
                         metadata = object@metadata,
                         first = 3L,
                         last = 4L))
    expect_identical(x@indicesStrucZero, integer())
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2, 2),
                                   dimnames = list(age = c("0-4", "5-9"),
                                                   sex = c("f", "m"))))
    x <- Skeleton(object = object, first = 3L, strucZeroArray = strucZeroArray, margin = 1L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonManyValues",
                         metadata = object@metadata,
                         first = 3L,
                         last = 4L,
                         indicesStrucZero = 2L))
})

test_that("SkeletonBetaIntercept creates valid object of class SkeletonBetaIntercept", {
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    ans.obtained <- SkeletonBetaIntercept(first = 10L)
    ans.expected <- new("SkeletonBetaIntercept",
                        first = 10L,
                        last = 10L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonBetaTerm creates valid object of class SkeletonBetaTerm", {
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10))))
    ans.obtained <- SkeletonBetaTerm(first = 10L,
                                     metadata = metadata)
    ans.expected <- new("SkeletonBetaTerm",
                        first = 10L,
                        last = 11L,
                        metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, 15)),
                        new("Categories", dimvalues = letters[1:5])))
    ans.obtained <- SkeletonBetaTerm(first = 10L,
                                     metadata = metadata)
    ans.expected <- new("SkeletonBetaTerm",
                        first = 10L,
                        last = 24L,
                        metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, 15)),
                        new("Categories", dimvalues = letters[1:5])))
    ans.obtained <- SkeletonBetaTerm(first = 10L,
                                     metadata = metadata)
    ans.expected <- new("SkeletonBetaTerm",
                        first = 10L,
                        last = 24L,
                        metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, 15)),
                                     new("Categories", dimvalues = letters[1:5])))
    strucZeroArray <- Counts(array(c(0L, 1L, 1L),
                                   dim = c(3, 5, 2),
                                   dimnames = list(age = c("0-4", "5-9", "10-14"),
                                                   region = letters[1:5],
                                                   sex = c("f", "m"))))
    ans.obtained <- SkeletonBetaTerm(first = 10L,
                                     metadata = metadata,
                                     strucZeroArray = strucZeroArray,
                                     margin = 1:2)
    ans.expected <- new("SkeletonBetaTerm",
                        first = 10L,
                        last = 24L,
                        metadata = metadata,
                        indicesStrucZero = c(1L, 4L, 7L, 10L, 13L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMu creates valid object of class SkeletonMu", {
    SkeletonMu <- demest:::SkeletonMu
    ## several terms
    metadata <- Counts(array(1L,
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10+"),
                                 region = letters[1:4])))@metadata
    margins <- list(0L, 1L, 2L, 3L, 1:2)
    betas <- list(rnorm(1), rnorm(2), rnorm(3), rnorm(4), rnorm(6))
    first <- 10L
    ans.obtained <- SkeletonMu(betas = betas,
                               margins = margins,
                               first = first,
                               metadata = metadata)
    ans.expected <- new("SkeletonMu",
                        margins = margins,
                        metadata = metadata,
                        offsets = list(new("Offsets", c(10L, 10L)),
                            new("Offsets", c(11L, 12L)),
                            new("Offsets", c(13L, 15L)),
                            new("Offsets", c(16L, 19L)),
                            new("Offsets", c(20L, 25L))))
    expect_identical(ans.obtained, ans.expected)
    ## intercept only
    metadata <- Counts(array(1L,
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10+"),
                                 region = letters[1:4])))@metadata
    margins <- list(0L)
    betas <- list(rnorm(1))
    first <- 10L
    ans.obtained <- SkeletonMu(betas = betas,
                               margins = margins,
                               first = first,
                               metadata = metadata)
    ans.expected <- new("SkeletonMu",
                        margins = margins,
                        metadata = metadata,
                        offsets = list(new("Offsets", c(10L, 10L))))
    expect_identical(ans.obtained, ans.expected)
    ## strucZeroArray
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = 2:4,
                                   dimnames = list(sex = c("f", "m"),
                                                   age = c("0-4", "5-9", "10+"),
                                                   region = letters[1:4])))
    margins <- list(0L)
    betas <- list(rnorm(1))
    first <- 10L
    ans.obtained <- SkeletonMu(betas = betas,
                               margins = margins,
                               first = first,
                               metadata = metadata,
                               strucZeroArray = strucZeroArray)
    ans.expected <- new("SkeletonMu",
                        margins = margins,
                        metadata = metadata,
                        offsets = list(new("Offsets", c(10L, 10L))),
                        indicesStrucZero = seq(from = 2L, by = 2L, to = 24L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonAccept creates valid object of class SkeletonAccept from first", {
    SkeletonAccept <- demest:::SkeletonAccept
    x <- SkeletonAccept(first = 3L, nChain = 4L, nIteration = 200L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonAccept",
                         first = 3L,
                         iFirstInChain = c(1L, 51L, 101L, 151L)))
})

test_that("SkeletonAccept creates valid object of class SkeletonNAccept from nAttempt, first", {
    SkeletonAccept <- demest:::SkeletonAccept
    x <- SkeletonAccept(nAttempt = 20L, first = 3L, nChain = 1L, nIteration = 100L)
    expect_true(validObject(x))
    expect_identical(x,
                     new("SkeletonNAccept",
                         nAttempt = 20L,
                         first = 3L,
                         iFirstInChain = 1L))
})

## Missing data

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataNormalVarsigmaKnown", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaKnown")
    model@w <- rep(0.3, 6)
    model@varsigma <- new("Scale", 0.6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    ans.expected <- new("SkeletonMissingDataNormalVarsigmaKnown",
                        data = object,
                        varsigma = new("Scale", 0.6),
                        w = rep(0.3, 6),
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataNormalVarsigmaUnknown", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaUnknown")
    model@w <- rep(0.3, 6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object),
                                         sd = Skeleton(first = 7L)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    ans.expected <- new("SkeletonMissingDataNormalVarsigmaUnknown",
                        data = object,
                        w = rep(0.3, 6),
                        offsetsTheta = new("Offsets", c(1L, 6L)),
                        offsetsVarsigma = new("Offsets", c(7L, 7L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataPoissonNotUseExp", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("PoissonVaryingNotUseExp")
    outputModel <- list(likelihood = list(count = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    ans.expected <- new("SkeletonMissingDataPoissonNotUseExp",
                        data = object,
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataPoissonNotUseExpSubtotals", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:4, NA, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    subtotals <- CountsOne(10L, labels = "2", name = "age")
    object <- attachSubtotals(object, subtotals = subtotals)
    model <- new("PoissonVaryingNotUseExp")
    outputModel <- list(likelihood = list(count = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    ans.expected <- new("SkeletonMissingDataPoissonNotUseExpSubtotals",
                        data = object,
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataPoissonUseExp", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = exposure)
    ans.expected <- new("SkeletonMissingDataPoissonUseExp",
                        data = object,
                        exposure = exposure,
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataPoissonUseExpSubtotals", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:4, NA, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    subtotals <- CountsOne(10L, labels = "2", name = "age")
    object <- attachSubtotals(object, subtotals = subtotals)
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = exposure)
    ans.expected <- new("SkeletonMissingDataPoissonUseExpSubtotals",
                        data = object,
                        exposure = exposure,
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataBinomial", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    model <- new("BinomialVarying")
    outputModel <- list(likelihood = list(prob = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = exposure)
    ans.expected <- new("SkeletonMissingDataBinomial",
                        data = object,
                        exposure = exposure,
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingDataset creates valid object of class SkeletonMissingDatasetPoisson", {
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 20L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    ans.obtained <- SkeletonMissingDataset(object = object,
                                           model = model,
                                           outputModel = outputModel,
                                           skeletonComponent = skeletonComponent,
                                           transformComponent = transformComponent)
    ans.expected <- new("SkeletonMissingDatasetPoisson",
                        data = object,
                        offsetsTheta = new("Offsets", c(20L, 25L)),
                        offsetsComponent = new("Offsets", c(1L, 12L)),
                        transformComponent = transformComponent)
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingDataset creates valid object of class SkeletonMissingDatasetPoissonSubtotals", {
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:4, NA, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    subtotals <- CountsOne(10L, labels = "2", name = "age")
    object <- attachSubtotals(object, subtotals = subtotals)
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 20L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    ans.obtained <- SkeletonMissingDataset(object = object,
                                           model = model,
                                           outputModel = outputModel,
                                           skeletonComponent = skeletonComponent,
                                           transformComponent = transformComponent)
    ans.expected <- new("SkeletonMissingDatasetPoissonSubtotals",
                        data = object,
                        offsetsTheta = new("Offsets", c(20L, 25L)),
                        offsetsComponent = new("Offsets", c(1L, 12L)),
                        transformComponent = transformComponent)
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingDataset creates valid object of class SkeletonMissingDatasetBinomial", {
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("BinomialVarying")
    outputModel <- list(likelihood = list(prob = Skeleton(first = 20L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    ans.obtained <- SkeletonMissingDataset(object = object,
                                           model = model,
                                           outputModel = outputModel,
                                           skeletonComponent = skeletonComponent,
                                           transformComponent = transformComponent)
    ans.expected <- new("SkeletonMissingDatasetBinomial",
                        data = object,
                        offsetsTheta = new("Offsets", c(20L, 25L)),
                        offsetsComponent = new("Offsets", c(1L, 12L)),
                        transformComponent = transformComponent)
    expect_identical(ans.obtained, ans.expected)
})

test_that("SkeletonMissingDataset creates valid object of class SkeletonMissingDatasetPoissonBinomial", {
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonBinomialMixture", prob = 0.98)
    outputModel <- list(prob = 0.98)
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    ans.obtained <- SkeletonMissingDataset(object = object,
                                           model = model,
                                           outputModel = outputModel,
                                           skeletonComponent = skeletonComponent,
                                           transformComponent = transformComponent)
    ans.expected <- new("SkeletonMissingDatasetPoissonBinomial",
                        data = object,
                        prob = 0.98,
                        offsetsComponent = new("Offsets", c(1L, 12L)),
                        transformComponent = transformComponent)
    expect_identical(ans.obtained, ans.expected)
})
                        
    








